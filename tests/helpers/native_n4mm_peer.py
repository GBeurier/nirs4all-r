"""Fresh Python process consumer of R-trained native N4MM models."""

from __future__ import annotations

import json
import sys
import ctypes
from pathlib import Path

import numpy as np
from pls4all import Context, Model, inspect_n4mm, export_linear_predictor_n4mm
from n4m._ffi import lib
from n4m._types import MatrixView


def checked(status: int, name: str) -> None:
    if status != 0:
        raise RuntimeError(f"{name} failed with native status {status}")


def train_native(x: np.ndarray, y: np.ndarray, embedded: bool) -> bytes:
    """Fit through n4m's C ABI; all numerical work remains native."""
    x = np.ascontiguousarray(x, dtype=np.float64)
    y = np.ascontiguousarray(y.reshape(-1, 1), dtype=np.float64)
    context = ctypes.c_void_p()
    config = ctypes.c_void_p()
    pipeline = ctypes.c_void_p()
    model = ctypes.c_void_p()
    try:
        checked(lib.n4m_context_create(ctypes.byref(context)), "context_create")
        checked(lib.n4m_config_create(ctypes.byref(config)), "config_create")
        checked(lib.n4m_config_set_algorithm(config, 0), "set_algorithm")
        checked(lib.n4m_config_set_solver(config, 1), "set_solver")
        checked(lib.n4m_config_set_n_components(config, 2), "set_n_components")
        if embedded:
            checked(lib.n4m_pipeline_create(ctypes.byref(pipeline)), "pipeline_create")
            checked(lib.n4m_pipeline_add_operator(pipeline, 4, None, 0), "add_snv")
            sg_params = (ctypes.c_double * 2)(5.0, 2.0)
            checked(lib.n4m_pipeline_add_operator(pipeline, 8, sg_params, 2), "add_savgol")
            checked(lib.n4m_config_set_pipeline(config, pipeline), "set_pipeline")
        x_view = MatrixView()
        y_view = MatrixView()
        checked(lib.n4m_matrix_view_init_rowmajor(
            ctypes.byref(x_view), x.ctypes.data, x.shape[0], x.shape[1], 1),
            "x_view")
        checked(lib.n4m_matrix_view_init_rowmajor(
            ctypes.byref(y_view), y.ctypes.data, y.shape[0], 1, 1),
            "y_view")
        checked(lib.n4m_model_fit(context, config, ctypes.byref(x_view),
                                  ctypes.byref(y_view), ctypes.byref(model)), "model_fit")
        size = ctypes.c_size_t()
        checked(lib.n4m_model_export_size(model, ctypes.byref(size)), "export_size")
        buffer = (ctypes.c_uint8 * size.value)()
        written = ctypes.c_size_t()
        checked(lib.n4m_model_export_to_buffer(model, buffer, size.value,
                                                ctypes.byref(written)), "export")
        return bytes(buffer[:written.value])
    finally:
        if model.value:
            lib.n4m_model_destroy(model)
        if pipeline.value:
            lib.n4m_pipeline_destroy(pipeline)
        if config.value:
            lib.n4m_config_destroy(config)
        if context.value:
            lib.n4m_context_destroy(context)


def main() -> None:
    mode = sys.argv[1]
    model_path, request_path, result_path = map(Path, sys.argv[2:5])
    request = json.loads(request_path.read_text(encoding="utf-8"))
    x = np.asarray(request["X"], dtype=np.float64)
    if x.ndim != 2:
        raise ValueError("X must be a matrix")
    predict_x = np.asarray(request.get("predict_X", request["X"]), dtype=np.float64)
    if predict_x.ndim != 2 or predict_x.shape[1] != x.shape[1]:
        raise ValueError("predict_X must be a matrix with the training feature width")
    if mode == "fit_affine":
        payload = export_linear_predictor_n4mm(
            request["coefficients"], request["intercept"],
            source_training_samples=x.shape[0],
        )
        model_path.write_bytes(payload)
    elif mode in ("fit", "fit_plain"):
        payload = train_native(x, np.asarray(request["y"], dtype=np.float64),
                               embedded=mode == "fit")
        model_path.write_bytes(payload)
    elif mode in ("predict", "predict_plain", "predict_affine"):
        payload = model_path.read_bytes()
    else:
        raise ValueError("unsupported native peer mode")
    info = inspect_n4mm(payload)
    if mode.endswith("_affine"):
        if info.pipeline is not None or info.algorithm != 11:
            raise ValueError("expected native affine predictor")
    elif mode.endswith("_plain"):
        if info.pipeline is not None:
            raise ValueError("expected plain native PLS model")
    elif info.pipeline is None or info.pipeline.semantic_profile != 1:
        raise ValueError("expected embedded native SNV-Savitzky-Golay profile")
    with Context() as context:
        model = Model.from_bytes(context, payload)
        try:
            predictions = model.predict(context, predict_x).reshape(-1).tolist()
        finally:
            model.close()
    result_path.write_text(json.dumps({"predictions": predictions}), encoding="utf-8")


if __name__ == "__main__":
    main()
