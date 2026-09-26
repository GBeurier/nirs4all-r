"""Independent Python/native replay of an R-generated N4MP+N4MM v6 fixture."""

from __future__ import annotations

import base64
import ctypes
import hashlib
import json
import sys
from pathlib import Path

import numpy as np

from n4m._errors import check
from n4m._ffi import lib
from n4m._matrix import numpy_to_view
from n4m.compose.preprocessing import NativePreprocessingPipeline


def payload(value: dict[str, str], kind: str, encoding: str) -> bytes:
    if set(value) != {"kind", "encoding", "sha256", "payload"}:
        raise ValueError("invalid payload fields")
    if value["kind"] != kind or value["encoding"] != encoding:
        raise ValueError("invalid payload kind")
    data = base64.b64decode(value["payload"], validate=True)
    if hashlib.sha256(data).hexdigest() != value["sha256"]:
        raise ValueError("payload digest mismatch")
    return data


def predict_model(bundle: bytes, X: np.ndarray) -> np.ndarray:
    context = ctypes.c_void_p()
    model = ctypes.c_void_p()
    result = np.empty((X.shape[0], 1), dtype=np.float64)
    try:
        check(lib.n4m_context_create(ctypes.byref(context)), "context_create")
        buffer = ctypes.create_string_buffer(bundle)
        check(lib.n4m_model_import_from_buffer(
            context, buffer, len(bundle), ctypes.byref(model)), "model_import")
        x_view = numpy_to_view(np.ascontiguousarray(X))
        y_view = numpy_to_view(result)
        check(lib.n4m_model_predict(context, model, ctypes.byref(x_view),
                                    ctypes.byref(y_view)), "model_predict")
    finally:
        if model.value:
            lib.n4m_model_destroy(model)
        if context.value:
            lib.n4m_context_destroy(context)
    return result.ravel()


def main(envelope_path: str, oracle_path: str) -> None:
    document = json.loads(Path(envelope_path).read_text(encoding="utf-8"))
    oracle = json.loads(Path(oracle_path).read_text(encoding="utf-8"))
    if document["schema"] != "nirs4all.n4m.trained_pipeline.v6":
        raise ValueError("unexpected trained schema")
    if hashlib.sha256(document["manifest_json"].encode()).hexdigest() != document["manifest_sha256"]:
        raise ValueError("manifest digest mismatch")
    manifest = json.loads(document["manifest_json"])
    if manifest["preprocessing_owner"] != "native_n4mp":
        raise ValueError("unexpected preprocessing owner")
    if manifest["input_n_features"] != len(oracle["feature_names"]):
        raise ValueError("feature width mismatch")
    if manifest["feature_names"] != oracle["feature_names"]:
        raise ValueError("feature ordering mismatch")
    pre = payload(document["preprocessing"], "n4m_preprocessing", "base64-n4mp")
    model = payload(document["model"], "n4m_model", "base64-n4mm")
    expected = [("snv", ()), ("msc", ()),
                ("savgol_derivative", (7, 2, 1, 1))]
    with NativePreprocessingPipeline.from_bytes(
        pre, expected_operators=expected) as pipeline:
        if pipeline.n_features_in_ != manifest["input_n_features"]:
            raise ValueError("N4MP input width mismatch")
        held = np.asarray(oracle["heldout"], dtype=np.float64)
        actual = predict_model(model, pipeline.transform(held))
    np.testing.assert_allclose(actual, oracle["predictions"], rtol=0, atol=1e-10)
    print(json.dumps({"predictions": actual.tolist()}))


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
