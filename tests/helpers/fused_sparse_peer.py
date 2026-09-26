"""Independent Python n4m C-ABI oracle for FusedSparsePLS regression."""

import json
import sys
from pathlib import Path

import numpy as np
import pls4all
from pls4all._types import Algorithm, Deflation, Solver


def main(request_path: str, response_path: str) -> None:
    request = json.loads(Path(request_path).read_text(encoding="utf-8"))
    train = np.asarray(request["train"], dtype=np.float64)
    held = np.asarray(request["held"], dtype=np.float64)
    y = np.asarray(request["y"], dtype=np.float64).reshape(-1, 1)
    with pls4all.Config() as config:
        config.algorithm = Algorithm.PLS_REGRESSION
        config.solver = Solver.SIMPLS
        config.deflation = Deflation.REGRESSION
        config.n_components = int(request["n_components"])
        config.center_x = True
        config.scale_x = False
        config.center_y = True
        config.scale_y = False
        result = pls4all.fused_sparse_pls_fit(
            pls4all.Context(), config, train, y,
            float(request["l1_lambda"]), float(request["fusion_lambda"]),
        )
    coefficients = np.asarray(result.matrix("coefficients"), dtype=np.float64)
    x_mean = np.asarray(result.matrix("x_mean"), dtype=np.float64).reshape(-1)
    y_mean = np.asarray(result.matrix("y_mean"), dtype=np.float64).reshape(-1)
    predictions = ((held - x_mean) @ coefficients + y_mean).reshape(-1)
    Path(response_path).write_text(json.dumps({
        "predictions": predictions.tolist(),
        "coefficients": coefficients.reshape(-1).tolist(),
    }), encoding="utf-8")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
