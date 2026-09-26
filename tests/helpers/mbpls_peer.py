"""Independent Python Methods C-ABI oracle for the unscaled MB-PLS recipe."""

import json
import sys
from pathlib import Path

import numpy as np
import pls4all
from pls4all._types import Algorithm, Deflation, Solver


def main(request_path: str, response_path: str) -> None:
    request = json.loads(Path(request_path).read_text(encoding="utf-8"))
    x = np.asarray(request["train"], dtype=np.float64)
    y = np.asarray(request["y"], dtype=np.float64).reshape(-1, 1)
    held = np.asarray(request["validation"], dtype=np.float64)
    blocks = np.asarray(request["block_sizes"], dtype=np.int64)
    with pls4all.Config() as config:
        config.algorithm = Algorithm.PLS_REGRESSION
        config.solver = Solver.NIPALS
        config.deflation = Deflation.REGRESSION
        config.n_components = int(request["n_components"])
        config.center_x = True
        config.scale_x = False
        config.center_y = True
        config.scale_y = False
        result = pls4all.mb_pls_fit(pls4all.Context(), config, x, y, blocks)
    coefficients = np.asarray(result.matrix("coefficients"), dtype=np.float64)
    intercept = float(np.asarray(result.matrix("intercept")).reshape(-1)[0])
    predictions = (held @ coefficients).reshape(-1) + intercept
    Path(response_path).write_text(
        json.dumps({"predictions": predictions.tolist(), "intercept": intercept}),
        encoding="utf-8",
    )


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
