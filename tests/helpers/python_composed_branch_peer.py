"""Fit the same portable composed branch recipe with Python nirs4all/n4m."""

import json
import sys
from pathlib import Path

import numpy as np


def main(request_path: str, response_path: str) -> None:
    request = json.loads(Path(request_path).read_text(encoding="utf-8"))
    sys.path.insert(0, request["python_root"])
    from nirs4all.pipeline.portable_n4m_trained import PortableN4MTrainedPipeline

    train = np.asarray(request["train"], dtype=np.float64)
    validation = np.asarray(request["validation"], dtype=np.float64)
    y = np.asarray(request["y"], dtype=np.float64)
    with PortableN4MTrainedPipeline.fit_recipe(request["recipe"], train, y) as fitted:
        predictions = np.asarray(fitted.predict(validation), dtype=np.float64)
    Path(response_path).write_text(
        json.dumps({"predictions": predictions.tolist()}), encoding="utf-8"
    )


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
