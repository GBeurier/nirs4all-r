"""Independent Python peer for R/Python trained sparse PLS-DA transfer."""

from __future__ import annotations

import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd


def main(request_path: str, response_path: str) -> None:
    request = json.loads(Path(request_path).read_text(encoding="utf-8"))
    sys.path.insert(0, request["python_root"])
    from nirs4all.pipeline.portable_n4m_trained import PortableN4MTrainedPipeline

    train = pd.DataFrame(np.asarray(request["train"], dtype=np.float64),
                         columns=request["feature_names"])
    validation = pd.DataFrame(np.asarray(request["validation"], dtype=np.float64),
                              columns=request["feature_names"])
    labels = np.asarray(request["labels"], dtype=str)
    with PortableN4MTrainedPipeline.from_json(request["r_bundle"]) as from_r:
        result = {
            "classes": from_r.classes,
            "r_predictions": from_r.predict(validation).tolist(),
            "r_scores": from_r.predict_scores(validation).tolist(),
            "r_probabilities": from_r.predict_proba(validation).tolist(),
            "retrained_predictions": from_r.retrain(train, labels).predict(validation).tolist(),
        }
    with PortableN4MTrainedPipeline.fit_recipe(request["recipe"], train, labels) as from_python:
        Path(request["python_bundle"]).write_text(from_python.to_json(), encoding="utf-8")
        result["python_predictions"] = from_python.predict(validation).tolist()
        result["python_scores"] = from_python.predict_scores(validation).tolist()
    Path(response_path).write_text(json.dumps(result, allow_nan=False), encoding="utf-8")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
