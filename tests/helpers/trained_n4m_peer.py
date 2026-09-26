"""Independent Python consumer for an R-trained n4m pipeline envelope."""

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
    if request.get("feature_names") is not None:
        import pandas as pd

        train = pd.DataFrame(train, columns=request["feature_names"])
        validation = pd.DataFrame(validation, columns=request["feature_names"])
    with PortableN4MTrainedPipeline.from_json(request["bundle"]) as imported:
        predictions = imported.predict(validation)
        rejected_reordered = None
        rejected_unnamed = None
        if request.get("feature_names") is not None:
            try:
                imported.predict(validation.iloc[:, ::-1])
                rejected_reordered = False
            except ValueError:
                rejected_reordered = True
            try:
                imported.predict(validation.to_numpy())
                rejected_unnamed = False
            except ValueError:
                rejected_unnamed = True
        retrained = imported.retrain(train, y)
        retrained_predictions = retrained.predict(validation)
        with PortableN4MTrainedPipeline.fit_recipe(imported.recipe, train, y) as python_fitted:
            python_predictions = python_fitted.predict(validation)
            python_fitted.to_json(request["python_bundle"])
    Path(response_path).write_text(json.dumps({
        "predictions": predictions.tolist(),
        "retrained_predictions": retrained_predictions.tolist(),
        "python_predictions": python_predictions.tolist(),
        "rejected_reordered": rejected_reordered,
        "rejected_unnamed": rejected_unnamed,
    }), encoding="utf-8")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
