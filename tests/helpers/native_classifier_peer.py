"""Independent Python n4m sparse PLS-DA decision oracle."""

from __future__ import annotations

import json
import sys
from pathlib import Path

import numpy as np
from n4m.transform.scatter import SNV
from pls4all.sklearn import SparsePLSDAClassifier


def main() -> None:
    request = json.loads(Path(sys.argv[1]).read_text(encoding="utf-8"))
    train = np.asarray(request["train"], dtype=np.float64)
    target = np.asarray(request["target"], dtype=np.int64)
    test = np.asarray(request["test"], dtype=np.float64)
    for operator in request.get("preprocessing", []):
        if operator != "n4m.SNV":
            raise ValueError("unsupported classifier oracle preprocessing")
        native = SNV()
        train = native.fit_transform(train)
        test = native.transform(test)
    model = SparsePLSDAClassifier(
        n_components=int(request["n_components"]),
        sparsity_lambda=float(request["sparsity_lambda"]),
    ).fit(train, target)
    scores = model.decision_function(test)
    result = {
        "classes": model.classes_.tolist(),
        "scores": scores.tolist(),
        "predictions": model.predict(test).tolist(),
    }
    Path(sys.argv[2]).write_text(json.dumps(result), encoding="utf-8")


if __name__ == "__main__":
    main()
