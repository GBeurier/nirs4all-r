"""Independent Python n4m SPA oracle for the R pipeline selection test."""

import json
import sys

import numpy as np
from n4m.feature_selection.wrapper import SPA
from pls4all.sklearn import PLSRegression


def main() -> None:
    with open(sys.argv[1], encoding="utf-8") as stream:
        request = json.load(stream)
    X = np.asarray(request["X"], dtype=np.float64)
    y = np.asarray(request["y"], dtype=np.float64)
    selector = SPA(request["top_k"], n_components=request["n_components"])
    selector.fit(X, y)
    validation = np.asarray(request["validation"], dtype=np.float64)
    train_selected = selector.transform(X)
    validation_selected = selector.transform(validation)
    model = PLSRegression(n_components=2, solver="simpls", scale_y=True)
    model.fit(train_selected, y)
    with open(sys.argv[2], "w", encoding="utf-8") as stream:
        json.dump({
            "selected_indices": selector.selected_indices_.tolist(),
            "train_selected": train_selected.tolist(),
            "validation_selected": validation_selected.tolist(),
            "predictions": np.asarray(model.predict(validation_selected)).reshape(-1).tolist(),
        }, stream)


if __name__ == "__main__":
    main()
