"""Fit R-exported n4m.Selector recipes through Python's native parser."""

import json
import sys
from pathlib import Path

import numpy as np


def main(request_path: str, response_path: str) -> None:
    request = json.loads(Path(request_path).read_text(encoding="utf-8"))
    sys.path.insert(0, request["python_root"])
    from nirs4all.pipeline.config.pipeline_config import PipelineConfigs
    from nirs4all.pipeline.steps.parser import StepParser

    train = np.asarray(request["train"], dtype=np.float64)
    held = np.asarray(request["held"], dtype=np.float64)
    y = np.asarray(request["y"], dtype=np.float64)
    parser = StepParser()
    results = {}
    for name, recipe in request["recipes"].items():
        steps = PipelineConfigs(recipe).steps[0]
        if len(steps) != 2:
            raise ValueError("selector peer expects one selector and one PLS model")
        selector = parser.parse(steps[0]).operator
        model = parser.parse(steps[1]).operator
        selector.fit(train, y)
        projected_train = selector.transform(train)
        projected_held = selector.transform(held)
        model.fit(projected_train, y)
        results[name] = {
            "selected_indices": selector.selected_indices_.tolist(),
            "held_selected": projected_held.tolist(),
            "predictions": np.asarray(model.predict(projected_held),
                                      dtype=np.float64).reshape(-1).tolist(),
        }
    Path(response_path).write_text(json.dumps(results), encoding="utf-8")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
