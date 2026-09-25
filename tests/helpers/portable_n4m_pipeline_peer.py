"""Independent Python nirs4all/n4m execution of an R-exported recipe."""

import json
import sys
from pathlib import Path

import numpy as np


def main(request_path: str, result_path: str) -> None:
    from nirs4all.pipeline.config.pipeline_config import PipelineConfigs
    from nirs4all.pipeline.steps.parser import StepParser

    request = json.loads(Path(request_path).read_text(encoding="utf-8"))
    recipe = PipelineConfigs(request["recipe"]).steps[0]
    parser = StepParser()
    train = np.asarray(request["train"], dtype=np.float64)
    validation = np.asarray(request["validation"], dtype=np.float64)
    targets = np.asarray(request["y"], dtype=np.float64)
    for step in recipe[:-1]:
        operator = parser.parse(step).operator
        operator.fit(train)
        train = operator.transform(train)
        validation = operator.transform(validation)
    model = parser.parse(recipe[-1]).operator
    model.fit(train, targets)
    predictions = np.asarray(model.predict(validation), dtype=np.float64).reshape(-1)
    Path(result_path).write_text(
        json.dumps({"predictions": predictions.tolist()}), encoding="utf-8"
    )


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
