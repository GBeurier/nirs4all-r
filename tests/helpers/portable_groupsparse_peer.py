"""Execute the bounded R GroupSparsePLS recipe through the Python n4m peer."""

import json
import sys
from pathlib import Path

import numpy as np
from n4m.estimators.regression.sparse import GroupSparsePLS
from n4m.transform.scatter import SNV


def main(request_path: str, response_path: str) -> None:
    request = json.loads(Path(request_path).read_text(encoding="utf-8"))
    stages = request["recipe"]["pipeline"]
    train = np.asarray(request["train"], dtype=np.float64)
    held = np.asarray(request["held"], dtype=np.float64)
    for stage in stages[:-1]:
        if stage != {"class": "n4m.SNV"}:
            raise ValueError("unsupported preprocessing in GroupSparsePLS peer")
        transform = SNV()
        train = transform.fit_transform(train)
        held = transform.transform(held)
    model = stages[-1]["model"]
    if model["class"] != "n4m.GroupSparsePLS":
        raise ValueError("unexpected model class")
    params = model["params"]
    if set(params) != {"n_components", "group_assignment", "group_lambda"}:
        raise ValueError("GroupSparsePLS recipe must carry every wire parameter")
    groups = params["group_assignment"]
    if not isinstance(groups, list) or len(groups) != train.shape[1]:
        raise ValueError("group_assignment must follow the transformed feature axis")
    if any(type(group) is not int or group < 0 for group in groups):
        raise ValueError("group_assignment must contain nonnegative integer IDs")
    estimator = GroupSparsePLS(
        n_components=params["n_components"],
        group_assignment=groups,
        group_lambda=params["group_lambda"],
    )
    estimator.fit(train, np.asarray(request["y"], dtype=np.float64))
    predictions = np.asarray(estimator.predict(held), dtype=np.float64).reshape(-1)
    Path(response_path).write_text(
        json.dumps({"predictions": predictions.tolist()}), encoding="utf-8"
    )


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
