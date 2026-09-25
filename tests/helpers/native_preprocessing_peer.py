"""Independent n4m Python preprocessing oracle for R recipe parity tests."""

import json
import sys

import numpy as np
from n4m.transform.baseline import Detrend
from n4m.transform.scatter import AreaNormalization, EMSC, LSNV, MSC, RNV, SNV
from n4m.transform.smoothing import SavitzkyGolay


OPERATORS = {
    "n4m.LSNV": LSNV,
    "n4m.RNV": RNV,
    "n4m.AreaNormalization": AreaNormalization,
    "n4m.Detrend": Detrend,
    "n4m.MSC": MSC,
    "n4m.EMSC": EMSC,
    "n4m.SNV": SNV,
    "n4m.SavitzkyGolay": SavitzkyGolay,
}


def main() -> None:
    request_path, response_path = sys.argv[1:3]
    with open(request_path, encoding="utf-8") as stream:
        request = json.load(stream)
    training = np.asarray(request["train"], dtype=np.float64)
    validation = np.asarray(request["validation"], dtype=np.float64)
    if "branches" in request:
        training_branches = []
        validation_branches = []
        for steps in request["branches"].values():
            branch_training = training.copy()
            branch_validation = validation.copy()
            for step in steps:
                operator = OPERATORS[step["class"]](**step.get("params", {}))
                branch_training = operator.fit_transform(branch_training)
                branch_validation = operator.transform(branch_validation)
            training_branches.append(branch_training)
            validation_branches.append(branch_validation)
        training = np.concatenate(training_branches, axis=1)
        validation = np.concatenate(validation_branches, axis=1)
    else:
        for step in request["steps"]:
            operator = OPERATORS[step["class"]](**step.get("params", {}))
            training = operator.fit_transform(training)
            validation = operator.transform(validation)
    with open(response_path, "w", encoding="utf-8") as stream:
        json.dump({"train": training.tolist(),
                   "validation": validation.tolist()}, stream)


if __name__ == "__main__":
    main()
