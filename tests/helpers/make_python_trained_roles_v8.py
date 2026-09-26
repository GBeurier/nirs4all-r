"""Write inst/extdata/python_trained_roles_v8.json: Python-trained v8 envelopes (L2 portability).

Run with the nirs4all Python package and n4m roles importable:
    python tests/helpers/make_python_trained_roles_v8.py inst/extdata/python_trained_roles_v8.json
"""
import json, sys
import numpy as np
from nirs4all.pipeline.portable_n4m_roles import PortableN4MRolePipeline
rng = np.random.default_rng(7)
scores = rng.normal(size=(60, 2))
X = scores @ rng.normal(size=(2, 24)) + 1.0 + 0.02 * rng.normal(size=(60, 24))
y = scores[:, 0] - 0.4 * scores[:, 1]
labels = np.where(y > np.median(y), "high", "low")
cases = {
    "regression": ({"pipeline": [
        {"class": "n4m:filters.y_outlier", "params": {"threshold": 2.0}},
        "n4m:preprocessing.scatter.snv",
        {"class": "n4m:filters.variance", "params": {"top_k": 16}},
        {"class": "n4m:models.pls.cppls", "params": {"n_components": 3}}]}, y),
    "classification": ({"pipeline": [
        "n4m:preprocessing.scatter.snv",
        {"class": "n4m:models.classification.pls_qda", "params": {"n_components": 2}}]}, labels),
}
out = {"x_train": X[:45].tolist(), "x_test": X[45:].tolist()}
for name, (recipe, target) in cases.items():
    fitted = PortableN4MRolePipeline.fit_recipe(recipe, X[:45], target[:45])
    out[name] = {"envelope": json.loads(fitted.to_json()), "y_train": target[:45].tolist(),
                 "predict": fitted.predict(X[45:]).tolist()}
json.dump(out, open(sys.argv[1], "w"), indent=1)
