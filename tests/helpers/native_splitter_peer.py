"""Independent Python n4m splitter oracle for the fixed R sample fixture."""

import json

import numpy as np

from n4m.model_selection.splitters import (
    BinnedStratifiedGroupKFold,
    DataTwinning,
    KBinsStratified,
    KMeans,
    KennardStone,
    SPXY,
    SPXYFold,
    SPXYGroupFold,
    SystematicCircular,
)


samples = np.arange(1, 31)
X = np.column_stack((samples, samples * 7 % 13)).astype(float)
Y = (samples % 11).astype(float).reshape(-1, 1)
groups = np.repeat(np.arange(1, 16), 2)

cases = {
    "kennard_stone": KennardStone(test_size=0.25).split(X),
    "spxy": SPXY(test_size=0.25).split(X, Y),
    "spxy_fold": SPXYFold(n_splits=3, y_metric=1).split_fold(X, Y, 0),
    "spxy_group_fold": SPXYGroupFold(n_splits=3, y_metric=1,
                                      aggregation=0).split_fold(X, Y, groups, 0),
    "kmeans": KMeans(test_size=0.25, seed=42, max_iter=100).split(X),
    "kbins_stratified": KBinsStratified(test_size=0.25, seed=42, n_bins=2,
                                        strategy=0).split(Y),
    "binned_strat_group_fold": BinnedStratifiedGroupKFold(
        n_splits=3, n_bins=2, strategy=0, shuffle=True,
        seed=42).split_fold(Y, groups, 0),
    "systematic_circular": SystematicCircular(test_size=0.25,
                                                seed=42).split(Y),
    "data_twinning": DataTwinning(test_size=0.25, seed=42).split(X),
}
print(json.dumps({key: {"train": train.tolist(), "test": test.tolist()}
                  for key, (train, test) in cases.items()}))
