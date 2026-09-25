# nirs4all for R (development)

License: AGPL-3.0-or-later. The local package is not yet published to
R-universe or submitted to CRAN.

This is the dedicated R product package. Its name will move from the current
`nirs4all-core/bindings/r` package only after this repository passes the native
DAG-ML parity gates. Do **not** publish both sources under the R package name
`nirs4all` simultaneously.

The first slice composes `n4m` numerical preprocessing and PLS with R learner
controllers on real numeric matrices. It is deliberately labelled **local**:
the high-level API does not yet execute DAG-ML CV/HPO/refit/replay contracts,
and no Python/R model conversion is promised.

```r
library(nirs4all)
X <- matrix(rnorm(160), 16, 10)
y <- 1 + X[, 2] - 0.5 * X[, 5]
pipeline <- nirs4all_pipeline(
  steps = list(nirs4all_snv(), nirs4all_savgol(5)),
  learner = nirs4all_pls(n_components = 2)
)
fit <- nirs4all_fit(pipeline, X, y)
predictions <- predict(fit, X)
nirs4all_save(fit, "model.rds")
```

`nirs4all_lm()` uses base R. `nirs4all_ranger()` and
`nirs4all_glmnet(lambda, alpha)` are optional random-forest and
regularized-regression controllers. A user-defined controller can be
provided with `nirs4all_controller(fit, predict, name)`; its state is R-only.
The `n4m` PLS model is saved as portable N4MM bytes inside the RDS bundle.
This makes the native model portable, not the surrounding R preprocessing or
custom-controller code.
`nirs4all_torch_mlp()` provides an optional CPU neural-network regressor
through the R `torch` runtime. Torch modules are saved with `torch`'s own
serializer inside the RDS bundle; they are R-specific and not ONNX exports.
Custom controllers may capture non-serializable R state; their bundles are
only reliable when the controller author has tested a fresh-process load.
When feature names exist, prediction requires their exact training order; when
both row names and target names exist, fitting requires exact sample alignment.
Unnamed data are treated positionally. The package test suite always compares
all four portable Python examples (SNV, Savitzky-Golay, Kennard-Stone, and a
PLS component sweep) against a vendored Python oracle. This tests the n4m
numerical path, not native DAG-ML orchestration or arbitrary n4m compositions.

Current missing product gates are documented in
[`dag-ml/docs/R_BINDING_PARITY_AND_INTEROP.md`](https://github.com/GBeurier/dag-ml/blob/main/docs/R_BINDING_PARITY_AND_INTEROP.md).
