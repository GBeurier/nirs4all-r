# nirs4all for R (development)

License: AGPL-3.0-or-later. The local package is not yet published to
R-universe or submitted to CRAN.

This is the dedicated R product package. Its name will move from the current
`nirs4all-core/bindings/r` package only after this repository passes the native
DAG-ML parity gates. Do **not** publish both sources under the R package name
`nirs4all` simultaneously.

The package composes `n4m` numerical preprocessing and PLS with R learner
controllers on real numeric matrices. Local fit/predict and a first native
DAG-ML path are separate surfaces. The native path covers single-model
CV → OOF → refit → replay; branches, HPO, external-test prediction and
Python/R host-model conversion are not yet exposed by the high-level API.

The optional `nirs4allformats` reader can feed either path without reparsing
spectra in this package. It accepts homogeneous one-dimensional signals and
keeps sample IDs plus spectral-axis/unit/type identities explicit:

```r
pipeline <- nirs4all_pipeline(list(nirs4all_snv()), nirs4all_pls(2))
dataset <- nirs4all_from_formats("spectra.csv", target = "protein")
fit <- nirs4all_fit(pipeline, dataset)
predictions <- predict(fit, nirs4all_from_formats("new_spectra.csv"))
# Or: nirs4all_dag_cv_refit_predict(pipeline, dataset, cli = ".../dag-ml-cli")
```

The Rust `nirs4all-formats` registry owns file decoding. Files with different
axes, multidimensional signals, missing targets or duplicate sample IDs are
not silently coerced into a training matrix. The flat dataset currently
preserves format labels and metadata, but not the full per-source provenance
available through `nirs4allformats_open_records()`.

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

With `dagml` and a matching `dag-ml-cli` installed, the same pipeline can
delegate folds, OOF evidence, refit and replay to DAG-ML:

```r
outcome <- nirs4all_dag_cv_refit_predict(
  pipeline, X, y, folds = 4,
  cli = "/path/to/dag-ml-cli"
)
outcome$fit_cv_result_count
outcome$replay_prediction_blocks
```

The CLI and `dagml` are still external development dependencies; the package
does not install either automatically. The replay in this path uses the
training cohort, so its predictions are **not** an independent test score.
The returned `workdir` contains the native contracts and refit artifact; pass
an explicit persistent `workdir` if these must survive the current R session.
The adapter reads a trusted RDS data sidecar; do not run it on untrusted files.
Set `NIRS4ALL_REQUIRE_DAG_PARITY=1` and `NIRS4ALL_DAGML_CLI=/absolute/path`
when running the package tests to require native CV/OOF/refit/replay checks.

`nirs4all_n4m_method()` exposes eight native linear MethodResult regressors:
ridge, ridge-PLS, robust PLS, CPPLS, sparse SIMPLS, ECR, continuum regression
and MIR-PLS. These use `n4m` for fitting and coefficient-based prediction;
their R model bundles are not N4MM exports. `nirs4all_lm()` uses base R.
`nirs4all_ranger()` and
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
numerical path. A second frozen Python `n4m` oracle checks six MethodResult
regressors, including solver-sensitive CPPLS, ridge-PLS and continuum
regression. A separate strict test checks native DAG-ML execution with
PLS, `n4m` ridge/CPPLS, `lm`, `ranger`, `glmnet` and `torch` against manual
fold-local fits.
Neither test qualifies arbitrary n4m compositions or complex DAG graphs.

Current missing product gates are documented in
[`dag-ml/docs/R_BINDING_PARITY_AND_INTEROP.md`](https://github.com/GBeurier/dag-ml/blob/fix/v1-stability-dag/docs/R_BINDING_PARITY_AND_INTEROP.md).
