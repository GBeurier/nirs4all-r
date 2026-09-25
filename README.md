# nirs4all for R (development)

License: AGPL-3.0-or-later. The R-universe registry now tracks this development
branch, but a successful external rebuild and installation have not yet been
verified. The package has not been submitted to CRAN.

This is the dedicated R product package named `nirs4all`. The former
`nirs4all-core/bindings/r` package and release workflow were retired from
Core's `main` by [PR #14](https://github.com/GBeurier/nirs4all-core/pull/14),
so the two repositories no longer own the same public R package. Full native
DAG and cross-language trained-pipeline parity remain development gates.

The package composes `n4m` numerical preprocessing and PLS with R learner
controllers on real numeric matrices. Local fit/predict and a first native
DAG-ML path are separate surfaces. The native path covers fixed candidate
selection by CV → OOF → one winner refit → replay. The persisted R refit
artifact can predict new samples locally; branches, adaptive HPO, native
external-cohort replay and Python/R host-model conversion are not yet exposed
by the high-level API.

The built-in `n4m` preprocessing steps now include SNV (with centering/scaling
flags), local SNV, robust SNV, area normalization, polynomial detrend and
Savitzky–Golay. They use the upstream C ABI through the R `n4m` binding;
their numerical kernels are not reimplemented here. Four new operators have
frozen matrix parity tests against Python `n4m` and are exercised in local
and native DAG pipelines. MSC and EMSC learn a reference on each training fold,
store only that vector in the fitted R bundle, and reuse it for validation or
future samples. EMSC also records its polynomial degree in the step definition.
Other train-fitted preprocessing such as baseline centering still needs an
explicitly serialized fit state.
Until the upstream `n4m` R release lands, this development branch requires
`n4m >= 1.0.21.9001` from its `feat/r-preprocessing-parity` branch.

The optional `nirs4allformats` reader can feed either path without reparsing
spectra in this package. It accepts homogeneous one-dimensional signals and
keeps sample IDs plus spectral-axis/unit/type identities explicit:

```r
pipeline <- nirs4all_pipeline(list(nirs4all_snv()), nirs4all_pls(2))
dataset <- nirs4all_from_formats("spectra.csv", target = "protein")
fit <- nirs4all_fit(pipeline, dataset)
predictions <- predict(fit, nirs4all_from_formats("new_spectra.csv"))
# Or: outcome <- nirs4all_dag_cv_refit_predict(pipeline, dataset,
#                                              cli = ".../dag-ml-cli")
#     nirs4all_dag_predict(outcome, nirs4all_from_formats("new_spectra.csv"))
```

The former `nirs4all-core` R JSON/YAML PipelineConfigs reader has moved here.
`nirs4all_load_pipeline()` and `nirs4all_run_portable_pipeline()` preserve its
bounded Kennard-Stone/SNV/Savitzky-Golay/PLS subset, including component
sweeps, and are checked against the four frozen Python examples. Unsupported
operators fail explicitly. Holdout selection RMSE is not an independent test
score; use the native DAG path for CV/OOF/refit.

For this subset, Python-style, `n4m.*` and language-neutral method names
resolve to the same R `n4m` operations. This is recipe portability, not yet
full binary portability: the existing fitted-pipeline RDS contains portable
N4MM model bytes but wraps them in an R-specific object. Cross-language
Archive V2/V3 pipeline replay and retraining still need a validated native
archive reader and explicit preprocessing-state/lineage contracts.

The former core R upstream accessors are also available here:
`nirs4all_upstreams()`, `nirs4all_require()`, `formats()`, `methods()`,
`dag_ml()` and the DAG-ML local implementation registry delegate to their
owning packages. Missing optional domains fail explicitly.

The Rust `nirs4all-formats` registry owns file decoding. Files with different
axes, multidimensional signals, missing targets or duplicate sample IDs are
not silently coerced into a training matrix. The converter retains per-record
provenance when the installed `nirs4allformats` version supplies it; older
releases expose it only through `nirs4allformats_open_records()`.

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

Pass a named list to compare complete preprocessing/learner pipelines on the
same folds. DAG-ML ranks their OOF RMSE and refits only the winner:

```r
candidates <- list(
  pls2 = nirs4all_pipeline(list(nirs4all_snv()), nirs4all_pls(2)),
  pls4 = nirs4all_pipeline(list(nirs4all_snv()), nirs4all_pls(4))
)
outcome <- nirs4all_dag_cv_refit_predict(candidates, X, y,
                                        cli = "/path/to/dag-ml-cli")
outcome$bundle$selected_variant_id
outcome$bundle$metadata$variant_catalog
```

Use `nirs4all_dag_predict(outcome, new_X)` for independent samples after the
refit. It verifies the winning artifact's SHA-256 fingerprint before loading
the R model and enforces its training feature order. Keep `outcome$workdir`
and load only trusted RDS artifacts. This prediction runs locally, not as a
new DAG-ML phase, and it does not create a test score.

This is a fixed candidate grid, not nested CV or adaptive hyperparameter
search. Its selected OOF score is optimistic if reported as an unbiased
generalization estimate; use independent outer validation for that purpose.

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
fold-local fits. It also checks a five-candidate PLS sweep against manual
fold-local OOF calculations and the selected refit, plus cross-family
selection among `n4m` PLS/ridge and `ranger`. External predictions are checked
against independent full-data fits.
Neither test qualifies arbitrary n4m compositions or complex DAG graphs.

Current missing product gates are documented in
[`dag-ml/docs/R_BINDING_PARITY_AND_INTEROP.md`](https://github.com/GBeurier/dag-ml/blob/fix/v1-stability-dag/docs/R_BINDING_PARITY_AND_INTEROP.md).
