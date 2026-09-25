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
artifact can predict new samples locally; bounded parallel preprocessing
branches can be concatenated before a model. General prediction stacking,
adaptive HPO, native
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
`n4m >= 1.0.21.9002` from its `feat/r-preprocessing-parity` branch.

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

`nirs4all_export_pipeline()` emits JSON or YAML from an unfitted R pipeline
for the currently shared Core profile (default SNV, unit-spacing SG, default
PLS). Its output is checked by the R, Python and WASM readers. It rejects
unsupported native options rather than dropping them. This is a recipe export,
not an export of a trained model.

The R reader also imports Python-style `branch` → `merge: features` recipes
with n4m-only preprocessing branches. For branches containing only default
SNV/SG, the R exporter emits the named Python feature-merge syntax; Python's
topology analyzer recognizes it. This branch export has **not** yet been
qualified by the Core/WASM portable readers, unlike the flat profile above.

For the exact default SNV → Savitzky-Golay smoothing → SIMPLS profile,
`nirs4all_fit()` now embeds preprocessing in the native N4MM format-2 model.
`nirs4all_export_native_model()` returns that fitted state as raw bytes;
`nirs4all_import_native_model()` accepts that JSON/YAML recipe or an R
pipeline, validates its native descriptor and predicts directly on raw
spectra. A strict test
fits in R and predicts in a fresh Python process, then fits via Python n4m
and predicts in R, with numerical equality. Other preprocessor combinations
still use R-owned fit state and are refused by this export API. The RDS bundle
remains R-specific. Cross-language Archive V2/V3 replay, recipe/lineage
packaging and retraining still need a validated native archive bridge.

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

For classification, use factor or character labels with the optional
`ranger` probability forest, a `parsnip` classification specification with
an explicit engine, or an untrained `mlr3` `LearnerClassif` supporting
probabilities. The local API returns factors and a probability matrix whose
columns follow the training class order. DAG-ML encodes those
classes as stable numeric labels, validates probabilities in each CV fold,
selects variants by OOF accuracy, and restores factor labels for local
external inference. Every training fold must contain every class; the R
frontend rejects a fold assignment that violates this requirement.

```r
X <- as.matrix(iris[, 1:4])
y <- iris$Species
classifier <- nirs4all_pipeline(
  list(nirs4all_snv()),
  nirs4all_ranger_classifier(num.trees = 200, seed = 7)
)
fit <- nirs4all_fit(classifier, X, y)
labels <- nirs4all_predict(fit, X)
probabilities <- nirs4all_predict_proba(fit, X)
outcome <- nirs4all_dag_cv_refit_predict(classifier, X, y,
  folds = 4, split_steps = TRUE, cli = "/path/to/dag-ml-cli")
new_labels <- nirs4all_dag_predict(outcome, X[1:3, , drop = FALSE])
```

The `ranger`, `parsnip` and `mlr3` models are RDS sidecars, not
cross-language trained-model formats. Their JSON/YAML aliases and trained
artifacts are not yet portable to Python or WASM. For example, the two
framework controllers can replace the `ranger` learner above:

```r
tidymodels_learner <- nirs4all_parsnip_classifier(
  parsnip::set_engine(parsnip::decision_tree(mode = "classification"), "rpart")
)
mlr3_learner <- nirs4all_mlr3_classifier(
  mlr3::lrn("classif.rpart", cp = 0)
)
```

For repeated samples, batches or sites, pass aligned `group_ids`. The R
frontend assigns whole groups to folds deterministically; DAG-ML validates
group boundaries before fitting. Named group IDs must match `sample_ids`
exactly, and there must be at least one distinct group per fold:

```r
outcome <- nirs4all_dag_cv_refit_predict(
  pipeline, X, y, folds = 4,
  sample_ids = rownames(X), group_ids = batch_id,
  cli = "/path/to/dag-ml-cli"
)
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

Set `split_steps = TRUE` to execute each preprocessing step as its own DAG-ML
transform node. Fixed candidates may use different n4m steps and learners,
provided they have the same number of preprocessing steps:

```r
candidates <- list(
  snv_pls = nirs4all_pipeline(list(nirs4all_snv()), nirs4all_pls(2)),
  msc_pls = nirs4all_pipeline(list(nirs4all_msc()), nirs4all_pls(3))
)
outcome <- nirs4all_dag_cv_refit_predict(candidates, X, y,
  split_steps = TRUE, cli = "/path/to/dag-ml-cli")
```

The R process adapters exchange fold-scoped matrices locally; this is not a
cross-language model format or a general branch/merge graph API.

To combine two feature views, `nirs4all_concat()` fits each named branch on
the same training rows, then concatenates its output columns before the
learner. With `split_steps = TRUE`, DAG-ML runs each branch step and the join
as separate nodes (currently for a single pipeline whose sole preprocessing
step is the concat):

```r
pipeline <- nirs4all_pipeline(
  list(nirs4all_concat(list(
    derivative = list(nirs4all_snv(), nirs4all_savgol(5)),
    scatter = list(nirs4all_msc(), nirs4all_detrend(1))
  ))),
  nirs4all_pls(2)
)
outcome <- nirs4all_dag_cv_refit_predict(pipeline, X, y,
  split_steps = TRUE, cli = "/path/to/dag-ml-cli")
```

Use `nirs4all_dag_predict(outcome, new_X)` for independent samples after the
refit. It verifies the winning model and transform artifacts' SHA-256
fingerprints before loading them and enforces the model's training feature
order. Keep `outcome$workdir`
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
regularized-regression controllers. `nirs4all_parsnip(spec)` accepts an
engine-selected `parsnip` regression model, widening the R backend surface
without copying engine implementations. `nirs4all_mlr3(learner)` accepts an
untrained `mlr3` regression learner, clones it independently for every fit,
and uses DAG-ML rather than `mlr3` for CV/OOF/refit. A user-defined controller can be
provided with `nirs4all_controller(fit, predict, name)`; its state is R-only.
The `n4m` PLS model is saved as portable N4MM bytes inside the RDS bundle.
Plain SIMPLS exports N4MM format 1; the exact default SNV → SG → SIMPLS
profile embeds its preprocessing in N4MM format 2. Both have fresh-process
R↔Python prediction tests. Other pipelines still leave R preprocessing
outside the model. The separate recipe is required to retrain, and N4MM
format 1 does not expose scaling flags for independent recipe verification.
`parsnip` and `mlr3` fitted states remain R-specific, as do custom-controller
code and state.
For portable n4m recipes, `nirs4all_expand_portable_pipelines()` expands the
bounded Python `_or_`/`_cartesian_` preprocessing generators and PLS component
ranges into named R pipelines. These can be passed to
`nirs4all_dag_cv_refit_predict()` for native OOF selection; generator modifiers,
other operators, and general DAG branch/stacking constructs still fail
explicitly; the feature-only branch/merge form above is the bounded exception.
The R JSON/YAML reader also resolves `n4m.LSNV`, `n4m.RNV`,
`n4m.AreaNormalization`, `n4m.Detrend`, `n4m.MSC` and `n4m.EMSC` through the
native Methods binding. Their transformed train/validation matrices are checked
against independent Python n4m fits. The cross-language recipe *export* remains
restricted to the earlier default SNV/Savitzky-Golay/PLS subset until these
additional identifiers are qualified by the other language readers.
`nirs4all_torch_mlp()` provides an optional CPU neural-network regressor
through the R `torch` runtime. `nirs4all_torch_module(builder, name)` accepts
a self-contained builder for a custom `nn_module` mapping `N × p` inputs to
`N × 1` regression outputs. DAG-ML invokes a fresh module for each fold and
stores the builder behind a validated R-specific specification fingerprint.
Torch modules are saved with `torch`'s own serializer inside the RDS bundle;
they are R-specific and not ONNX exports or portable Python weights.
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
PLS, `n4m` ridge/CPPLS, `lm`, `ranger`, `glmnet`, `parsnip`, `mlr3` and two R `torch` architectures against manual
fold-local fits. It also checks a five-candidate PLS sweep against manual
fold-local OOF calculations and the selected refit, plus cross-family
selection among `n4m` PLS/ridge and `ranger`. Grouped CV is tested against
manual group-exclusive fold fits, including a tampered group-ID rejection.
External predictions are checked
against independent full-data fits.
Neither test qualifies arbitrary n4m compositions or complex DAG graphs.

Current missing product gates are documented in
[`dag-ml/docs/R_BINDING_PARITY_AND_INTEROP.md`](https://github.com/GBeurier/dag-ml/blob/fix/v1-stability-dag/docs/R_BINDING_PARITY_AND_INTEROP.md).
