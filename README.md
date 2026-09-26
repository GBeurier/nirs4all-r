# nirs4all for R (development)

License: AGPL-3.0-or-later. R-universe serves this development package from
`nirs4all-r`; version 0.4.0.9018 and its `n4m` dependency were installed and
exercised from public source tarballs in a clean R library. Later versions
must be checked after each repository synchronization. The package has not
been submitted to CRAN.

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
SPA selects wavelengths from each training fold's predictors and targets via
`n4m::spa_select()`; validation and prediction reuse its saved indices. The R
fit keeps the native selection rank but projects columns in input order, as
Python's selector does. The JSON/YAML reader and writer recognize `n4m.SPA`
with `top_k` and
`n_components`. R/Python selector and DAG fold parity are checked locally;
Core/WASM recipe qualification remains open.
Other train-fitted preprocessing such as baseline centering still needs an
explicitly serialized fit state.
This development branch requires `n4m >= 1.0.21.9003` for SPA.

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

For classification CSVs, the Rust reader puts text reference labels in
per-record metadata. If explicitly selected as `target`, the R converter
requires that label to be present and non-empty for every sample, then passes
the named character vector to a classification controller. Numeric targets
retain their original regression behavior. No CSV parsing is duplicated here.

The former `nirs4all-core` R JSON/YAML PipelineConfigs reader has moved here.
`nirs4all_load_pipeline()` and `nirs4all_run_portable_pipeline()` preserve its
bounded Kennard-Stone/SNV/Savitzky-Golay/PLS subset, including component
sweeps, and add the n4m preprocessing listed below. The original subset is
checked against four frozen Python examples; unsupported operators fail
explicitly. Holdout selection RMSE is not an independent test score; use the
native DAG path for CV/OOF/refit.

`nirs4all_export_pipeline()` emits JSON or YAML from an unfitted R pipeline.
The original default SNV/unit-spacing SG/PLS profile is checked by R, Python
and WASM readers; the additional n4m preprocessing below is checked by R and
the full Python `nirs4all` parser, with Core/WASM qualification still open.
Unsupported settings fail rather than being dropped. This exports a recipe,
not a trained model.

Thirteen native affine regressions have explicit R↔Python recipe aliases:
`n4m.Ridge`, `n4m.RidgePLS`, `n4m.RobustPLS`, `n4m.CPPLS`,
`n4m.SparseSIMPLS`, `n4m.ECR`, `n4m.ContinuumRegression`, `n4m.MIRPLS`,
`n4m.FusedSparsePLS`, `n4m.BaggingPLS`, `n4m.BoostingPLS` and
`n4m.RandomSubspacePLS` and `n4m.NPLS`.
Their JSON/YAML definitions fit and predict on held-out samples through the
same n4m kernels in both languages, with an independently frozen numerical
oracle. The alias defaults make Ridge's X scaling and the robust/Ridge-PLS
settings explicit; using similarly named host classes without these settings
does not guarantee parity. These thirteen recipe aliases are **not yet** qualified
by the Core/WASM pipeline reader. Their trained-state transfer is separate
from this level-1 recipe support and is not implied by this paragraph.

R additionally supports `n4m.MBPLS` with required `block_sizes`, at least
two positive integer block widths summing to the feature count after
preprocessing. Its recipe uses the low-level Methods NIPALS kernel with
centered X/Y and `scale_x = scale_y = FALSE`. The native fit returns
original-scale coefficients and a separate intercept; held-out prediction
is `X %*% coefficients + intercept`. The Python
`pls4all.sklearn.MBPLSRegression` class defaults to scaling X/Y, so its
default fit is a different recipe. The R JSON/YAML and v5 trained envelopes
carry the block layout for refitting; the N4MM model carries affine prediction
only and cannot attest the fit algorithm or block layout. Cross-language
refitting requires a Python shared reader with the matching `n4m.MBPLS`
alias and unscaled Methods configuration.

For an R-specific JSON/YAML recipe, use `scope = "r_native"` and
`nirs4all_r_pipeline_from_recipe()`. The closed model aliases cover regression
and classification forests via `ranger`, Gaussian elastic-net via `glmnet`,
and CPU MLPs via R `torch`; the same n4m preprocessing and feature-branch
syntax can precede them. For example:

```r
recipe <- nirs4all_export_pipeline(
  nirs4all_pipeline(list(nirs4all_snv(ddof = 1L)),
                   nirs4all_ranger(num.trees = 200L, seed = 7L)),
  format = "yaml", scope = "r_native")
fitted <- nirs4all_fit(nirs4all_r_pipeline_from_recipe(recipe), X, y)
```

These `r.*` aliases deliberately remain R-only: they neither convert a
scikit-learn pipeline nor transfer fitted `ranger`/`glmnet`/`torch` binaries.
The default cross-language export remains restricted to qualified n4m recipes.

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
still use R-owned fit state and are refused by this raw-N4MM export API.
For n4m PLS regression and sparse PLS-DA classification pipelines covered by
the portable recipe,
`nirs4all_export_trained_pipeline()` writes a bounded JSON envelope with the
recipe, fitted MSC/EMSC references (including feature-merge branches), and an
exact hash-checked N4MM model. The classification envelope additionally carries
ordered class labels and native affine decision scores. R imports it with
`nirs4all_import_trained_pipeline()`; full Python `nirs4all` imports it with
`PortableN4MTrainedPipeline.from_json(...)` and can fit a fresh Methods model
via `.retrain(X, y)`. Python can also create the same envelope with
`PortableN4MTrainedPipeline.fit_recipe(recipe, X, y).to_json(...)`, which R
imports. Held-out PLS predictions from both directions agree for plain,
stateless, stateful, embedded and branch profiles. Sparse PLS-DA classification
has been tested in both directions for plain, SNV and MSC profiles, including
retraining. This is **not** a DAG-ML Archive V2/V3 package and does not cover
arbitrary native methods or non-n4m controllers. See
[`docs/PORTABLE_TRAINED_PIPELINES.md`](docs/PORTABLE_TRAINED_PIPELINES.md) for
the exact envelope boundary and validation guarantees.
The RDS bundle remains R-specific.

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

For classification, use factor or character labels with the native
`nirs4all_sparse_pls_da()` controller, the optional `ranger` probability
forest, a `parsnip` classification specification with
an explicit engine, or an untrained `mlr3` `LearnerClassif` supporting
probabilities. Optional `torch` CPU MLP and custom-module classification
controllers use cross-entropy and softmax probabilities. The local API returns factors and a probability matrix whose
columns follow the training class order. DAG-ML encodes those
classes as stable numeric labels, validates probabilities in each CV fold,
selects variants by OOF accuracy, and restores factor labels for local
external inference. Every training fold must contain every class; the R
frontend rejects a fold assignment that violates this requirement.
The sparse PLS-DA controller fits through `n4m`, accepts the portable
`n4m.SparsePLSDA` JSON/YAML model alias, and matches the Python n4m decision
scores on unseen samples. Its softmax outputs are **not calibrated
probabilities**; they are only a DAG-compatible normalization of class scores.
Its decision-score predictor is now an N4MM affine payload inside either a
local RDS bundle or the bounded cross-language trained envelope. Neither is a
DAG-ML Archive V2/V3 package; its softmax remains uncalibrated.
The bounded `nirs4all_run_portable_pipeline()` reader can execute such a
recipe on categorical R data, including a `nirs4all-formats` dataset, and
select a component variant by accuracy. Numeric Python-style class codes are
accepted by this recipe runner and returned unchanged in type. This alias has
not yet been qualified
in the Python/Core/WASM pipeline readers, so the recipe is not currently a
proven level-1 cross-language classifier.

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

The R process adapters exchange fold-scoped matrices locally. Eligible
native-only n4m refits (plain PLS, embedded SNV→SG→PLS, or an affine
MethodResult without external preprocessing) now carry N4MM bytes in the
DAG-ML bundle rather than an RDS model sidecar. Python can consume those
same model bytes. This is still not a full interlanguage training archive:
the DAG plan, recipe, data identities, and retraining contract need a portable
Archive V2/V3 package. Other controllers remain RDS-backed.

To combine two feature views, `nirs4all_concat()` fits each named branch on
the same training rows, then concatenates its output columns before the
learner. With `split_steps = TRUE`, DAG-ML runs each branch step and the join
as separate nodes. A concat can follow and precede other n4m preprocessing
steps within one pipeline; fixed-candidate variants containing a concat are
not yet supported:

```r
pipeline <- nirs4all_pipeline(
  list(nirs4all_spa(top_k = 6),
       nirs4all_concat(list(
         scatter = list(nirs4all_msc(), nirs4all_detrend(1)),
         normalized = list(nirs4all_snv())
       )),
       nirs4all_snv()),
  nirs4all_pls(2)
)
outcome <- nirs4all_dag_cv_refit_predict(pipeline, X, y,
  split_steps = TRUE, cli = "/path/to/dag-ml-cli")
```

Use `nirs4all_dag_predict(outcome, new_X)` for independent samples after the
refit. It verifies the winning model and transform artifacts' SHA-256
fingerprints before loading them and enforces the model's training feature
order. A native-only raw bundle predicts after transfer to a fresh R process
without its original `workdir`; RDS-backed pipelines still require that
directory and trusted sidecars. This prediction runs locally, not as a
new DAG-ML phase, and it does not create a test score.

This is a fixed candidate grid, not nested CV or adaptive hyperparameter
search. Its selected OOF score is optimistic if reported as an unbiased
generalization estimate; use independent outer validation for that purpose.

The CLI and `dagml` are still external development dependencies; the package
does not install either automatically. The replay in this path uses the
training cohort, so its predictions are **not** an independent test score.
The returned `workdir` contains the native contracts and any host sidecars;
pass an explicit persistent `workdir` when an RDS-backed pipeline must
survive the current R session. Native-only N4MM bytes are inside the bundle.
The adapter reads a trusted RDS data sidecar; do not run it on untrusted files.
Set `NIRS4ALL_REQUIRE_DAG_PARITY=1` and `NIRS4ALL_DAGML_CLI=/absolute/path`
when running the package tests to require native CV/OOF/refit/replay checks.
For the exported-recipe and trained-envelope R↔Python regression tests, set
`NIRS4ALL_METHODS_PYTHON` to a Python executable and
`NIRS4ALL_PYTHON_FULL_ROOT` to a checkout of full Python `nirs4all` containing
the shared n4m alias resolver; the Methods Python binding must also be on
`PYTHONPATH` with a matching `N4M_LIB_PATH`.

`nirs4all_n4m_method()` exposes fourteen native linear MethodResult regressors:
ridge, ridge-PLS, robust PLS, CPPLS, sparse SIMPLS, ECR, continuum regression
MIR-PLS, fused sparse PLS, bagging PLS, boosting PLS, random-subspace PLS and
N-PLS and MB-PLS. N-PLS requires explicit positive `mode_j` and `mode_k` with
`mode_j * mode_k` equal to the feature width after preprocessing.
MB-PLS requires an explicit positive integer `block_sizes` vector summing to
that width.
These use `n4m` for fitting. Their R bundles now contain a
portable N4MM affine predictor, and a preprocessing-free fit can be exported
as N4MM for Python/R inference. That artifact attests the fitted affine
prediction, not the original fitting method, its hyperparameters, or a
retrainable pipeline; export the recipe separately. A MethodResult fit with
R preprocessing can still be saved and replayed in R, but its bare N4MM
export is rejected because it would omit the preprocessing. `nirs4all_lm()`
uses base R.
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
The R JSON/YAML reader and exporter resolve `n4m.LSNV`, `n4m.RNV`,
`n4m.AreaNormalization`, `n4m.Detrend`, `n4m.MSC` and `n4m.EMSC` through the
native Methods binding. Exported recipes have been parsed and executed in the
full Python `nirs4all` step parser with its n4m-backed aliases, including
stateful MSC/EMSC fitted on training rows only; predictions match R. Core/WASM
qualification of these additional aliases is still pending. Recipe export does
not include fitted state; use a native N4MM bundle for a supported trained
model, and preserve external fitted preprocessing separately.
`nirs4all_torch_mlp()` provides an optional CPU neural-network regressor
through the R `torch` runtime. `nirs4all_torch_module(builder, name)` accepts
a self-contained builder for a custom `nn_module` mapping `N × p` inputs to
`N × 1` regression outputs. DAG-ML invokes a fresh module for each fold and
stores the builder behind a validated R-specific specification fingerprint.
The corresponding `nirs4all_torch_mlp_classifier()` and
`nirs4all_torch_module_classifier(builder, name)` controllers accept factor
targets; a classification builder receives both the feature and class counts
and emits `N × classes` raw logits. Fold-local OOF and fresh-process replay
are tested. Torch modules are saved with `torch`'s own serializer inside the RDS bundle;
they are R-specific and not ONNX exports or portable Python weights.
Custom controllers may capture non-serializable R state; their bundles are
only reliable when the controller author has tested a fresh-process load.

The optional `nirs4all_xgboost()` and `nirs4all_xgboost_classifier()`
controllers fit CPU XGBoost boosters per training fold. For example:

```r
pipeline <- nirs4all_pipeline(
  list(nirs4all_snv()),
  nirs4all_xgboost_classifier(nrounds = 80, max_depth = 3,
                              eta = 0.1, seed = 7, nthread = 1)
)
fit <- nirs4all_fit(pipeline, X, factor(y))
probabilities <- nirs4all_predict_proba(fit, X)
outcome <- nirs4all_dag_cv_refit_predict(pipeline, X, factor(y), folds = 5)
new_labels <- nirs4all_dag_predict(outcome, new_X)
```

The fitted booster is saved as XGBoost model bytes inside the R sidecar.
This provides RDS replay with the XGBoost runtime installed; it does not add
XGBoost to the cross-language N4MM or JSON/YAML recipe subset.
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
[`dag-ml/docs/R_BINDING_PARITY_AND_INTEROP.md`](https://github.com/GBeurier/dag-ml/blob/main/docs/R_BINDING_PARITY_AND_INTEROP.md).
