# Portable runner profiles (`nirs4all_run_portable_pipeline`)

**Status.** The Core operations named here are specified by the portable owner
interface (`PUBLIC_API.md`, frozen 2026-11-08) and its classification addendum
(width amendment, SHA-256 `6cfbf341…dae1`). On **Linux, against the current native
cohort**, 12 unique R scripts passed, covering the linear, classification, graph
(feature-branch) and OPLS profiles, including a true OPLS training and
raw-Kennard-Stone holdout run with non-default flags and MSC. Not executed: the
optional Python peers, Formats-backed and DAG-CV paths, and any other platform.
Direct Methods and frozen-oracle checks are separate evidence. This is **not** a
universal or full-B01 claim. The standalone train-augmentation and
MBPLS/GroupSparsePLS fit-input section below is a private source successor,
**not qualified**: it awaits the next pooled wave.

## Implemented now: linear regression profile (`portable_linear_regression.v1`)

A recipe whose parsed profile is a linear chain of n4m preprocessing steps
(SNV, Savitzky–Golay, LSNV, RNV, area normalization, detrend, MSC, EMSC, SPA,
`n4m.Selector`) ending in PLS or an affine `n4m.*` regressor, with an optional
Kennard-Stone holdout and an inclusive component sweep, runs through the Core
CLI (`nirs4all-core-archive`):

- `nirs4all_run_portable_pipeline(source, dataset)` → `portable-pipeline-run`
  (requires the Methods library);
- `nirs4all_parse_execution_plan(source, core_cli = <cli>)` →
  `portable-pipeline-plan`, a no-fit owner admission returned unchanged in
  `plan$owner`. Without `core_cli` the function is the host's source-level
  preflight (`plan$admission == "source_preflight"`).

R serializes the **complete original document** (comments, root controls and
unknown fields included; explicit JSON/YAML types are preserved, so `params: []`
stays an array and `params: {}` an object; YAML sequences stay arrays) and the
dataset. `nirs4all_load_pipeline()` keeps its four visible fields and attaches
the original document with the visible baseline: an unedited loaded definition
sends the original unchanged, and an edited visible field (`name`, `description`,
`random_state`, `pipeline`) replaces only its counterpart, so edits such as a
changed SNV `with_std` or PLS `scale_x` reach the owner while unrelated root
controls remain. An edited `pipeline` is the visible, comment-free step list.
Only an R-list source, which cannot express `{}`, has an empty unnamed
`params`/`method_params` adapted to an empty mapping. Splitting
(Kennard-Stone on raw X, Methods order), preprocessing fitted on the training
rows only, variant generation, RMSE scoring and first-tie selection are owner
work. R keeps its source-level refusals (Kennard-Stone `test_size` only, so
`n_train` is refused; Savitzky–Golay `delta = 1` only; accepted SNV `ddof`,
`with_mean`, `with_std` and PLS `algo`, `center_*`, `scale_*`) and projects the
owner result into the existing fields (`name`, `rows`, `cols`, `split` with
zero-based ordered indices, `variants`, `selected`, `targets`, `evaluation`).
Added fields: `profile`, `mode`, `identities`, `owner`, and per-variant `params`,
`ordinal`, `variant_id`. Evaluation is selection-validation (`independent_test`
is `FALSE`) with a splitter and `training` without one. No winner is refit and no
model is returned: this runner returns evaluation only.

There is **no R fallback** for this profile. A missing CLI, a CLI without the
portable commands, a missing Methods library, an owner refusal or a malformed
response is an error. CLI resolution: `core_cli` argument, the record-backed
dataset's own `core_cli`, `NIRS4ALL_CORE_CLI`, then `PATH`. Methods library:
`methods_library`, `N4M_LIB_PATH`, then `N4M_LIBRARY_PATH`.

### Dataset forms

| Input | Forwarded | Notes |
| --- | --- | --- |
| `list(X, y[, rows, cols])` (matrix, nested or flat X) | `X`, `y` | Row-scoped positional execution IDs (`position_0`, …) assigned by the owner. Row names are not identity. No groups. |
| `nirs4all_from_formats()` object | `X`, `y`, `sample_ids`, `axis` (`kind`, `unit`, `signal_type`, `wavelengths`), and when present `metadata`, `provenance` | Declared IDs and axis facts at 17 significant digits. `metadata`/`provenance` are the dense-contract arrays: one opaque JSON object or list per row (empty allowed), in X/`sample_ids` order, passed unchanged; they are not groups or identity and are never folded into `axis`. The owner refuses a length that differs from X. `formats` stays in R. The owner returns the full arrays at the common paths `identities.metadata` and `identities.provenance`, aligned with `identities.sample_ids` (X row order); R's projection copies `identities` whole, so they are preserved without reordering or inferring identity or group fields. This is a source-level fact: a Formats-backed run was not executed. |
| `nirs4all_dataset()` record object | the complete IO record | Projected by the owner's IO provider (single finite source, declared IDs). Masks, multiple sources or targets, non-train partitions, declared folds, groups or repetitions are refused by the owner by name. Nothing is reassembled in R. |

## Classification profile (`portable_linear_classification.v1`; Linux cohort-executed)

A recipe whose terminal model is `n4m.SparsePLSDA` (preprocessing chain,
optional Kennard-Stone holdout, inclusive `n_components` range, `sparsity_lambda`)
runs through the same two commands; R sends `profile =
"portable_linear_classification.v1"` and the complete original document. The
owner admits it, runs bank v2 (`single_resampled_holdout_bank.v2` /
`training_bank.v2`, native accuracy, maximized, first-candidate ties) and returns
`variants[].accuracy`, `selected`, class-id `predictions`/`targets` and
`unseen_validation_classes`. R computes no score, winner, argmax, softmax,
intercept or class count, and has no fallback.

- **Codec (existing R, unchanged).** Factor/character targets: `id =
  as.integer(factor) - 1L` in R's own level order (a factor's unsorted levels are
  kept; a character vector is factored alphabetically). Numeric codes: `id =
  match(y, sort(unique(y))) - 1L`, keeping the integer or double type. The dense
  `y` carries only these nonnegative integral ids; there is **no** class-label or
  domain field on the wire. Class names and the numeric type stay in R
  (`result$class_values`) and the owner's ids are decoded by the lookup
  `class_values[id + 1L]`. An id outside the domain is an error.
- **Reports.** `result$unseen_validation_classes` is the owner's
  `[{class_id, count, sample_ids}]`, copied untouched (identical to
  `result$owner$evidence$unseen_validation_classes`). It lists *evaluated* rows
  whose class is outside the fitted classifier's domain and can be nonempty in
  both holdout and training mode (for example after a training sample filter);
  those rows stay evaluated and count as errors. Recorded behavior change: the
  old local runner could predict an absent class through the kernel's empty
  indicator column; the owner cannot. This is not a claim of universal parity.
- **Datasets.** Plain lists and Formats-backed datasets (declared `sample_ids`,
  `axis`, `metadata`, `provenance`) are marshalled as for regression with the
  codec above. A record-backed dataset is forwarded unchanged and refused by the
  owner by name until Core carries IO `y_categorical` class labels; R parses no
  record.
- **Width.** `n_components ≤ min(n_fit − 1, p_transformed)` is enforced by the
  Methods classifier after the preprocessors are trained, on the actual terminal
  matrix (the width guard passed in the Linux native-cohort classification run;
  other platforms and profiles were not executed, and this is not a universal
  claim); there is no R-side transform whitelist or width predictor.

## Feature branch profile (`portable_feature_regression.v1`; Linux cohort-executed)

A regression recipe with `branch` immediately followed by `merge: "features"` and
a terminal model (preprocessing may precede and follow the merge; optional
Kennard-Stone on raw X) is owner-executed: DAG compiles the original definition
into per-estimator native nodes, a native `controller:n4m.feature_join` and the
terminal model (profile `portable_feature_regression.v1`, owner profile
`feature_graph.v1`, banks `single_resampled_holdout_graph_bank.v1` /
`training_graph_bank.v1`). R sends the **complete original definition unchanged**
(branch names and their object order, array form, `{name, steps}` entries,
parameters, merge) and the explicit profile; it flattens, reorders and runs
nothing locally. Every branch is fitted on the same training rows only and the
join concatenates branch columns in declared order (Methods owns selector column
order). The result keeps the linear fields; where the owner supplies them,
`variants[].feature_blocks` and `selected$feature_blocks` are copied untouched:
objects keyed by join node id holding ordered `{branch_id, column_count}`. R
computes no widths, fabricates no merged feature labels or wavelength axis
(`identities$axis` still describes the original input), and the profile offers
no fitted model, deployment or cold replay.

**Limits.** A feature branch combined with a classifier, training augmentation
or extra fit inputs stays a declared residual (below); R's source refusals
(for example augmentation with a branch) are kept. Owner errors are errors.
See the status note above for what was executed.

## Standalone train augmentation and extra fit inputs (same profile; source-only, unqualified)

A **regression** recipe with the `train_augmentation` prefix
(`n4m.NativeXAugmentation`: `kind`, positional `values`, exact `seed`) and/or a
terminal `n4m.MBPLS` with `block_sizes` / `n4m.GroupSparsePLS` with
`group_assignment`, with **no branch and no classifier**, takes the existing
profile `portable_feature_regression.v1` (owner profile `feature_graph.v1`,
banks `single_resampled_holdout_graph_bank.v1` / `training_graph_bank.v1`) through
the unchanged `portable-pipeline-plan` / `-run` commands. R sends the original
document unchanged: `values` stay the declared numeric array in the declared
order (booleans and enum positions are numbers `0`/`1` and indices; the owner
types them from the live Methods manifest), `seed` is the exact declared integer
(`0 … 2^53-1`, including values above 2^31; R never casts it), and
`block_sizes` / `group_assignment` stay positional feature-axis arrays (not sample
groups; the owner lowers them to the terminal fit inputs and Methods checks their
width on the actual transformed matrix). R keeps its existing source refusals
(augmentation first, not with a selector, DI-PLS or a branch; positional arrays)
and adds no width inference, method table or alias.

Owner semantics (separate contract, not R work): Kennard-Stone splits raw X, the
augmenters perturb only the fit rows in declared order with their own seeds (no
rows are added), preprocessors learn from the augmented rows, and training-scope
scores use the raw fit rows after learned preprocessing. There is no
`feature_blocks` without a branch, no fitted model, deployment or replay.
Owner-vs-R equality for GroupSparsePLS is same-kernel interoperability, not an
independent oracle.

## Temporary residual B01 table

These profiles are selected from the **declared** parsed recipe before any
execution (`result$profile`, `plan$profile`) and keep the unchanged local R runner.
They are never reached by catching an owner error. Each one is a **required later
migration inside the same consolidation goal**, not a permanent exception, not a
frozen reference and not DAG/native parity. B01 is not closed by this batch.

| Key | Source reason (current) | Required next owner seam |
| --- | --- | --- |
| `feature_branch` (only when combined with a classifier or `extra_fit_inputs`) | `branch` + `merge: features` with one of those: the owner graph profile qualifies regression feature branches only. | Combined owner profile (graph classification, augmentation or fit-input transport) once each qualifies. |
| `train_augmentation` (only with a classifier) | Training augmentation combined with a classifier: the owner graph profile qualifies regression only. | Graph classification profile. |
| `extra_fit_inputs` (only with a feature branch) | `n4m.MBPLS` `block_sizes` / `n4m.GroupSparsePLS` `group_assignment` combined with a feature branch: the owner graph profile refuses fit inputs with a branch. | Owner branch plus fit-input profile. |

A recipe may carry several keys. A classifier or an `opls_nipals` model combined
with one of these keys stays residual through *that* key until its topology,
augmentation or extra-input profile qualifies; it is never split or flattened.
`opls_nipals` (Methods role `models.pls.opls`, flags forwarded from the original
document, no solver substitution) is on the owner route; the Linux native-cohort R OPLS
check passed (training and raw-Kennard-Stone holdout, MSC, non-default flags,
terminal `models.pls.opls` identity). A record-backed dataset with a residual profile is
refused by name (no X/y is invented).

## Tests

Classification, graph and OPLS scripts were executed as stated in the status note;
`portable-owner-augmentation-transport.R` and `portable-owner-augmentation.R`
(the standalone augmentation / fit-input profile) are authored and **not yet
executed**.

- `tests/portable-owner-transport.R`: shell-stub inspection of the argument
  vector, original-document serialization, dataset forms, projection (the stub's
  winner is deliberately not the minimum RMSE) and error propagation. Transport
  only; no scientific claim. POSIX.
- `tests/portable-residual-dispatch.R`: declared dispatch, exact residual table
  (a branch with a classifier or fit inputs, a classifier with augmentation;
  classifier, OPLS, feature-branch-alone and standalone augmentation / fit-input
  recipes are owner-routed), a failing stub proves residual recipes never reach
  the owner and linear failures never reroute.
- `tests/portable-owner-classification-transport.R`: stub-only codec and
  projection checks of the classification profile (unsorted factor levels, integer
  and double codes, ids-only wire without any label field, training-mode unseen
  report, Formats metadata/provenance, original-document edits, plan operation,
  OPLS flags, record-backed refusal, boundary refusals). No fit; no parity claim.
- `tests/portable-owner-classification.R`: real Core CLI classification profile
  (same two prerequisites; part of the Linux native-cohort run).
- `tests/portable-owner-branch-transport.R`: stub-only checks of the feature
  branch profile (exact profile, non-alphabetical object order in the sent bytes,
  `feature_blocks` passthrough, plan operation, residual combinations, owner
  error without fallback). No fit; no parity claim.
- `tests/portable-owner-branch.R`: real Core CLI feature-branch profile (same two
  prerequisites; part of the Linux native-cohort run); `portable-branch-merge.R`
  (unchanged) runs its branch recipe through the owner route and needs the same
  prerequisites.
- `tests/portable-owner-augmentation-transport.R`: stub-only checks of the
  standalone augmentation / MBPLS / GroupSparsePLS profile (exact profile, declared
  values, order, multi-parameter numbers and seeds above 2^31 in the sent bytes,
  unchanged arrays, no `fit_inputs_v1`/`groups` invented, combined residuals,
  owner error without fallback). No fit; no parity claim.
- `tests/portable-owner-augmentation.R`: real Core CLI `band_perturb` check (same
  two prerequisites, not executed): training and Kennard-Stone holdout against the
  existing local R fit/predict.
- `tests/portable-owner-linear.R`: real Core CLI profile; needs `NIRS4ALL_CORE_CLI`
  and `N4M_LIB_PATH` (`NIRS4ALL_REQUIRE_CORE_PORTABLE=1` makes absence fail).
- Existing runner-calling tests (`portable-json-yaml`, `portable-n4m-preprocessing`,
  `portable-affine-methods`, `spa-selection`, `strict-inputs`) are unchanged and
  now exercise the owner route for their linear recipes; they need the same two
  prerequisites. `portable-branch-merge`, `portable-train-augmentations`,
  `portable-mbpls` and `portable-group-sparse-pls` (all unchanged) run their
  runner calls through the owner graph route, and `n4m-classification` (unchanged)
  its non-branch classifier recipes through the owner classification route, once
  the owner gates pass. `tests/python-oracle.R` does not call the runner and is
  frozen with its oracle JSON.

Platform: Linux/R. Octave/MATLAB are separate bindings and are not qualified here.
