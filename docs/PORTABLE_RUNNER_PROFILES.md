# Portable runner profiles (`nirs4all_run_portable_pipeline`)

Source-authored staging; not built, run or qualified. The Core operations named
here are specified by the portable owner interface (`PUBLIC_API.md`, frozen
2026-11-08) and are not claimed to exist in any installed build until the shared
owner compilation wave has run.

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
| `list(X, y[, rows, cols])` (matrix, nested or flat X) | `X`, `y` | Row-scoped positional execution IDs (`position:0`, …) assigned by the owner. Row names are not identity. No groups. |
| `nirs4all_from_formats()` object | `X`, `y`, `sample_ids`, `axis` (`kind`, `unit`, `signal_type`, `wavelengths`), and when present `metadata`, `provenance` | Declared IDs and axis facts at 17 significant digits. `metadata`/`provenance` are the dense-contract arrays: one opaque JSON object or list per row (empty allowed), in X/`sample_ids` order, passed unchanged; they are not groups or identity and are never folded into `axis`. The owner refuses a length that differs from X. `formats` stays in R. **B pending:** the exact owner-result field that carries this context back is not published in `PUBLIC_API.md`; R projects `identities` and `owner` unchanged and invents no key. |
| `nirs4all_dataset()` record object | the complete IO record | Projected by the owner's IO provider (single finite source, declared IDs). Masks, multiple sources or targets, non-train partitions, declared folds, groups or repetitions are refused by the owner by name. Nothing is reassembled in R. |

## Temporary residual B01 table

These profiles are selected from the **declared** parsed recipe before any
execution (`result$profile`, `plan$profile`) and keep the unchanged local R runner.
They are never reached by catching an owner error. Each one is a **required later
migration inside the same consolidation goal**, not a permanent exception, not a
frozen reference and not DAG/native parity. B01 is not closed by this batch.

| Key | Source reason (current) | Required next owner seam |
| --- | --- | --- |
| `classification` | `n4m.SparsePLSDA`, accuracy and ordered class labels: bank v1 selects on RMSE only and the native 1.1.0 `pipeline_steps_v1` controller is regression-only. | Classifier variant of the 1.1.0 controller (labels, exact class order) and an independently versioned bank v2 profile selecting on accuracy, maximized. |
| `feature_branch` | `branch` + `merge: features`: RolePipeline has no topology role and the bank admits a single `N4mRolePipeline` target node. | Owner topology support for feature branches in the portable bank. |
| `train_augmentation` | RolePipeline has no augmenter role and DAG native roles exclude augmenters. | Owner fit-population augmenter support. |
| `opls_solver` | PLS `algo = "opls_nipals"`: R executes the existing `N4M_ALGO_OPLS` kernel, but the generic RolePipeline regressor `models.pls.pls_regression` fixes `N4M_ALGO_PLS_REGRESSION` (`linear_adapters.cpp:358–363`) and exposes no OPLS role. Every other PLS `algo` stays on the owner route. | Methods owner exposes the existing OPLS path as a generic RolePipeline regressor (`methods-opls-role-20261108`) and DAG lowers it; remove this key when the role is qualified. No PLS NIPALS substitution. |
| `extra_fit_inputs` | `n4m.MBPLS` `block_sizes` and `n4m.GroupSparsePLS` `group_assignment`: transport of these extra fit inputs through the controller is not shown in source. | Owner manifest admission of these parameters; move the key to the owner route when the plan operation admits it. |

A recipe may carry several keys. A record-backed dataset with a residual profile
is refused by name (no X/y is invented).

## Tests (authored, not executed)

- `tests/portable-owner-transport.R`: shell-stub inspection of the argument
  vector, original-document serialization, dataset forms, projection (the stub's
  winner is deliberately not the minimum RMSE) and error propagation. Transport
  only; no scientific claim. POSIX.
- `tests/portable-residual-dispatch.R`: declared dispatch, exact residual table
  (including `opls_solver`, with other PLS solvers staying on the owner route),
  a failing stub proves residual recipes never reach the owner and linear
  failures never reroute.
- `tests/portable-owner-linear.R`: real Core CLI profile; needs `NIRS4ALL_CORE_CLI`
  and `N4M_LIB_PATH` (`NIRS4ALL_REQUIRE_CORE_PORTABLE=1` makes absence fail).
- Existing runner-calling tests (`portable-json-yaml`, `portable-n4m-preprocessing`,
  `portable-affine-methods`, `spa-selection`, `strict-inputs`) are unchanged and
  now exercise the owner route for their linear recipes; they need the same two
  prerequisites. `portable-branch-merge`, `portable-train-augmentations`,
  `portable-mbpls`, `portable-group-sparse-pls` and `n4m-classification` exercise
  the residual route. `tests/python-oracle.R` does not call the runner and is
  frozen with its oracle JSON.

Platform: Linux/R. Octave/MATLAB are separate bindings and are not qualified here.
