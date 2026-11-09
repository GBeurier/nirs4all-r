# nirs4all 0.7.5

- Dependency floors follow the native-container cohort: Methods `n4m >= 1.3.15`
  (native N4MM format 3), `dagml`/DAG CLI `>= 0.4.0`, and the Core archive CLI and
  Python `nirs4all-core >= 0.5.0`. The two Core requirement statements are
  aligned; `nirs4allio >= 0.2.6` is unchanged.
- `nirs4all_execution_profiles()` documentation now describes the five native
  profiles at version 1 (`native-workflow`, `native-conformal`,
  `native-methods-package`, `native-n4a-canonical`, `n4a-container`) instead of two.
  The embedded registry data is unchanged; R remains an externally federated
  surface, and readability, execution support and scientific qualification stay
  separate.

- `nirs4all_core_archive()` now opens Core N4a V1 containers (`archive$schema_version`
  1, profile `native-methods-package`). The Package schema is reported as
  `archive$package_schema_version` (2 predictor, 3 refit), the root physical member
  as `archive$package_member`, and `archive_id`/`archive_sha256` are the container ID
  and full-file hash. It gains an optional `entrypoint` forwarded to Core; old
  pre-V1 ZIP archives are refused with no fallback.
- Add `nirs4all_n4a_inspect()`, `nirs4all_n4a_produce()`,
  `nirs4all_n4a_predict()` and `nirs4all_n4a_retrain()` for canonical V1 `.n4a`
  containers through the `n4a-*` commands of `nirs4all-core-archive`. They take
  record files by path, return rows in physical record order aligned by owner
  sample IDs, keep the owner replay and receipt unchanged, and report
  readability without claiming executability. Core, DAG and Methods decide which
  recipes and profiles run; R has no method list and no fallback route.
- `nirs4all_n4a_retrain()` calls the Core owner `retrain_n4a_native`; the source
  ID and stored recipe come from the validated package and owner errors
  propagate unchanged.
- Add `nirs4all_n4a_write()` and `nirs4all_n4a_read()` for owner-neutral
  exchange of arbitrary members (data, recipes, results, scores, opaque host
  models) through the `n4a-write` and `n4a-read` commands. They take exact
  manifest, member-list and member files by path, update an immutable base
  without overwriting, return numbered extracted files plus the unchanged owner
  report, and need no Methods runtime. Readable is not executable.
- Every `nirs4all_n4a_*` transport accepts an optional `memory_budget_bytes`,
  forwarded to the CLI as the exact decimal text of `--memory-budget-bytes`.
  `NULL` or `""` (the default) sends no flag and R adds no cap or default. A
  whole number up to 2^53 - 1 or decimal text up to 18446744073709551615 is
  accepted; anything else, including a larger double, is refused before any CLI
  call. Reports keep `bigint_as_char = TRUE` and the exact `native_json`.
- Add `tests/n4a-canonical.R`, a `standard_scale` + `ridge` two-target profile
  witness (produce, inspect, predict, retrain) that skips explicitly without the
  native executable, Methods library and fixtures.

# nirs4all 0.7.4

- Licensing metadata only: the package is dual-licensed
  `CeCILL-2.1 | AGPL (>= 3)` (SPDX `CECILL-2.1 OR AGPL-3.0-or-later`) instead of
  `AGPL (>= 3)`, like the rest of the nirs4all ecosystem. `LICENSING.md` and
  `THIRD_PARTY_NOTICES.md` document the choice and the dependency licenses;
  both license texts are installed under `inst/LICENSES/`.

# nirs4all 0.7.3

- Replay Methods N4MM format 3 PLS state, with or without native embedded
  preprocessing, while retaining format 1/2 compatibility and strict recipe,
  dimension, capability and preprocessing ownership validation.
- Verify trained and native round trips against current Methods and genuine
  legacy format 1/2 fixtures; keep rejection of incompatible recipes and state.

# nirs4all 0.7.2

- Update the optional native runtime cohort to Core 0.4.5 and DAG-ML 0.3.41.
- Preserve the R APIs and numerical delegation to n4m 1.3.4.

# nirs4all 0.7.1

- Add native estimator pipelines with CV, candidate selection, refit, export,
  reload and retraining through the Core CLI.
- Add ordered multimodal source policies, native ragged summaries and one
  observed-target campaign per partial regression or classification target.
- Open, query, predict, export and import modern SDK workspaces through the
  validated Python/Core bridge, including paths with spaces and Unicode.
- Require Core 0.4.4, DAG-ML 0.3.39, IO 0.2.6 and n4m 1.3.4 for the new cohort.

Native model arithmetic and persistence stay in Methods; dataset projection
stays in IO, and phase execution stays in DAG-ML. These APIs qualify the
published finite profiles described in Core documentation. Workspace commands
validate a fresh snapshot for each invocation and do not keep a database or
model process open between calls.

# nirs4all 0.7.0

- Integrate DAG-ML 0.3.37, DAG-ML-Data 0.2.13, Core 0.4.2 and n4m 1.3.2.
- Expose and document native workflow, result, resumable tuning, conformal,
  robustness and complete multimodal predictor APIs.
- Retain explicit runtime paths through prediction, tuning resume, calibration
  and export; resolve executable names through PATH and support paths with spaces.
- Publish native workflow and tuning exports only after all members are staged;
  failed exports leave no incomplete destination and existing files are preserved.
- Run tuning tests against the installed package instead of source-relative files.

# nirs4all 0.6.0

- Load and replay the qualified native RolePipeline/N4ME archive V3 profiles
  alongside the existing V2 routes, preserving parent profiles during retraining.
- Replay supported V2/V3 ZIP archives through the public R facade without Python.
- Preserve sample and typed label alignment, lifecycle checks and native
  numerical execution in the qualified cross-language profiles.

U07 process V3 and licensed MATLAB remain outside this release claim. The
persistent process launcher retains its documented POSIX profile.
