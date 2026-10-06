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
