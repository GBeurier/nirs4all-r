# CLAUDE.md

@AGENTS.md

## What this repo is

`nirs4all-r` is the end-user R package **named `nirs4all`** (AGPL-3, served by R-universe `gbeurier.r-universe.dev`, not on CRAN yet). It composes:

- `n4m` (R binding of `nirs4all-methods`) for every numerical kernel: preprocessing, PLS family, selectors, splitters, augmentations, N4MM/N4MP serialization;
- `dagml` / `dagmldata` for native CV → OOF → selection → refit → replay;
- `nirs4allformats` / `nirs4allio` for dataset reading;
- R ML/DL learners as controllers (`ranger`, `glmnet`, `parsnip`, `mlr3`, `xgboost`, CPU `torch`).

It must not reimplement numerics. When an `n4m` method lacks a fit/predict/export path, fix it in `nirs4all-methods` (generic C-ABI role), not here.

## Layout

- `R/pipeline.R`, `R/methods.R`, `R/controllers.R`: local pipeline API and learner controllers.
- `R/dag.R`: native DAG-ML path (CLI/ABI).
- `R/portable.R`, `R/r_native_recipe.R`: Level 1 JSON/YAML recipes shared with Python and Core/WASM (`n4m.*` aliases).
- `R/trained_portable.R`, `R/trained_n4mp.R`: Level 2 trained envelopes `nirs4all.n4m.trained_pipeline.v*` (N4MP + N4MM).
- `tests/*.R`: plain R scripts run by `R CMD check` (no testthat). Cross-language oracles use fixtures produced by the Python package.

## Commands

`Rscript` is not on PATH: use `/home/delete/miniconda3/bin/Rscript` and `R`.

```bash
R CMD build .
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-manual nirs4all_*.tar.gz
Rscript tests/native-cross-language.R      # single test script, package installed
```

Local development libraries (not the default library, which lacks `n4m`):
`R_LIBS_USER=<n4m lib>:<nirs4all-r deps lib>`, plus `N4M_LIB_PATH=<libn4m.so>` from a `nirs4all-methods` build and, for cross-language tests, `NIRS4ALL_PYTHON_FULL_ROOT` pointing at a `nirs4all` Python checkout. Record the exact environment in any parity claim.

## Release rules

Test locally; bump the version and publish to R-universe only for a large coherent batch. The `n4m (>= x)` requirement must name a version actually distributed on R-universe (check its Linux/macOS/Windows builds, not only metadata). Nothing is submitted to CRAN without the maintainer.
