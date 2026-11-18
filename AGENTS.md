# R product contributor instructions

## Host and owner boundaries

- This repository owns the end-user R package `nirs4all` (CeCILL-2.1 | AGPL >=3). Preserve license/provenance; retired Core R bindings are not its canonical home. R-universe is the release host; CRAN publication needs the maintainer.
- [Methods](../nirs4all-methods/AGENTS.md) supplies n4m preprocessing/models/selectors/splitters/augmentation, generic roles and N4MP/N4MM state.
- If fit/predict/export is missing, fix the owner; never duplicate numerical algorithms or training-row policy in R.
- [DAG](../dag-ml/AGENTS.md)/[DMD](../dag-ml-data/AGENTS.md) supply execution versus data/identity; [Formats](../nirs4all-formats/AGENTS.md)/[IO](../nirs4all-io/AGENTS.md) supply reads/assembly.
- These sibling links are optional navigation.
- Local R fit/predict is reported separately from native DAG parity until the actual phase path is integrated and qualified.
- Keep explicit sample alignment, training/validation separation and reproducibility. R ML/DL learners (ranger/glmnet/parsnip/mlr3/xgboost/CPU torch) are controllers over native owners, not a second portable engine.
- `R/pipeline.R`, `methods.R`, `controllers.R` hold host API/controllers; `R/dag.R` the native path.
- `portable.R`/`r_native_recipe.R` carry L1 JSON/YAML aliases; trained_portable/trained_n4mp/trained_roles hold trained envelopes over native role pipelines.
- Native validation, fit routing, column checks and training-row policy stay in n4m.
- Schema/ABI/package versions are distinct; discover supported current envelopes from owner contracts, not a pinned old v8 claim. [README](README.md) is the local entrypoint.

- Dependency packages are `dagml` and `dagmldata` for execution/data contracts, and `nirs4allformats` and `nirs4allio` for reading/assembly.

## Commands and environment

```bash
R CMD build .
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-manual nirs4all_*.tar.gz
Rscript tests/native-cross-language.R
```

- Ensure `R`/`Rscript` are on PATH or supply an explicit locally configured executable; do not publish a workstation-specific absolute interpreter path. The single-test command needs the package installed.
- Tests are plain `tests/*.R` scripts run by R CMD check, not testthat. Cross-language oracles use Python-produced fixtures; record exact environment/evidence and unsupported/skipped paths.
- Local libraries require `R_LIBS_USER=<n4m lib>:<R deps lib>`, `N4M_LIB_PATH=<libn4m.so>` from a Methods build, and `NIRS4ALL_PYTHON_FULL_ROOT` for full-Python cross-language tests. The default library may lack n4m.
- Test locally; version/publish to R-universe only for a coherent large authorized batch.
- Versioning: versions are `X.Y.Z`. `Z` (patch) = fixes and purely additive, backward-compatible features (existing outputs, wire formats, schemas and ABI symbols unchanged); `Y` (minor) = any compatibility break (changed/removed API signature, ABI break or ABI version change, wire/byte/schema/fixture change, renamed ids, changed numerical results of existing operations); `X` = product version. The package that bumps classifies its change. Cross-repository dependencies accept one tested series, `>=X.Y[.Z],<X.(Y+1)` (patch floor only when a fix or feature from that patch is required); for 1.x packages never npm `^1.x` or Cargo `"1.x"` (they accept the next minor): use `~1.4.1`, `">=1.4.1, <1.5"`, `>=1.4.1,<1.5`. Exact pins only in lockfiles, wheel locks and product install lists; intra-repo lockstep may stay exact. R declares a lower bound in `DESCRIPTION` and documents the tested series (README/NEWS). See the [ecosystem version constraints](https://github.com/GBeurier/nirs4all-ecosystem/blob/main/docs/contracts/release/VERSION_CONSTRAINTS.md).
- Required n4m versions must actually be distributed on Linux/macOS/Windows, not merely present in metadata.
- Never submit to CRAN without the maintainer.

## Working rules

- Read applicable local and deeper instructions before editing. Preserve dirty/concurrent work; make authorized owner fixes and propagate affected contracts/bindings/consumers. Do not reset another author's work.
- Use CodeGraph FIRST for implementation navigation only if this checkout's root already has `.codegraph/`; use its exact projectPath or run the CLI here. Never auto-index or silently substitute an ancestor index.
- Unindexed code and documentation use ordinary source navigation. Explicit source, tests and build tools are final authority. Owner links are navigation; this policy works without a parent workspace.
- All application/project content is English: UI text/subtitles/tooltips/errors, docs/comments/docstrings, test/fixture prose, reports/issues/PRs, commits and generated material. Preserve identifiers, quotations and historical/provenance meaning; converse with the maintainer in French.
- Do not commit credentials, private data, generated workspaces, caches or build outputs. Follow local licensing/fixture restrictions and report actual commands/exits/skips; a command reference grants no execution/publication permission.
