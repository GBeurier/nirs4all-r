# CRAN form drafts — not for submission yet

The CRAN web form accepts a source tarball built by `R CMD build` and an
optional comment. The maintainer must confirm the resulting email. Submit
`n4m` first, wait for its CRAN publication, then build and check a stable
`nirs4all` version against that release. Do not submit either development
tarball from this workspace as-is. See [CRAN policy](https://stat.ethz.ch/CRAN/web/packages/policies.html)
and the [submission checklist](https://cran.r-project.org/web/packages/submission_checklist.html).

## `n4m` — first submission

- Name/email: use the final `Maintainer` field in the `n4m` DESCRIPTION.
- Package: `n4m_<STABLE_VERSION>.tar.gz` after `R CMD build` and multi-OS
  `R CMD check --as-cran` of that exact file.
- Optional comment, replacing every placeholder with evidence:

> This is the first CRAN submission of n4m, the R binding to the
> nirs4all-methods native numerical engine for spectroscopy and chemometrics.
> The binding calls the shared C ABI and does not duplicate numerical kernels.
> The source tarball includes the upstream sources and copyright/licence notices
> required for an offline build. The exact submitted tarball was checked with
> R-devel on Linux, Windows and macOS: <RESULTS AND LINKS>. Remaining notes:
> <NONE OR EXPLANATION>. The native Methods ABI version tested was <VERSION>.

## `nirs4all` — only after `n4m` reaches CRAN

- Name/email: use the final `Maintainer` field in `nirs4all/DESCRIPTION`.
- Package: `nirs4all_<STABLE_VERSION>.tar.gz`, built and checked after its
  `Imports: n4m` minimum is changed from a development version to the CRAN
  release.
- Optional comment, replacing every placeholder with evidence:

> This is the first CRAN submission of nirs4all for R. It composes the n4m
> numerical engine with R learner controllers and optional DAG-ML, formats and
> dataset integrations. No numerical kernel or vendor-format parser is copied
> into this package. The exact submitted tarball was checked with R-devel on
> Linux, Windows and macOS: <RESULTS AND LINKS>. All strong dependencies are
> available from CRAN or Bioconductor; optional repositories and their
> installation paths were checked: <EVIDENCE>. Remaining notes:
> <NONE OR EXPLANATION>.

## Do not use the local result as final evidence

On 2026-09-25, `nirs4all_0.4.0.9021.tar.gz` passed Linux/R 4.6.0 checks with
0 errors but 1 CRAN-incoming warning: `n4m` is still outside CRAN/Bioconductor,
and the first run did not force all optional `Suggests` to be installed. A
second check of the same tarball with all `Suggests`, strict DAG parity,
Python n4m oracle and CPU torch passed with the same 0-error/1-warning result.
The R-universe
`dagml` source package was installed and exercised with a locally built DAG CLI.
`n4m_1.0.21.9002.tar.gz` passed Linux checks with 0 errors, 0 warnings and
2 notes after an autonomous vendored-source build. Both are development
versions. An earlier `nirs4all` 0.4.0.9018 build passed the R-universe
Linux/Windows/macOS/WASM matrix and installed with public `n4m` sources in a
fresh Linux R library, but that is not a multi-OS check of the exact 0.4.0.9021
tarball. Complete trained-pipeline interoperability is also not yet qualified. The
form text above is a template, not a claim that these gates have passed.
