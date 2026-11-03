# Third-Party Notices — nirs4all-r

`nirs4all-r` (the R package `nirs4all`) is distributed under `CeCILL-2.1 OR AGPL-3.0-or-later`
(R: `CeCILL-2.1 | AGPL (>= 3)`), plus an optional commercial license; see
[`LICENSING.md`](LICENSING.md).

The package is pure R (`NeedsCompilation: no`) and vendors no source from outside the nirs4all
ecosystem. It does carry ecosystem material: `inst/adapters/nirs4all_core_multimodal_adapter.R`
preserves a DAG-ML adapter implementation, and the portable test fixtures are copies of
`nirs4all-core` parity fixtures; both projects are licensed `CECILL-2.1 OR AGPL-3.0-or-later`.
The license texts ship under `inst/LICENSES/`.

The packages below are declared in `DESCRIPTION` and installed separately from CRAN or
R-universe; each keeps its own license. External fields are the CRAN `License` fields checked on
2026-11-03 (digest 0.6.39, jsonlite 2.0.0, yaml 2.3.12, glmnet 5.2, mlr3 1.8.0, parsnip 1.6.1,
processx 3.9.0, ranger 0.18.0, rpart 4.1.27, torch 0.17.0, xgboost 3.2.1.1).

| Package | Role | License (R `DESCRIPTION`) |
|---|---|---|
| `digest` | Imports | GPL (>= 2) |
| `jsonlite` | Imports | MIT + file LICENSE |
| `yaml` | Imports | BSD_3_clause + file LICENSE |
| `stats`, `tools` | Imports (base R) | Part of R: GPL-2 \| GPL-3 |
| `n4m` | Imports (nirs4all ecosystem) | Project license `CECILL-2.1 OR AGPL-3.0-or-later`; R field `CeCILL (== 2.1)` up to 1.3.13, dual from 1.3.14 |
| `dagml`, `dagmldata`, `nirs4allformats`, `nirs4allio`, `nirs4alldatasets` | Suggests (nirs4all ecosystem) | Project license `CECILL-2.1 OR AGPL-3.0-or-later`; R fields being aligned on 2026-11-03 (`nirs4allio` spells it `CeCILL-2.1 \| AGPL-3`) |
| `glmnet` | Suggests | GPL-2 |
| `mlr3` | Suggests | LGPL-3 |
| `parsnip` | Suggests | MIT + file LICENSE |
| `processx` | Suggests | MIT + file LICENSE |
| `ranger` | Suggests | GPL-3 |
| `rpart` | Suggests | GPL-2 \| GPL-3 |
| `torch` | Suggests | MIT + file LICENSE |
| `xgboost` | Suggests | Apache License (== 2.0) \| file LICENSE |

## Why the dual license is allowed

- A dependency's license governs that dependency and works that combine it; it does not restrict
  the license `nirs4all-r` grants on its own code. No dependency declares terms that forbid
  offering `CECILL-2.1 OR AGPL-3.0-or-later` for this package.
- The `AGPL-3.0-or-later` alternative is the license the package already had, so the dual grant
  removes no option a user had before.
- Combining is a separate question that each distributor of a combined work must check: the
  permissive dependencies and the GPL-3/LGPL-3/GPL (>= 2) ones can be combined under AGPL-3.0
  (GPL-3.0 and AGPL-3.0 section 13). CeCILL-2.1 article 5.3.4 lets CeCILL code be distributed
  with code under any version of the GNU GPL (2 or later) or GNU AGPL (3 or later) under that
  GNU license, which covers the optional GPL-2-only `glmnet`. It does not make GPL-2-only code
  compatible with AGPL-3.0, GPL-3.0-only `ranger` or Apache-2.0 `xgboost` inside one combined
  work, and the optional commercial license of this package cannot waive any dependency's
  obligations.
