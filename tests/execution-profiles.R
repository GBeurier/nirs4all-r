# Source metadata contract for the federated execution profiles. This test
# needs only jsonlite: it never starts the native stack and is never gated, so
# a missing dependency is an error, not a silent pass. It prints
# `EXECUTED:execution-profiles cells=<n> entrypoints=<n>` only after every assertion ran.
#
# Run from a source checkout:  NIRS4ALL_R_SOURCE=<repo> Rscript tests/execution-profiles.R
# or against an installed package (library(nirs4all)).
root <- Sys.getenv("NIRS4ALL_R_SOURCE")
if (!nzchar(root) && file.exists(file.path("R", "execution_profiles.R"))) root <- getwd()
if (nzchar(root)) {
  source(file.path(root, "R", "execution_profiles_data.R"))
  source(file.path(root, "R", "execution_profiles.R"))
  namespace_lines <- readLines(file.path(root, "NAMESPACE"))
  exported <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", namespace_lines, value = TRUE))
} else {
  library(nirs4all)
  exported <- getNamespaceExports("nirs4all")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite is required", call. = FALSE)

profiles <- nirs4all_execution_profiles()
by_id <- setNames(profiles$profiles, vapply(profiles$profiles, `[[`, character(1), "id"))
surfaces <- c("python", "javascript_wasm", "rust", "matlab_octave")
operations <- c("compile", "fit", "predict", "import", "export", "produce", "inspect", "calibrate",
                "retrain", "read", "write", "update")
profile_ids <- c("native-workflow", "native-conformal", "native-methods-package",
                 "native-n4a-canonical", "n4a-container")
stopifnot(
  identical(profiles$schema, "nirs4all-core.execution-profiles.v1"),
  identical(profiles$version, 1L),
  identical(profiles$generated_from, "compat/capabilities.toml"),
  identical(unlist(profiles$runtime_surfaces), surfaces),
  identical(unlist(profiles$operations), operations),
  "nirs4all_execution_profiles" %in% exported,
  # The declared set is exactly these five profiles at version 1, in this order.
  identical(names(by_id), profile_ids),
  identical(vapply(profiles$profiles, `[[`, integer(1), "version"), rep(1L, 5L)))

# Exact operation declaration, written independently of the producer; statuses follow the
# surface order python, javascript_wasm, rust, matlab_octave. Every operation a profile does not
# declare is an explicit refusal in all four surfaces.
S <- "supported"; U <- "unsupported"; allU <- c(U, U, U, U)
expected <- list(
  "native-workflow" = list(compile = c(U, U, S, U), fit = c(S, S, S, S), predict = c(S, S, S, S),
                           import = c(S, S, U, S), export = c(S, S, U, S), produce = allU, inspect = allU,
                           calibrate = allU, retrain = allU, read = allU, write = allU, update = allU),
  "native-conformal" = list(compile = allU, fit = c(S, S, S, S), predict = c(S, S, S, S),
                            import = c(S, S, U, S), export = c(S, S, U, S), produce = allU, inspect = allU,
                            calibrate = allU, retrain = allU, read = allU, write = allU, update = allU),
  "native-methods-package" = list(compile = allU, fit = allU, predict = c(S, S, S, U),
                                  import = c(S, S, S, U), export = c(S, S, S, U), produce = allU,
                                  inspect = allU, calibrate = allU, retrain = allU, read = allU,
                                  write = allU, update = allU),
  "native-n4a-canonical" = list(compile = allU, fit = allU, predict = c(S, S, S, S), import = allU,
                                export = allU, produce = c(S, S, S, S), inspect = c(S, S, S, S),
                                calibrate = c(U, U, S, U), retrain = c(S, S, S, S), read = allU,
                                write = allU, update = allU),
  "n4a-container" = list(compile = allU, fit = allU, predict = allU, import = allU, export = allU,
                         produce = allU, inspect = c(S, S, S, S), calibrate = allU, retrain = allU,
                         read = c(S, S, S, S), write = c(S, S, S, S), update = c(S, S, S, S)))
# The Methods package profile is Python/JS/Rust only; the others cover all four surfaces.
expected_surfaces <- list("native-methods-package" = surfaces[1:3])
for (id in names(expected)) {
  profile <- by_id[[id]]
  stopifnot(identical(unlist(profile$surfaces), if (is.null(expected_surfaces[[id]])) surfaces else expected_surfaces[[id]]),
            identical(names(profile$operations), operations))
  for (operation in operations) {
    cells <- profile$operations[[operation]]
    stopifnot(identical(names(cells), surfaces))
    statuses <- vapply(cells, `[[`, character(1), "status", USE.NAMES = FALSE)
    if (!identical(statuses, expected[[id]][[operation]]))
      stop(sprintf("%s.%s statuses differ from the exact declaration", id, operation), call. = FALSE)
    for (cell in cells) {
      if (identical(cell$status, U))
        stopifnot(is.character(cell$reason), nzchar(cell$reason), is.null(cell$entrypoints))
      else
        stopifnot(length(cell$entrypoints) >= 1L, is.null(cell$reason))
    }
  }
}

# Per-surface runtime requirements of the initial workflow and conformal profiles: the browser
# needs WASM builds only, Rust needs no CLI.
for (profile in profiles$profiles[1:2]) {
  req <- profile$runtime_requirements
  stopifnot(identical(names(req), surfaces),
            grepl("WASM builds", paste(unlist(req$javascript_wasm), collapse = " ")),
            grepl("no native library path and no CLI", paste(unlist(req$javascript_wasm), collapse = " ")),
            grepl("no CLI", paste(unlist(req$rust), collapse = " ")),
            grepl("nirs4all-core-archive CLI", paste(unlist(req$python), collapse = " ")),
            grepl("nirs4all-core-archive CLI", paste(unlist(req$matlab_octave), collapse = " ")))
}

# Content types. calibrated.v1 is the Core wrapper schema; the Methods package profile carries the
# DAG-ML scientific Package V2/V3 closure, and the canonical/container profiles carry the neutral Core V1 container.
workflow <- by_id[["native-workflow"]]
conformal <- by_id[["native-conformal"]]
types_of <- function(id, field) vapply(by_id[[id]]$content_types, `[[`, character(1), field)
stopifnot(
  identical(types_of("native-workflow", "schema"), "nirs4all.native-pipeline.v1"),
  identical(types_of("native-workflow", "owner"), "nirs4all-core"),
  identical(types_of("native-conformal", "schema"), "nirs4all.calibrated.v1"),
  identical(types_of("native-conformal", "owner"), "nirs4all-core"),
  identical(types_of("native-methods-package", "schema"),
            c("dagml.portable_predictor_package", "dagml.portable_refit_package")),
  identical(types_of("native-methods-package", "owner"), c("dag-ml", "dag-ml")),
  identical(types_of("native-n4a-canonical", "schema"), c("nirs4all.n4a.container.v1", "nirs4all.dataset.v1")),
  identical(types_of("native-n4a-canonical", "owner"), c("nirs4all-core", "nirs4all-io")),
  identical(types_of("n4a-container", "schema"), "nirs4all.n4a.container.v1"),
  identical(types_of("n4a-container", "owner"), "nirs4all-core"),
  # Package V2 is the scientific closure; the physical container is the owner-neutral Core V1 one.
  identical(conformal$content_types[[1L]]$carried_in,
            paste0("Core calibration wrapper JSON, only on the content_routes that name this schema: the Rust ",
                   "calibrate_archive return value, the CLI fit/load responses and the MATLAB struct. The JS/WASM host ",
                   "object and the Python host object carry no schema id; import/export transfer the native DAG Package V2 ",
                   "(scientific package schema2) carried in the owner-neutral Core V1 container (physical schema1) via ",
                   "native-methods-package@1")),
  length(conformal$content_types) == 1L, "dag_ml" %in% unlist(conformal$dependencies),
  identical(sort(unlist(conformal$dependencies)), c("dag_ml", "methods")),
  identical(unlist(workflow$dependencies), c("methods", "dag_ml", "io")),
  identical(unlist(by_id[["native-methods-package"]]$dependencies), c("dag_ml", "methods")),
  identical(unlist(by_id[["native-n4a-canonical"]]$dependencies), c("methods", "dag_ml", "io")),
  length(unlist(by_id[["n4a-container"]]$dependencies)) == 0L,
  length(workflow$content_routes) == 0L, length(conformal$content_routes) == 21L,
  all(vapply(by_id[c("native-methods-package", "native-n4a-canonical", "n4a-container")],
             function(profile) length(profile$content_routes) == 0L, logical(1))))
# Content is declared per route: JS/WASM never carries the wrapper schema id. Archive transfers carry
# the native scientific Package V2 (route label schema 2) over native-methods-package@1 and no wrapper schema.
wasm_routes <- Filter(function(r) identical(r$surface, "javascript_wasm"), conformal$content_routes)
transfers <- Filter(function(r) identical(r$route, "archive_transfer"), conformal$content_routes)
stopifnot(length(wasm_routes) == 5L, all(vapply(wasm_routes, function(r) is.null(r$schema), logical(1))),
          length(transfers) == 10L,
          all(vapply(transfers, function(r) identical(r$archive_schema_version, 2L) && is.null(r$schema) &&
                       identical(r$transport, "native-methods-package@1"), logical(1))),
          identical(unique(unlist(lapply(conformal$content_routes, `[[`, "schema"))), "nirs4all.calibrated.v1"))

# The API/Rd prose names all five declared profiles and never the removed ones.
if (nzchar(root)) {
  prose <- paste(c(readLines(file.path(root, "R", "execution_profiles.R")),
                   readLines(file.path(root, "man", "nirs4all_execution_profiles.Rd"))), collapse = "\n")
  for (retired in c("legacy", "baseline", "extension", "fixture-exact", "parity"))
    if (grepl(retired, prose, ignore.case = TRUE)) stop("stale profile prose: ", retired, call. = FALSE)
  for (id in profile_ids)
    if (!grepl(paste0(id, "@1"), prose, fixed = TRUE)) stop("prose omits ", id, call. = FALSE)
}

# Retired scopes are unreachable through the exposed data.
text <- as.character(jsonlite::toJSON(profiles, auto_unbox = TRUE))
for (retired in c("legacy", "wasm-methods-extension", "portable_pipeline", "runtime_contract",
                  "parity-validated", "conformal_store"))
  if (grepl(retired, text, fixed = TRUE)) stop("retired term reachable: ", retired, call. = FALSE)

# R is outside Core's runtime surfaces, references the five declared profiles, and its
# federated entrypoints are real exports.
federation <- profiles$external_federation
stopifnot(length(federation) == 1L)
federation <- federation[[1L]]
stopifnot(identical(federation$surface, "r"), identical(federation$core_runtime_surface, FALSE),
          identical(federation$status, "externally-federated"),
          identical(federation$package, "nirs4all"),
          !("r" %in% unlist(profiles$runtime_surfaces)),
          identical(unlist(federation$profiles), profile_ids))
symbols <- vapply(federation$entrypoints, `[[`, character(1), "symbol")
triples <- vapply(federation$entrypoints, function(entry)
  paste(entry$profile, entry$operation, entry$symbol, sep = ":"), character(1))
stopifnot(identical(triples, c(
  "native-workflow:fit:nirs4all_run_pipeline", "native-workflow:predict:nirs4all_pipeline_predict",
  "native-workflow:import:nirs4all_pipeline_load", "native-workflow:export:nirs4all_pipeline_export",
  "native-conformal:fit:nirs4all_calibrate", "native-conformal:predict:nirs4all_predict_calibrated",
  "native-conformal:import:nirs4all_load_calibrated", "native-conformal:export:nirs4all_export_calibrated",
  "native-n4a-canonical:produce:nirs4all_n4a_produce", "native-n4a-canonical:inspect:nirs4all_n4a_inspect",
  "native-n4a-canonical:predict:nirs4all_n4a_predict", "native-n4a-canonical:retrain:nirs4all_n4a_retrain",
  "n4a-container:inspect:nirs4all_n4a_inspect", "n4a-container:read:nirs4all_n4a_read",
  "n4a-container:write:nirs4all_n4a_write", "n4a-container:update:nirs4all_n4a_write")),
  length(symbols) == 16L)
missing_exports <- setdiff(symbols, exported)
if (length(missing_exports))
  stop("federated entrypoints are not exported: ", paste(missing_exports, collapse = ", "), call. = FALSE)
if (nzchar(root)) {
  paths <- c(unlist(federation$contracts), unlist(federation$evidence))
  absent <- paths[!file.exists(file.path(root, paths))]
  if (length(absent)) stop("federation points at missing files: ", paste(absent, collapse = ", "), call. = FALSE)
}

# Independent copies.
profiles$profiles[[1L]]$operations$fit$python$status <- "mutated"
profiles$schema <- "mutated"
fresh <- nirs4all_execution_profiles()
stopifnot(identical(fresh$schema, "nirs4all-core.execution-profiles.v1"),
          identical(fresh$profiles[[1L]]$operations$fit$python$status, "supported"))

cells_checked <- sum(vapply(profiles$profiles, function(profile)
  sum(vapply(profile$operations, length, integer(1))), integer(1)))
cat(sprintf("EXECUTED:execution-profiles cells=%d entrypoints=%d\n", cells_checked, length(symbols)))
