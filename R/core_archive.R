# Core owns ZIP/container validation. DAG-ML owns signed package replay and
# portable-artifact lifecycle. These R functions only transport native values.
.nirs4all_core_path <- function(value, label, executable = FALSE) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(value) || !file.exists(value) || dir.exists(value))
    stop(paste(label, "must identify an existing file"), call. = FALSE)
  value <- normalizePath(value, mustWork = TRUE)
  if (executable && file.access(value, 1L) != 0L)
    stop(paste(label, "must be executable"), call. = FALSE)
  value
}

.nirs4all_core_run <- function(cli, args, workdir) {
  stdout <- file.path(workdir, "native.stdout.log")
  stderr <- file.path(workdir, "native.stderr.log")
  status <- system2(cli, vapply(args, shQuote, character(1)),
                    stdout = stdout, stderr = stderr)
  if (!identical(status, 0L)) {
    detail <- if (file.exists(stderr)) paste(readLines(stderr, warn = FALSE),
                                            collapse = "\n") else ""
    stop(paste0("Native Core/DAG archive replay refused (", status, "): ", detail),
         call. = FALSE)
  }
  invisible(NULL)
}

.nirs4all_core_workdir <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value))
    stop("workdir must be a nonempty directory path", call. = FALSE)
  if (dir.exists(value) && length(list.files(value, all.files = TRUE, no.. = TRUE)))
    stop("workdir must be new or empty", call. = FALSE)
  if (!dir.exists(value) && !dir.create(value, recursive = TRUE))
    stop("could not create workdir", call. = FALSE)
  normalizePath(value, mustWork = TRUE)
}

.nirs4all_core_contract <- function(value, path, label) {
  if (is.character(value) && length(value) == 1L) {
    source <- .nirs4all_core_path(value, label)
    if (!file.copy(source, path, overwrite = FALSE))
      stop(paste("could not copy", label), call. = FALSE)
  } else if (is.list(value)) {
    json <- jsonlite::toJSON(value, auto_unbox = TRUE, null = "null",
                            digits = I(17L), force = TRUE)
    writeLines(json, path, useBytes = TRUE)
  } else {
    stop(paste(label, "must be a native JSON file or contract list"), call. = FALSE)
  }
  path
}

#' Open and validate a Core ZIP archive using the native Core reader
#'
#' Accepts Core Archive V2/V3 `.n4a` files. The native reader validates container
#' limits, inventory closure, payload hashes and archive references. R does not
#' unzip members or deserialize fitted state. A loaded archive keeps its original
#' raw SHA; prediction revalidates the file against that identity.
#' @param path Path to a Core `.n4a` archive.
#' @param core_cli Installed `nirs4all-core-archive` executable.
#' @return An immutable-by-contract archive reference and validated inventory.
#' @export
nirs4all_core_archive <- function(path, core_cli = Sys.which("nirs4all-core-archive")) {
  core_cli <- .nirs4all_core_path(core_cli, "core_cli", executable = TRUE)
  path <- .nirs4all_core_path(path, "archive")
  directory <- tempfile("nirs4all-core-inspect-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  output <- file.path(directory, "inventory.json")
  .nirs4all_core_run(core_cli, c("inspect", "--archive", path, "--output", output), directory)
  inventory <- jsonlite::fromJSON(output, simplifyVector = FALSE)
  if (!identical(inventory$schema_version, 1L) ||
      !(inventory$archive$schema_version %in% c(2L, 3L)))
    stop("R native archive replay requires a Core Archive V2 or V3", call. = FALSE)
  structure(list(path = path, archive = inventory$archive,
                 manifest = inventory$manifest, core_cli = core_cli),
            class = "nirs4all_core_archive")
}

#' Replay a Core Archive V2/V3 from R with native DAG process callbacks
#'
#' Core revalidates the original archive and passes its exact signed package
#' bytes to DAG-ML. DAG-ML verifies independent controller trust, the signed
#' replay request and prediction-only envelopes before starting the R adapter.
#' It hydrates artifacts, executes PREDICT and releases state through the existing
#' process protocol. No FIT, REFIT, HPO or target access is permitted on this route.
#'
#' This is a native-contract interface: requests and envelopes must be produced
#' by DAG-ML, not inferred from an archive. Python is not required. The installed
#' raw multimodal adapter supports the closed U07 native scaler/PCA/mixed-column
#' plus Ridge profile; other supported installed process adapters may be supplied.
#' @param archive An object returned by [nirs4all_core_archive()].
#' @param request Signed native TrainingReplayRequest (V2) or the native V3
#'   portable-refit replay request, as a list or exact JSON file.
#' @param data_envelopes Native JSON map of current prediction envelopes, as a
#'   list or exact JSON file. Target-free PREDICT contracts are required.
#' @param adapter An executable persistent R process adapter, or the list
#'   returned by [nirs4all_core_multimodal_adapter()] or
#'   [nirs4all_dag_role_adapter()] prepared with `allow_fit = FALSE`.
#' @param trusted_controllers Independently installed native manifests, as a
#'   list or exact JSON file. Trust must not be copied from archive contents.
#' @param dag_cli Installed `dag-ml-cli` executable.
#' @param workdir New or empty directory; retains contracts, output and logs.
#' @param outcome_id Native replay outcome ID.
#' @param run_id Native replay run ID.
#' @param process_timeout_ms Optional native worker timeout; zero is unbounded.
#' @return Native replay outcome, with an additional `workdir` path.
#' @export
nirs4all_core_archive_predict <- function(archive, request, data_envelopes,
    adapter, trusted_controllers, dag_cli = Sys.which("dag-ml-cli"),
    workdir = tempfile("nirs4all-core-replay-"),
    outcome_id = "outcome:nirs4all-r.core.replay", run_id = "run:nirs4all-r.core.replay",
    process_timeout_ms = 0) {
  if (!inherits(archive, "nirs4all_core_archive") ||
      !(archive$archive$schema_version %in% c(2L, 3L)) ||
      !is.character(archive$archive$archive_sha256) ||
      length(archive$archive$archive_sha256) != 1L ||
      !grepl("^[0-9a-f]{64}$", archive$archive$archive_sha256))
    stop("archive must be a validated Core Archive V2/V3 reference", call. = FALSE)
  core_cli <- .nirs4all_core_path(archive$core_cli, "core_cli", executable = TRUE)
  dag_cli <- .nirs4all_core_path(dag_cli, "dag_cli", executable = TRUE)
  if (is.list(adapter)) {
    if (!is.null(adapter$allow_fit) && !identical(adapter$allow_fit, FALSE))
      stop("archive replay requires an adapter with fitting disabled", call. = FALSE)
    adapter <- adapter$adapter
  }
  adapter <- .nirs4all_core_path(adapter, "R process adapter", executable = TRUE)
  if (!is.numeric(process_timeout_ms) || length(process_timeout_ms) != 1L ||
      !is.finite(process_timeout_ms) || process_timeout_ms < 0 ||
      process_timeout_ms != floor(process_timeout_ms))
    stop("process_timeout_ms must be a nonnegative integer", call. = FALSE)
  for (identifier in list(outcome_id, run_id))
    if (!is.character(identifier) || length(identifier) != 1L || is.na(identifier) ||
        !nzchar(identifier)) stop("run/outcome IDs must be nonempty strings", call. = FALSE)
  workdir <- .nirs4all_core_workdir(workdir)
  request_path <- .nirs4all_core_contract(request, file.path(workdir, "request.json"), "replay request")
  envelopes_path <- .nirs4all_core_contract(data_envelopes, file.path(workdir, "envelopes.json"), "envelopes")
  controllers_path <- .nirs4all_core_contract(trusted_controllers,
    file.path(workdir, "trusted-controllers.json"), "trusted controller manifests")
  output <- file.path(workdir, "outcome.json")
  .nirs4all_core_run(core_cli, c("replay-process", "--archive", archive$path,
    "--expected-archive-sha256", archive$archive$archive_sha256, "--dag-cli", dag_cli,
    "--request", request_path, "--envelopes", envelopes_path,
    "--trusted-controllers", controllers_path, "--adapter", adapter, "--output", output,
    "--outcome-id", outcome_id, "--run-id", run_id,
    "--process-timeout-ms", format(process_timeout_ms, scientific = FALSE)), workdir)
  outcome <- jsonlite::fromJSON(output, simplifyVector = FALSE)
  outcome$workdir <- workdir
  outcome
}

#' Replay native N4MM, N4ME or RolePipeline archives directly through Core
#'
#' Uses Core's existing native Methods replay entry points for Archive V2/V3.
#' DAG-ML validates and schedules the original portable package; Methods imports
#' and predicts the supported native state. No process adapter, Python, fitting,
#' host-sidecar hydration or target access is involved. This is the direct route
#' for archives produced by the native Methods controllers; the raw U07 process
#' profile remains available through [nirs4all_core_archive_predict()].
#' @param archive An object returned by [nirs4all_core_archive()].
#' @param request Signed native TrainingReplayRequest list or exact JSON file.
#' @param data_envelopes Native map of target-free prediction envelopes, as a list
#'   or exact JSON file.
#' @param methods_inputs Native map of numerical input contracts, as a list or
#'   exact JSON file. Each input has explicit ordered `sample_ids`, matrix `x`,
#'   and ordered `target_names`; `y` must be absent or NULL.
#' @param methods_library Exact native libn4m file.
#' @param methods_library_sha256 Independently attested lowercase SHA-256 of that
#'   library. Core verifies its private loaded snapshot and Methods ABI.
#' @param workdir New or empty directory for contracts and native output/logs.
#' @param outcome_id Native replay outcome ID.
#' @param run_id Native replay run ID.
#' @return Native replay outcome with an additional `workdir` path.
#' @export
nirs4all_core_archive_predict_methods <- function(archive, request, data_envelopes,
    methods_inputs, methods_library, methods_library_sha256,
    workdir = tempfile("nirs4all-core-methods-"),
    outcome_id = "outcome:nirs4all-r.core.methods", run_id = "run:nirs4all-r.core.methods") {
  if (!inherits(archive, "nirs4all_core_archive") ||
      !(archive$archive$schema_version %in% c(2L, 3L)) ||
      !is.character(archive$archive$archive_sha256) ||
      length(archive$archive$archive_sha256) != 1L ||
      !grepl("^[0-9a-f]{64}$", archive$archive$archive_sha256))
    stop("archive must be a validated Core Archive V2/V3 reference", call. = FALSE)
  core_cli <- .nirs4all_core_path(archive$core_cli, "core_cli", executable = TRUE)
  library <- .nirs4all_core_path(methods_library, "Methods library")
  if (!is.character(methods_library_sha256) || length(methods_library_sha256) != 1L ||
      is.na(methods_library_sha256) || !grepl("^[0-9a-f]{64}$", methods_library_sha256))
    stop("methods_library_sha256 must be an independently attested lowercase SHA-256", call. = FALSE)
  if (is.list(methods_inputs) && any(vapply(methods_inputs, function(input)
      is.list(input) && !is.null(input$y), logical(1))))
    stop("native archive replay refuses target values in methods_inputs", call. = FALSE)
  if (is.list(data_envelopes) && any(vapply(data_envelopes, function(envelope)
      is.list(envelope) && (!is.null(envelope$target_content_fingerprint) ||
        (!is.null(envelope$predict_cohort) &&
         !is.null(envelope$predict_cohort$target_content_fingerprint))), logical(1))))
    stop("native archive replay requires target-free prediction envelopes", call. = FALSE)
  for (identifier in list(outcome_id, run_id))
    if (!is.character(identifier) || length(identifier) != 1L || is.na(identifier) ||
        !nzchar(identifier)) stop("run/outcome IDs must be nonempty strings", call. = FALSE)
  workdir <- .nirs4all_core_workdir(workdir)
  request_path <- .nirs4all_core_contract(request, file.path(workdir, "request.json"), "replay request")
  envelopes_path <- .nirs4all_core_contract(data_envelopes, file.path(workdir, "envelopes.json"), "envelopes")
  inputs_path <- .nirs4all_core_contract(methods_inputs, file.path(workdir, "methods-inputs.json"), "Methods inputs")
  output <- file.path(workdir, "outcome.json")
  .nirs4all_core_run(core_cli, c("replay-methods", "--archive", archive$path,
    "--expected-archive-sha256", archive$archive$archive_sha256,
    "--request", request_path, "--envelopes", envelopes_path, "--methods-inputs", inputs_path,
    "--methods-library", library, "--methods-library-sha256", methods_library_sha256,
    "--output", output, "--outcome-id", outcome_id, "--run-id", run_id), workdir)
  outcome <- jsonlite::fromJSON(output, simplifyVector = FALSE)
  outcome$workdir <- workdir
  outcome
}

#' Prepare the installed raw multimodal native replay adapter
#'
#' The adapter preserves the original Python/R/WASM/Octave Methods producer
#' identity while running numerical predictions in R's installed n4m. It accepts
#' the closed U07 four-source declarations (`nir`, `image`, `series`, `metadata`)
#' and their scaler, tensor-PCA, mixed-column and Ridge recipes. This function
#' prepares buffers and transport only; the adapter and natives validate schemas,
#' recipes, state bytes and selected parameters. It cannot fit or read targets.
#' @param sources Named raw source contracts. Each includes `sample_ids` and
#'   `descriptor`; numeric sources also have row-major `data` and `shape`, while
#'   metadata has explicit two-column `rows`.
#' @param operators Current installed map of node IDs to native operators.
#' @param node_params Current effective parameters by node ID.
#' @param source_ids Ordered native source IDs corresponding to the four sources.
#' @param controller_manifest Expected signed producer manifest.
#' @param trusted_manifest Independently installed manifest for that producer.
#' @param target_names Ordered target names; this profile has exactly one target.
#' @param workdir New or empty directory for transport and lifecycle log.
#' @return Persistent process adapter paths and native lifecycle audit path.
#' @export
nirs4all_core_multimodal_adapter <- function(sources, operators, node_params,
    source_ids, controller_manifest, trusted_manifest, target_names = "y",
    workdir = tempfile("nirs4all-core-r-adapter-")) {
  if (.Platform$OS.type == "windows")
    stop("The installed native raw R process adapter currently requires POSIX", call. = FALSE)
  .nirs4all_role_require(is.list(sources) &&
    identical(sort(names(sources)), sort(c("nir", "image", "series", "metadata"))),
    "The native raw adapter requires the four explicit U07 source declarations")
  .nirs4all_role_require(.nirs4all_role_equal(controller_manifest, trusted_manifest),
    "Signed producer manifest differs from the independently trusted manifest")
  .nirs4all_role_require(is.character(controller_manifest$controller_id) &&
    controller_manifest$controller_id %in% paste0("controller:methods.",
      c("python", "wasm", "r", "octave"), ".multimodal"), "Unsupported native producer owner")
  source_ids <- .nirs4all_role_ids(source_ids, "Native source IDs")
  target_names <- .nirs4all_role_ids(target_names, "Target names")
  .nirs4all_role_require(length(source_ids) == 4L && length(target_names) == 1L,
    "The native raw adapter requires four source IDs and one target")
  operators <- .nirs4all_role_map(operators, "Current native operators", TRUE)
  node_params <- .nirs4all_role_map(node_params, "Current node parameters")
  workdir <- .nirs4all_core_workdir(workdir)
  config <- list(sources = sources[c("nir", "image", "series", "metadata")],
    operators = operators, node_params = node_params, source_ids = as.list(source_ids),
    targets = NULL, target_names = as.list(target_names), allow_fit = FALSE,
    controller_id = controller_manifest$controller_id, manifest = controller_manifest,
    trusted_manifest = trusted_manifest, audit_path = file.path(workdir, "lifecycle.jsonl"))
  config_path <- .nirs4all_core_contract(config, file.path(workdir, "config.json"), "native adapter config")
  script <- system.file("adapters", "nirs4all_core_multimodal_adapter.R", package = "nirs4all", mustWork = TRUE)
  rscript <- .nirs4all_core_path(file.path(R.home("bin"), "Rscript"), "Rscript", executable = TRUE)
  adapter <- file.path(workdir, "run-r-core-multimodal-adapter")
  writeLines(c("#!/bin/sh", paste0("export DAGML_METHODS_MULTIMODAL_CONFIG=", shQuote(config_path)),
    paste0("exec ", shQuote(rscript), " ", shQuote(script), " \"$@\"")), adapter)
  Sys.chmod(adapter, "0755")
  list(adapter = adapter, manifest = controller_manifest, trusted_manifest = trusted_manifest,
       workdir = workdir, audit_path = config$audit_path, allow_fit = FALSE)
}
