# Common Archive V2 workflow transport; every computation stays native.
.nirs4all_native_bundle <- function(archive, directory, metadata, name, cli) {
  if (!is.character(directory) || length(directory) != 1L || is.na(directory) ||
      !nzchar(directory) || file.exists(directory))
    stop("export directory must be a new path", call. = FALSE)
  parent <- dirname(directory)
  if (!dir.exists(parent) && !dir.create(parent, recursive = TRUE))
    stop("cannot create export parent directory", call. = FALSE)
  stage <- tempfile(".nirs4all-export-", tmpdir = parent)
  if (!dir.create(stage)) stop("cannot stage export", call. = FALSE)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  if (!file.copy(archive, file.path(stage, "model.n4a")))
    stop("archive copy failed", call. = FALSE)
  jsonlite::write_json(metadata, file.path(stage, name), auto_unbox = TRUE,
                       null = "null", digits = I(17L))
  .nirs4all_workflow_native_call("publish-directory", list(input = stage,
    destination = directory), cli = cli)
  invisible(directory)
}

.nirs4all_workflow_native_call <- function(operation, flags, record = NULL,
    cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive")) {
  cli <- .nirs4all_native_cli(cli)
  directory <- tempfile("nirs4all-native-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  output <- file.path(directory, "output.json")
  args <- c(operation, "--output", output)
  if (!is.null(record)) {
    input <- file.path(directory, "input.json")
    writeLines(jsonlite::toJSON(record, auto_unbox = TRUE, null = "null",
                               digits = I(17L), force = TRUE), input)
    args <- c(args, "--input", input)
  }
  for (name in names(flags)) {
    if (!is.null(flags[[name]]))
      args <- c(args, paste0("--", gsub("_", "-", name, fixed = TRUE)),
                as.character(flags[[name]]))
  }
  status <- system2(cli, shQuote(args), stdout = file.path(directory, "log"),
                    stderr = file.path(directory, "log"))
  if (!identical(status, 0L) || !file.exists(output))
    stop(paste(readLines(file.path(directory, "log"), warn = FALSE),
               collapse = "\n"), call. = FALSE)
  native_json <- paste(readLines(output, warn = FALSE), collapse = "\n")
  result <- jsonlite::fromJSON(native_json, simplifyVector = FALSE, bigint_as_char = TRUE)
  attr(result, "native_json") <- native_json
  result
}

#' Run the common native CV/OOF/refit Archive V2 profile
nirs4all_native_run <- function(dataset, archive, source_id = "spectra",
    components = c(1L, 2L), methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
    run_id = paste0("run:r:", basename(tempfile())),
    results_directory = paste0(archive, ".results"),
    cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive")) {
  if (inherits(dataset, "nirs4all_dataset")) {
    if (missing(cli)) cli <- dataset$core_cli
    dataset <- dataset$record
  }
  cli <- .nirs4all_native_cli(cli)
  outcome <- .nirs4all_workflow_native_call("workflow-run", list(
    source_id = source_id, components = jsonlite::toJSON(as.list(unname(components)), auto_unbox = TRUE),
    methods_library = methods_library, archive = archive, run_id = run_id,
    results_directory = results_directory), dataset, cli)
  structure(list(archive = normalizePath(outcome$model_archive, mustWork = TRUE), outcome = outcome,
                 config = outcome$config, cli = cli),
            class = "nirs4all_native_workflow")
}

#' Predict from a common native archive without fitting
nirs4all_native_predict <- function(object, X, sample_ids = NULL,
    methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
    cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive")) {
  archive <- if (inherits(object, "nirs4all_native_workflow")) object$archive else object
  if (missing(cli) && inherits(object, "nirs4all_native_workflow")) cli <- object$cli
  X <- nirs4all_matrix(X)
  if (is.null(sample_ids)) sample_ids <- paste0("predict:", seq_len(nrow(X)))
  .nirs4all_workflow_native_call("workflow-predict", list(
    archive = archive, methods_library = methods_library,
    run_id = paste0("run:r:predict:", basename(tempfile()))),
    list(x = lapply(seq_len(nrow(X)), function(i) as.list(unname(X[i, ]))),
         sample_ids = as.list(unname(sample_ids))), cli)
}

#' Export a common native workflow
nirs4all_native_export <- function(object, directory) {
  if (!inherits(object, "nirs4all_native_workflow")) stop("expected native workflow")
  .nirs4all_native_bundle(object$archive, directory,
    list(schema = "nirs4all.workflow.v1", config = object$config,
         training_outcome_fingerprint = object$outcome$training_outcome$outcome_fingerprint),
    "workflow.json", object$cli)
}

#' Reload native-validated outcome and configuration
nirs4all_native_load <- function(directory,
    cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive")) {
  cli <- .nirs4all_native_cli(cli)
  archive <- normalizePath(file.path(directory, "model.n4a"), mustWork = TRUE)
  saved <- jsonlite::fromJSON(file.path(directory, "workflow.json"), simplifyVector = FALSE)
  outcome <- .nirs4all_workflow_native_call("workflow-load", list(archive = archive), cli = cli)
  # Older wrappers may add descriptive numeric_storage. Authoritative choices
  # are always taken from the validated native archive.
  config <- saved$config[c("source_id", "components", "preprocessing")]
  if (!identical(saved$schema, "nirs4all.workflow.v1") ||
      !identical(sort(names(config)), sort(names(outcome$config))) ||
      !isTRUE(all.equal(config[sort(names(config))], outcome$config[sort(names(outcome$config))],
                       tolerance = 0, check.attributes = FALSE)) ||
      !identical(saved$training_outcome_fingerprint, outcome$training_outcome$outcome_fingerprint))
    stop("workflow metadata differs from its native archive")
  structure(list(archive = archive, outcome = outcome, config = outcome$config, cli = cli),
            class = "nirs4all_native_workflow")
}

#' Retrain the native profile on a replacement dataset
nirs4all_native_retrain <- function(object, dataset, archive, ...,
    cli = object$cli) {
  if (!inherits(object, "nirs4all_native_workflow")) stop("expected native workflow")
  nirs4all_native_run(dataset, archive, source_id = object$config$source_id,
                     components = unlist(object$config$components), ..., cli = cli)
}

#' Save native results and their optional portable model
nirs4all_save_experiment <- function(native_directory, destination,
    model_archive = NULL, cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive")) {
  .nirs4all_workflow_native_call("experiment-save", list(
    input = native_directory, destination = destination, archive = model_archive), cli = cli)
  nirs4all_open_experiment(destination, cli)
}
