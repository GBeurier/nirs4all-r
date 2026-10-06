.nirs4all_pipeline_recipe_json <- function(pipeline) {
  encode <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, digits = I(17L), null = "null")
  object <- function(x) {
    if (length(x) == 0L) return("{}")
    if (is.null(names(x)) || any(names(x) == "")) stop("Pipeline parameters must be named lists")
    encode(x)
  }
  if (!is.list(pipeline) || !setequal(names(pipeline), c("steps", "candidates")))
    stop("Pipeline requires exactly steps and candidates")
  steps <- vapply(pipeline$steps, function(step) {
    if (!is.list(step) || !all(names(step) %in% c("method_id", "role", "params")) ||
        !all(c("method_id", "role") %in% names(step))) stop("Invalid pipeline step")
    paste0('{"method_id":', encode(step$method_id), ',"role":', encode(step$role),
           ',"params":', object(step$params), '}')
  }, character(1))
  candidates <- vapply(pipeline$candidates, object, character(1))
  paste0('{"steps":[', paste(steps, collapse = ","), '],"candidates":[',
         paste(candidates, collapse = ","), ']}')
}

# Product transport over native DAG pipeline recipes; no numerical operations.
.nirs4all_pipeline_record <- function(value) structure(value, class = "nirs4all_native_json")

#' Run a catalog-native pipeline with CV, OOF selection and full refit
nirs4all_run_pipeline <- function(dataset, pipeline, source_id = "spectra",
    methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
    run_id = paste0("run:r:pipeline:", basename(tempfile())),
    cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive")) {
  if (inherits(dataset, "nirs4all_dataset")) dataset <- dataset$record
  result <- .nirs4all_workflow_native_call("pipeline-run", list(source_id = source_id,
    methods_library = methods_library, run_id = run_id), .nirs4all_pipeline_record(paste0('{"dataset":', jsonlite::toJSON(dataset, auto_unbox = TRUE, digits = I(17L), null = "null"),
      ',"pipeline":', .nirs4all_pipeline_recipe_json(pipeline), '}')), cli)
  structure(list(native_json = attr(result, "native_json"), config = result$config,
                 outcome = result$training_outcome, cli = cli), class = "nirs4all_native_pipeline")
}

#' Predict from a portable native pipeline without fitting
nirs4all_pipeline_predict <- function(object, X, sample_ids = NULL,
    methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
    cli = object$cli) {
  stopifnot(inherits(object, "nirs4all_native_pipeline"))
  X <- nirs4all_matrix(X)
  if (is.null(sample_ids)) sample_ids <- paste0("predict:", seq_len(nrow(X)))
  payload <- paste0('{"model":', object$native_json, ',"x":',
    jsonlite::toJSON(X, digits = I(17L)), ',"sample_ids":',
    jsonlite::toJSON(as.list(sample_ids), auto_unbox = TRUE), '}')
  .nirs4all_workflow_native_call("pipeline-predict", list(methods_library = methods_library,
    run_id = paste0("run:r:predict:", basename(tempfile()))), .nirs4all_pipeline_record(payload), cli)
}

#' Export the exact native JSON package without re-encoding its fingerprints
nirs4all_pipeline_export <- function(object, path) {
  stopifnot(inherits(object, "nirs4all_native_pipeline"))
  .nirs4all_workflow_native_call("pipeline-export", list(destination = path),
    .nirs4all_pipeline_record(object$native_json), object$cli)
  invisible(path)
}

#' Load a native-validated portable pipeline package
nirs4all_pipeline_load <- function(path,
    cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive")) {
  payload <- paste(readLines(path, warn = FALSE), collapse = "\n")
  result <- .nirs4all_workflow_native_call("pipeline-load", list(), .nirs4all_pipeline_record(payload), cli)
  structure(list(native_json = attr(result, "native_json"), config = result$config,
                 outcome = result$training_outcome, cli = cli), class = "nirs4all_native_pipeline")
}

#' Retrain a portable native pipeline in a fresh campaign
nirs4all_pipeline_retrain <- function(object, dataset,
    methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
    run_id = paste0("run:r:retrain:", basename(tempfile())), cli = object$cli) {
  stopifnot(inherits(object, "nirs4all_native_pipeline"))
  if (inherits(dataset, "nirs4all_dataset")) dataset <- dataset$record
  data_json <- if (is.character(dataset) && length(dataset) == 1L) dataset else
    jsonlite::toJSON(dataset, auto_unbox = TRUE, digits = I(17L), null = "null")
  payload <- paste0('{"model":', object$native_json, ',"dataset":', data_json, '}')
  result <- .nirs4all_workflow_native_call("pipeline-retrain",
    list(methods_library = methods_library, run_id = run_id), .nirs4all_pipeline_record(payload), cli)
  structure(list(native_json = attr(result, "native_json"), config = result$config,
    outcome = result$training_outcome, cli = cli), class = "nirs4all_native_pipeline")
}
