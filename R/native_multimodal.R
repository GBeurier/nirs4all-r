.nirs4all_multimodal_dataset_json <- function(dataset) {
  if (inherits(dataset, "nirs4all_dataset")) dataset <- dataset$record
  if (is.character(dataset) && length(dataset) == 1L) return(dataset)
  jsonlite::toJSON(dataset, auto_unbox = TRUE, digits = I(17L), null = "null")
}
.nirs4all_multimodal_object <- function(result, cli) {
  structure(list(native_json = attr(result, "native_json"), config = result$config,
    outcomes = lapply(result$target_models, function(entry) entry$model$training_outcome),
    cli = cli), class = "nirs4all_native_multimodal")
}

#' Run native multimodal CV with explicit source and missing-data policies
nirs4all_run_multimodal <- function(dataset, pipeline, source_policies,
    methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
    run_id = paste0("run:r:multimodal:", basename(tempfile())),
    cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive")) {
  policies <- jsonlite::toJSON(unname(source_policies), auto_unbox = TRUE, digits = I(17L))
  payload <- paste0('{"dataset":', .nirs4all_multimodal_dataset_json(dataset),
    ',"pipeline":', .nirs4all_pipeline_recipe_json(pipeline), ',"source_policies":', policies, '}')
  result <- .nirs4all_workflow_native_call("native-multimodal-run",
    list(methods_library = methods_library, run_id = run_id), .nirs4all_pipeline_record(payload), cli)
  .nirs4all_multimodal_object(result, cli)
}

#' Predict from a native multimodal model with target-free data and no fitting
nirs4all_multimodal_predict <- function(object, dataset,
    methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")), cli = object$cli) {
  stopifnot(inherits(object, "nirs4all_native_multimodal"))
  payload <- paste0('{"model":', object$native_json, ',"dataset":',
    .nirs4all_multimodal_dataset_json(dataset), '}')
  .nirs4all_workflow_native_call("native-multimodal-predict", list(methods_library = methods_library,
    run_id = paste0("run:r:multimodal:predict:", basename(tempfile()))), .nirs4all_pipeline_record(payload), cli)
}

#' Export exact native JSON without re-encoding integer fingerprints
nirs4all_multimodal_export <- function(object, path) {
  stopifnot(inherits(object, "nirs4all_native_multimodal"))
  .nirs4all_workflow_native_call("native-multimodal-export", list(destination = path),
    .nirs4all_pipeline_record(object$native_json), object$cli)
  invisible(path)
}

#' Load a native-validated multimodal model
nirs4all_multimodal_load <- function(path, cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive")) {
  result <- .nirs4all_workflow_native_call("native-multimodal-load", list(),
    .nirs4all_pipeline_record(paste(readLines(path, warn = FALSE), collapse = "\n")), cli)
  .nirs4all_multimodal_object(result, cli)
}

#' Retrain with captured recipe and explicit source policies in a fresh campaign
nirs4all_multimodal_retrain <- function(object, dataset,
    methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
    run_id = paste0("run:r:retrain:", basename(tempfile())), cli = object$cli) {
  stopifnot(inherits(object, "nirs4all_native_multimodal"))
  if (inherits(dataset, "nirs4all_dataset")) dataset <- dataset$record
  data_json <- if (is.character(dataset) && length(dataset) == 1L) dataset else
    jsonlite::toJSON(dataset, auto_unbox = TRUE, digits = I(17L), null = "null")
  payload <- paste0('{"model":', object$native_json, ',"dataset":', data_json, '}')
  result <- .nirs4all_workflow_native_call("native-multimodal-retrain",
    list(methods_library = methods_library, run_id = run_id), .nirs4all_pipeline_record(payload), cli)
  .nirs4all_multimodal_object(result, cli)
}
