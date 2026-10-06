# IO owns raw dataset validation/alignment; Core exposes the native IO command.
.nirs4all_dataset_native <- function(value, command, core_cli) {
  core_cli <- .nirs4all_native_cli(core_cli)
  directory <- tempfile("nirs4all-dataset-"); dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  input <- file.path(directory, "input.json"); output <- file.path(directory, "output.json")
  if (inherits(value, "nirs4all_dataset")) value <- value$record
  if (is.character(value) && length(value) == 1L && file.exists(value)) {
    if (!file.copy(value, input)) stop("Cannot copy dataset input", call. = FALSE)
  } else if (is.list(value)) {
    writeLines(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", digits = I(17L), force = TRUE), input, useBytes = TRUE)
  } else stop("Dataset must be an IO record or a JSON file", call. = FALSE)
  .nirs4all_core_run(core_cli, c(command, "--input", input, "--output", output), directory)
  jsonlite::fromJSON(output, simplifyVector = FALSE)
}

#' Construct a public raw dataset through native IO
#' @export
nirs4all_dataset <- function(value, core_cli = Sys.which("nirs4all-core-archive")) {
  if (missing(core_cli) && inherits(value, "nirs4all_dataset")) core_cli <- value$core_cli
  core_cli <- .nirs4all_native_cli(core_cli)
  record <- .nirs4all_dataset_native(value, "dataset-normalize", core_cli)
  structure(list(record = record, core_cli = normalizePath(core_cli, mustWork = TRUE)), class = "nirs4all_dataset")
}

.nirs4all_mm_schemas <- function(current, saved, core_cli) {
  .nirs4all_dataset_native(list(current = current, saved = saved), "dataset-compatible-schemas", core_cli)
}
.nirs4all_mm_blocks <- function(raw) {
  lapply(raw$sources, function(source) {
    if (!is.null(source$rows)) {
      result <- matrix(vector("list", 2L * length(source$rows)), ncol = 2L)
      for (i in seq_along(source$rows)) { result[[i, 1L]] <- source$rows[[i]][[1L]]; result[[i, 2L]] <- source$rows[[i]][[2L]] }
      # n4m's mixed table binding accepts a character matrix and converts only
      # the declared numeric column; category bytes stay untouched.
      return(matrix(vapply(result, function(value) if (is.numeric(value)) sprintf("%.17g", value) else as.character(value), character(1)), nrow = nrow(result)))
    }
    shape <- as.integer(unlist(source$shape, use.names = FALSE))
    aperm(array(as.double(unlist(source$data, use.names = FALSE)), dim = rev(shape)), rev(seq_along(shape)))
  })
}
.nirs4all_mm_predictor <- function(native, recipe, schemas, target_names, core_cli) {
  structure(list(native = native, recipe = recipe, source_schemas = schemas,
                 target_names = target_names, core_cli = core_cli), class = "nirs4all_multimodal_predictor")
}

#' Fit one complete native raw multimodal predictor
#' @export
nirs4all_multimodal_fit <- function(recipe, dataset, core_cli = Sys.which("nirs4all-core-archive")) {
  if (missing(core_cli) && inherits(dataset, "nirs4all_dataset")) core_cli <- dataset$core_cli
  ds <- nirs4all_dataset(dataset, core_cli); record <- ds$record$dataset
  core_cli <- ds$core_cli
  y <- as.double(unlist(record$y$values, use.names = FALSE))
  if (is.null(record$y) || !record$y$dtype %in% c("float32", "float64", "int8", "int16", "int32", "int64", "uint8", "uint16", "uint32", "uint64") || length(record$y$shape) != 1L || length(y) != length(record$sample_ids) ||
      any(!is.finite(y)) || !all(unlist(record$target_mask$values)) || any(unlist(record$partitions$values) != "train"))
    stop("Full multimodal fit requires training rows and one finite observed numeric target", call. = FALSE)
  raw <- .nirs4all_dataset_native(ds, "dataset-u07-sources", core_cli)
  native <- n4m::n4m_multimodal_pipeline(recipe, raw$source_schemas)
  retained <- FALSE; on.exit(if (!retained) n4m::n4m_close(native), add = TRUE)
  n4m::n4m_fit(native, .nirs4all_mm_blocks(raw), matrix(y, ncol = 1L))
  retained <- TRUE
  .nirs4all_mm_predictor(native, recipe, raw$source_schemas, unlist(record$target_names), core_cli)
}

#' Predict from a target-free raw cohort
#' @export
predict.nirs4all_multimodal_predictor <- function(object, newdata, ...) {
  ds <- nirs4all_dataset(newdata, object$core_cli); record <- ds$record$dataset
  if (!is.null(record$y) || any(unlist(record$partitions$values) != "predict")) stop("Prediction requires a target-free predict cohort", call. = FALSE)
  raw <- .nirs4all_dataset_native(ds, "dataset-u07-sources", object$core_cli)
  schemas <- .nirs4all_mm_schemas(raw$source_schemas, object$source_schemas, object$core_cli)
  values <- as.double(stats::predict(object$native, .nirs4all_mm_blocks(raw), source_schemas = schemas))
  list(sample_ids = unlist(record$sample_ids), target_names = object$target_names, values = matrix(values, ncol = 1L))
}

#' Export complete native N4MF state and its IO contracts
#' @export
nirs4all_multimodal_export <- function(object, path = NULL) {
  record <- list(schema = "nirs4all.multimodal-predictor.v1", schema_version = 1L,
    recipe = object$recipe, source_schemas = object$source_schemas,
    state = as.list(as.integer(n4m::n4m_export_state(object$native))), target_names = as.list(object$target_names))
  if (is.null(path)) return(record)
  if (file.exists(path)) stop("Predictor output already exists", call. = FALSE)
  writeLines(jsonlite::toJSON(record, auto_unbox = TRUE, null = "null", digits = I(17L), force = TRUE), path, useBytes = TRUE)
  invisible(path)
}

#' Load complete native state without fitting or reading a training workspace
#' @export
nirs4all_multimodal_load <- function(value, core_cli = Sys.which("nirs4all-core-archive")) {
  core_cli <- .nirs4all_native_cli(core_cli)
  record <- if (is.character(value) && length(value) == 1L) jsonlite::fromJSON(value, simplifyVector = FALSE) else value
  keys <- c("schema", "schema_version", "recipe", "source_schemas", "state", "target_names")
  state <- unlist(record$state, use.names = FALSE); targets <- unlist(record$target_names, use.names = FALSE)
  if (!is.list(record) || !identical(sort(names(record)), sort(keys)) ||
      !identical(record$schema, "nirs4all.multimodal-predictor.v1") || !identical(record$schema_version, 1L) ||
      !is.numeric(state) || !length(state) || length(state) > 67108864L || any(!is.finite(state) | state != floor(state) | state < 0 | state > 255) ||
      !is.character(targets) || length(targets) != 1L || !nzchar(targets)) stop("Invalid multimodal predictor envelope", call. = FALSE)
  native <- n4m::n4m_multimodal_pipeline_from_state(as.raw(state), record$recipe, record$source_schemas)
  .nirs4all_mm_predictor(native, record$recipe, record$source_schemas, targets, core_cli)
}
