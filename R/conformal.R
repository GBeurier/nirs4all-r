# Native calibrated archives are shared with the Rust/Python/WASM aggregate.
nirs4all_calibrate <- function(model, data, coverages = c(0.9),
                               small_sample_policy = "error", source_id = "spectra",
                               archive = tempfile(fileext = ".n4a"),
                               methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
                               cli = Sys.getenv("NIRS4ALL_CORE_CLI")) {
  if (missing(cli) && is.list(model) && !is.null(model$cli)) cli <- model$cli
  cli <- .nirs4all_native_cli(cli)
  original <- if (is.character(model)) model else model$archive
  if (is.null(original)) stop("model requires a native archive", call. = FALSE)
  result <- .nirs4all_workflow_native_call("conformal-calibrate", list(
    archive = normalizePath(original, mustWork = TRUE), destination = archive,
    source_id = source_id, methods_library = normalizePath(methods_library, mustWork = TRUE),
    run_id = paste0("run:calibrate:", basename(tempfile())),
    coverages = jsonlite::toJSON(unname(as.numeric(coverages)), auto_unbox = FALSE, digits = I(17L)),
    small_sample_policy = jsonlite::toJSON(small_sample_policy, auto_unbox = TRUE)),
    record = if (inherits(data, "nirs4all_dataset")) data$record else nirs4all_dataset(data, core_cli = cli)$record, cli = cli)
  structure(list(archive = normalizePath(archive, mustWork = TRUE),
                 calibration = result$calibration, cli = cli), class = "nirs4all_calibrated",
            native_json = attr(result, "native_json"))
}

.nirs4all_uncertainty_rows <- function(X) {
  X <- as.matrix(X)
  lapply(seq_len(nrow(X)), function(i) as.list(unname(X[i, ])))
}

.nirs4all_uncertainty_input <- function(X, sample_ids, source_id, cli) {
  if (is.data.frame(X)) X <- as.matrix(X)
  structured <- inherits(X, "nirs4all_dataset") || is.list(X) || is.character(X)
  if (structured) {
    if (!is.null(sample_ids)) stop("structured uncertainty uses Dataset sample IDs; omit sample_ids", call. = FALSE)
    data <- if (inherits(X, "nirs4all_dataset")) X$record else nirs4all_dataset(X, core_cli = cli)$record
    result <- list(dataset = data)
    if (!is.null(source_id)) result$source_id <- source_id
    result
  } else {
    if (is.null(sample_ids)) stop("positional uncertainty requires sample_ids", call. = FALSE)
    if (!is.null(source_id)) stop("source_id requires a structured Dataset", call. = FALSE)
    list(x = .nirs4all_uncertainty_rows(X), sample_ids = as.list(unname(as.character(sample_ids))))
  }
}

nirs4all_predict_calibrated <- function(model, X, sample_ids = NULL,
                                        methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
                                        cli = Sys.getenv("NIRS4ALL_CORE_CLI"), source_id = NULL) {
  if (missing(cli) && is.list(model) && !is.null(model$cli)) cli <- model$cli
  archive <- if (is.character(model)) model else model$archive
  .nirs4all_workflow_native_call("conformal-predict", list(
    archive = normalizePath(archive, mustWork = TRUE),
    methods_library = normalizePath(methods_library, mustWork = TRUE),
    run_id = paste0("run:calibrated:predict:", basename(tempfile()))),
    record = .nirs4all_uncertainty_input(X, sample_ids, source_id, cli), cli = cli)
}

nirs4all_conformal_metrics <- function(model, prediction, y, sample_ids,
                                      cli = Sys.getenv("NIRS4ALL_CORE_CLI")) {
  if (missing(cli) && !is.null(model$cli)) cli <- model$cli
  calibration_json <- attr(model, "native_json")
  prediction_json <- attr(prediction, "native_json")
  if (is.null(calibration_json) || is.null(prediction_json))
    stop("metrics requires untouched native calibration and prediction JSON", call. = FALSE)
  .nirs4all_workflow_native_call("conformal-metrics", list(),
    record = list(calibration_json = calibration_json, prediction_json = prediction_json,
                  truth = list(sample_ids = as.list(unname(as.character(sample_ids))),
                               values = .nirs4all_uncertainty_rows(y))), cli = cli)
}

nirs4all_export_calibrated <- function(model, path) {
  if (file.exists(path)) stop("destination exists", call. = FALSE)
  if (!file.copy(model$archive, path, overwrite = FALSE)) stop("archive copy failed", call. = FALSE)
  invisible(path)
}

nirs4all_load_calibrated <- function(path, cli = Sys.getenv("NIRS4ALL_CORE_CLI")) {
  cli <- .nirs4all_native_cli(cli)
  path <- normalizePath(path, mustWork = TRUE)
  result <- .nirs4all_workflow_native_call("conformal-load", list(archive = path), cli = cli)
  structure(list(archive = path, calibration = result$calibration, cli = cli), class = "nirs4all_calibrated",
            native_json = attr(result, "native_json"))
}
