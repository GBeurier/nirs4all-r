#' Save a fitted pipeline
#' @param object Fitted pipeline.
#' @param file RDS path. n4m model state is stored as portable N4MM bytes.
#' @export
nirs4all_save <- function(object, file) {
  if (!inherits(object, "nirs4all_fitted"))
    stop("object must be a fitted nirs4all pipeline", call. = FALSE)
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file))
    stop("file must be a non-empty path", call. = FALSE)
  saved <- object
  if (identical(saved$learner$format, "n4mm"))
    saved$state <- n4m::n4m_model_export(object$state)
  if (identical(saved$learner$format, "torch-r")) {
    if (!requireNamespace("torch", quietly = TRUE))
      stop("torch is required to save this model", call. = FALSE)
    saved$state$module <- torch::torch_serialize(object$state$module)
  }
  saveRDS(list(schema = 1L, fitted = saved), file)
  invisible(file)
}

#' Load a fitted pipeline
#' @param file RDS path previously written by [nirs4all_save()].
#' @export
nirs4all_load <- function(file) {
  saved <- readRDS(file)
  if (!is.list(saved) || !identical(saved$schema, 1L) ||
      !inherits(saved$fitted, "nirs4all_fitted"))
    stop("unsupported nirs4all model bundle", call. = FALSE)
  fitted <- saved$fitted
  if (identical(fitted$learner$format, "n4mm")) {
    if (!is.raw(fitted$state)) stop("n4m model bundle lacks N4MM bytes", call. = FALSE)
    fitted$state <- n4m::n4m_model_import(fitted$state)
  }
  if (identical(fitted$learner$format, "torch-r")) {
    if (!is.list(fitted$state) || !is.raw(fitted$state$module))
      stop("torch bundle lacks serialized module bytes", call. = FALSE)
    if (!requireNamespace("torch", quietly = TRUE))
      stop("torch is required to load this model", call. = FALSE)
    connection <- rawConnection(fitted$state$module, "rb")
    on.exit(close(connection), add = TRUE)
    fitted$state$module <- torch::torch_load(connection, device = "cpu")
  }
  fitted
}

#' Export a fitted native pipeline as portable N4MM bytes
#'
#' Accepts plain SIMPLS (N4MM format 1) or the native
#' SNV → Savitzky-Golay → SIMPLS profile (format 2). Export the unfitted recipe separately with
#' [nirs4all_export_pipeline()] when transferring it to another language.
#' These bytes are model state, not a full DAG-ML Archive V2/V3.
#' @param object A fitted [nirs4all_fit()] result.
#' @return Raw N4MM format-1 or format-2 bytes.
#' @export
nirs4all_export_native_model <- function(object) {
  if (!inherits(object, "nirs4all_fitted") ||
      !identical(object$learner$format, "n4mm"))
    stop("only a native n4m PLS pipeline can be exported", call. = FALSE)
  recipe <- structure(list(steps = object$steps, learner = object$learner),
                      class = "nirs4all_pipeline")
  profile <- nirs4all_native_model_profile(recipe)
  if (is.null(profile) ||
      !identical(object$preprocessing_owner, profile$owner))
    stop("fitted native recipe is unsupported", call. = FALSE)
  bytes <- n4m::n4m_model_export(object$state)
  info <- n4m::n4m_model_pipeline_info(bytes)
  descriptor <- n4m::n4m_model_descriptor(bytes)
  if (!identical(descriptor$format_version, profile$format_version) ||
      !identical(descriptor$algorithm, 0L) ||
      !identical(descriptor$solver, 1L) ||
      !identical(descriptor$deflation, 0L) ||
      !identical(descriptor$n_features, as.integer(object$n_features)) ||
      !identical(descriptor$n_components,
                 as.integer(object$learner$spec$n_components)) ||
      !identical(descriptor$n_targets, 1L) ||
      bitwAnd(as.integer(descriptor$capabilities), profile$capabilities) !=
        profile$capabilities ||
      !nirs4all_native_model_pipeline_matches(info, profile,
                                               descriptor$n_features))
    stop("native N4MM state does not match its R recipe", call. = FALSE)
  bytes
}

#' Import native N4MM pipeline state against an explicit R recipe
#'
#' The n4m decoder validates the bytes and any embedded preprocessing profile.
#' This function refuses a mismatched R recipe or feature width. The caller
#' supplies the recipe and, if relevant, the original ordered feature names.
#' For plain format-1 PLS, scaling flags are caller assertions because the
#' validated native descriptor does not expose those training flags.
#' @param bytes Raw N4MM format-1 or format-2 bytes.
#' @param pipeline Matching unfitted [nirs4all_pipeline()] definition or its
#'   portable JSON/YAML recipe.
#' @param feature_names Optional ordered feature names from training.
#' @return A fitted pipeline that predicts directly on raw spectra.
#' @export
nirs4all_import_native_model <- function(bytes, pipeline, feature_names = NULL) {
  if (!is.raw(bytes) || !length(bytes))
    stop("bytes must be non-empty raw N4MM state", call. = FALSE)
  if (!inherits(pipeline, "nirs4all_pipeline"))
    pipeline <- nirs4all_pipeline_from_portable(pipeline)
  profile <- nirs4all_native_model_profile(pipeline)
  if (is.null(profile)) stop("R recipe is not a supported native profile", call. = FALSE)
  info <- n4m::n4m_model_pipeline_info(bytes)
  descriptor <- n4m::n4m_model_descriptor(bytes)
  width <- descriptor$n_features
  if (!identical(descriptor$format_version, profile$format_version) ||
      !identical(descriptor$algorithm, 0L) ||
      !identical(descriptor$solver, 1L) ||
      !identical(descriptor$deflation, 0L) ||
      width < 1L ||
      !identical(descriptor$n_targets, 1L) ||
      !identical(descriptor$n_components,
                 as.integer(pipeline$learner$spec$n_components)) ||
      bitwAnd(as.integer(descriptor$capabilities), profile$capabilities) !=
        profile$capabilities ||
      !nirs4all_native_model_pipeline_matches(info, profile, width))
    stop("N4MM state does not match the native R recipe", call. = FALSE)
  if (!is.null(feature_names) &&
      (!is.character(feature_names) || length(feature_names) != width ||
       anyNA(feature_names) || any(!nzchar(feature_names)) ||
       anyDuplicated(feature_names)))
    stop("feature_names must match the native feature width", call. = FALSE)
  state <- n4m::n4m_model_import(bytes)
  if (!identical(attr(state, "n_features"), width) ||
      !identical(attr(state, "n_targets"), 1L))
    stop("imported N4MM model dimensions do not match the recipe", call. = FALSE)
  structure(list(steps = pipeline$steps, learner = pipeline$learner,
                 state = state, step_states = rep(list(NULL), length(pipeline$steps)),
                 preprocessing_owner = profile$owner,
                 n_features = width,
                 feature_names = feature_names), class = "nirs4all_fitted")
}

nirs4all_native_model_profile <- function(pipeline) {
  spec <- pipeline$learner$spec
  defaults <- is.list(spec) && identical(spec$learner, "pls") &&
    identical(spec$algo, "pls_simpls") &&
    all(vapply(spec[c("center_x", "scale_x", "center_y", "scale_y")],
               identical, logical(1), TRUE))
  if (!defaults) return(NULL)
  if (!length(pipeline$steps))
    return(list(format_version = 1L, owner = "external_r",
                capabilities = 1L, embedded = NULL))
  embedded <- nirs4all_embedded_snv_savgol(pipeline)
  if (is.null(embedded)) return(NULL)
  list(format_version = 2L, owner = "embedded_methods",
       capabilities = 9L, embedded = embedded)
}

nirs4all_native_model_pipeline_matches <- function(info, profile, width) {
  if (identical(profile$format_version, 1L)) return(!isTRUE(info$present))
  isTRUE(info$present) && identical(info$semantic_profile, 1L) &&
    identical(info$window_length, as.integer(profile$embedded[[1L]])) &&
    identical(info$polyorder, as.integer(profile$embedded[[2L]])) &&
    identical(info$raw_n_features, width) &&
    identical(info$model_n_features, width)
}
