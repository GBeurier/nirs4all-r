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
  fitted
}
