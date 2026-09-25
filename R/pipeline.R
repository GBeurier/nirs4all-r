#' Define a row-wise standard normal variate step
#' @param ddof Degrees-of-freedom correction, normally zero.
#' @export
nirs4all_snv <- function(ddof = 0L) {
  if (length(ddof) != 1L || !is.numeric(ddof) || is.na(ddof) ||
      !is.finite(ddof) || ddof < 0L || ddof > .Machine$integer.max ||
      ddof != floor(ddof))
    stop("ddof must be a non-negative integer", call. = FALSE)
  structure(list(kind = "snv", ddof = as.integer(ddof)), class = "nirs4all_step")
}

#' Define a Savitzky-Golay step
#' @param window_length Odd window length.
#' @param polyorder Polynomial order below the window length.
#' @param deriv Derivative order at most polyorder.
#' @param delta Wavelength spacing.
#' @param mode Boundary mode supported by `n4m`.
#' @param cval Fill value for constant boundary mode.
#' @export
nirs4all_savgol <- function(window_length, polyorder = 2L, deriv = 0L,
                            delta = 1, mode = "interp", cval = 0) {
  integer_scalar <- function(x) length(x) == 1L && is.numeric(x) &&
    is.finite(x) && x >= 0 && x <= .Machine$integer.max && x == floor(x)
  if (!integer_scalar(window_length) || window_length < 3L || window_length %% 2L != 1L)
    stop("window_length must be an odd integer >= 3", call. = FALSE)
  if (!integer_scalar(polyorder) || polyorder < 0L || polyorder >= window_length)
    stop("polyorder must be an integer below window_length", call. = FALSE)
  if (!integer_scalar(deriv) || deriv < 0L || deriv > polyorder)
    stop("deriv must be an integer between zero and polyorder", call. = FALSE)
  if (length(delta) != 1L || !is.numeric(delta) || !is.finite(delta) || delta <= 0)
    stop("delta must be positive and finite", call. = FALSE)
  if (length(mode) != 1L || is.na(mode) ||
      !(mode %in% c("mirror", "constant", "nearest", "wrap", "interp")))
    stop("unsupported Savitzky-Golay boundary mode", call. = FALSE)
  if (length(cval) != 1L || !is.numeric(cval) || !is.finite(cval))
    stop("cval must be finite", call. = FALSE)
  structure(list(kind = "savgol", window_length = as.integer(window_length),
                 polyorder = as.integer(polyorder), deriv = as.integer(deriv),
                 delta = delta, mode = mode, cval = cval), class = "nirs4all_step")
}

#' Define an R pipeline
#' @param steps List of preprocessing steps, applied in order.
#' @param learner One controller from [nirs4all_pls()], [nirs4all_lm()],
#'   [nirs4all_ranger()] or [nirs4all_controller()].
#' @export
nirs4all_pipeline <- function(steps = list(), learner = nirs4all_pls()) {
  if (!is.list(steps) || !all(vapply(steps, inherits, logical(1), "nirs4all_step")))
    stop("steps must be a list of nirs4all preprocessing steps", call. = FALSE)
  if (!inherits(learner, "nirs4all_controller"))
    stop("learner must be a nirs4all controller", call. = FALSE)
  structure(list(steps = steps, learner = learner), class = "nirs4all_pipeline")
}

nirs4all_matrix <- function(X, n_features = NULL) {
  if (!is.matrix(X) || !is.numeric(X) || length(dim(X)) != 2L ||
      nrow(X) < 1L || ncol(X) < 1L || anyNA(X) || any(!is.finite(X)))
    stop("X must be a non-empty finite numeric matrix", call. = FALSE)
  if (!is.null(n_features) && ncol(X) != n_features)
    stop(sprintf("X has %d features; expected %d", ncol(X), n_features), call. = FALSE)
  storage.mode(X) <- "double"
  X
}

nirs4all_transform <- function(X, steps) {
  for (step in steps) {
    if (identical(step$kind, "snv") && step$ddof >= ncol(X))
      stop("SNV ddof must be smaller than the feature count", call. = FALSE)
    X <- switch(step$kind,
      snv = n4m::snv_transform(X, ddof = step$ddof),
      savgol = n4m::savgol_transform(X, step$window_length,
        step$polyorder, step$deriv, step$delta, step$mode, step$cval),
      stop("unknown preprocessing step", call. = FALSE))
    if (!is.matrix(X) || !is.numeric(X) || anyNA(X) || any(!is.finite(X)))
      stop("preprocessing produced non-finite or invalid data", call. = FALSE)
  }
  X
}

#' Fit a pipeline on R matrices
#' @param pipeline A [nirs4all_pipeline()] definition.
#' @param X Numeric samples-by-features matrix.
#' @param y Finite numeric target vector.
#' @return Fitted pipeline. Use [nirs4all_predict()] or [nirs4all_save()].
#' @export
nirs4all_fit <- function(pipeline, X, y) {
  if (!inherits(pipeline, "nirs4all_pipeline"))
    stop("pipeline must be a nirs4all_pipeline", call. = FALSE)
  X <- nirs4all_matrix(X)
  if (!is.numeric(y) || is.matrix(y) || length(y) != nrow(X) ||
      anyNA(y) || any(!is.finite(y)))
    stop("y must be one finite numeric value per row of X", call. = FALSE)
  if (!is.null(rownames(X)) && !is.null(names(y)) &&
      !identical(rownames(X), names(y)))
    stop("X row names and y sample names differ", call. = FALSE)
  transformed <- nirs4all_transform(X, pipeline$steps)
  state <- pipeline$learner$fit(transformed, as.numeric(y))
  structure(list(steps = pipeline$steps, learner = pipeline$learner,
                 state = state, n_features = ncol(X),
                 feature_names = colnames(X)), class = "nirs4all_fitted")
}

#' Predict from a fitted pipeline
#' @param object Fitted pipeline.
#' @param X Numeric samples-by-features matrix.
#' @export
nirs4all_predict <- function(object, X) {
  if (!inherits(object, "nirs4all_fitted"))
    stop("object must be a fitted nirs4all pipeline", call. = FALSE)
  X <- nirs4all_matrix(X, object$n_features)
  if (!is.null(object$feature_names) && !identical(colnames(X), object$feature_names))
    stop("X feature names or order differ from training", call. = FALSE)
  out <- object$learner$predict(object$state, nirs4all_transform(X, object$steps))
  if (!is.numeric(out) || length(out) != nrow(X) || anyNA(out) || any(!is.finite(out)))
    stop("controller returned invalid predictions", call. = FALSE)
  as.numeric(out)
}

#' @export
predict.nirs4all_fitted <- function(object, newdata, ...) {
  if (length(list(...))) stop("unused prediction arguments", call. = FALSE)
  nirs4all_predict(object, newdata)
}
