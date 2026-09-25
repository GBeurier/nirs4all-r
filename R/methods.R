#' Native n4m linear-method controller
#'
#' Exposes MethodResult regressors whose fitted coefficients obey
#' `(X - x_mean) %*% coefficients + y_mean`. Other n4m methods may need
#' algorithm-specific prediction state and are deliberately excluded.
#' MethodResult coefficients are saved in the R bundle; unlike the PLS
#' controller, this is not an N4MM model export.
#'
#' @param method Supported native regression method.
#' @param n_components Positive component count; ignored by ridge's solver.
#' @param params Named method-specific scalar parameter list.
#' @export
nirs4all_n4m_method <- function(method, n_components = 2L, params = list()) {
  allowed <- list(
    ridge = "ridge_lambda", ridge_pls = "ridge_lambda",
    robust_pls = c("huber_k", "max_irls_iter"), cppls = "gamma",
    sparse_simpls = "sparsity_lambda", ecr = "alpha",
    continuum_regression = "tau", mir_pls = character())
  if (!is.character(method) || length(method) != 1L || is.na(method) ||
      !(method %in% names(allowed)))
    stop("unsupported n4m linear method", call. = FALSE)
  if (!is.numeric(n_components) || length(n_components) != 1L ||
      !is.finite(n_components) || n_components < 1L ||
      n_components > .Machine$integer.max || n_components != floor(n_components))
    stop("n_components must be a positive integer", call. = FALSE)
  if (!is.list(params) || (length(params) &&
      (is.null(names(params)) || anyNA(names(params)) ||
       any(!nzchar(names(params))) || anyDuplicated(names(params)) ||
       !all(names(params) %in% allowed[[method]]))))
    stop("params must contain only named parameters supported by this method",
         call. = FALSE)
  if (length(params) && !all(vapply(params, function(value)
      (is.numeric(value) || is.logical(value)) && length(value) == 1L &&
      !is.na(value) && is.finite(value), logical(1))))
    stop("method parameters must be finite numeric or logical scalars",
         call. = FALSE)
  if ("max_irls_iter" %in% names(params)) {
    value <- params$max_irls_iter
    if (!is.numeric(value) || value < 1L || value != floor(value) ||
        value > .Machine$integer.max)
      stop("max_irls_iter must be a positive integer", call. = FALSE)
    params$max_irls_iter <- as.integer(value)
  }
  controller <- nirs4all_controller(
    fit = function(X, y) {
      result <- n4m::n4m_method(method, X, y, as.integer(n_components),
                                params = params)
      coefficients <- as.matrix(result$coefficients)
      x_mean <- as.numeric(result$x_mean)
      y_mean <- as.numeric(result$y_mean)
      if (!is.numeric(coefficients) || !identical(dim(coefficients), c(ncol(X), 1L)) ||
          length(x_mean) != ncol(X) || length(y_mean) != 1L ||
          any(!is.finite(coefficients)) || any(!is.finite(x_mean)) ||
          any(!is.finite(y_mean)))
        stop("n4m method returned an unsupported regression model", call. = FALSE)
      list(coefficients = coefficients, x_mean = x_mean, y_mean = y_mean)
    },
    predict = function(state, X) as.numeric(
      sweep(X, 2L, state$x_mean, "-") %*% state$coefficients + state$y_mean),
    name = paste0("n4m:", method))
  controller$spec <- list(learner = "n4m_method", method = method,
                          n_components = as.integer(n_components), params = params)
  controller
}
