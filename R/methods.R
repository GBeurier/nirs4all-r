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

#' Native n4m sparse PLS-DA classification controller
#'
#' Fits the sparse PLS-DA kernel in n4m. Out-of-sample class decisions use
#' the native coefficient matrix and centering state, matching the Python
#' `SparsePLSDAClassifier` decision function. `predict_proba()` applies a
#' softmax to these scores solely to satisfy probability-based DAG consumers;
#' these values are not calibrated probabilities.
#'
#' @param n_components Positive number of latent components.
#' @param sparsity_lambda Non-negative sparse PLS-DA regularization.
#' @export
nirs4all_sparse_pls_da <- function(n_components = 2L,
                                   sparsity_lambda = 0.05) {
  if (!is.numeric(n_components) || length(n_components) != 1L ||
      !is.finite(n_components) || n_components < 1L ||
      n_components > .Machine$integer.max || n_components != floor(n_components))
    stop("n_components must be a positive integer", call. = FALSE)
  if (!is.numeric(sparsity_lambda) || length(sparsity_lambda) != 1L ||
      !is.finite(sparsity_lambda) || sparsity_lambda < 0)
    stop("sparsity_lambda must be a finite non-negative number", call. = FALSE)
  scores <- function(state, X) {
    if (!is.list(state) || !is.matrix(state$coefficients) ||
        ncol(X) != nrow(state$coefficients) ||
        length(state$x_mean) != ncol(X) ||
        length(state$y_mean) != ncol(state$coefficients))
      stop("invalid sparse PLS-DA model state", call. = FALSE)
    out <- sweep(X, 2L, state$x_mean, "-") %*% state$coefficients
    sweep(out, 2L, state$y_mean, "+")
  }
  controller <- nirs4all_controller(
    fit = function(X, y) {
      if (n_components > min(nrow(X) - 1L, ncol(X)))
        stop("n_components exceeds available sparse PLS-DA rank", call. = FALSE)
      classes <- levels(y)
      result <- n4m::n4m_method(
        "sparse_pls_da", X, rep(0, nrow(X)), as.integer(n_components),
        params = list(y_labels = as.integer(y) - 1L,
                      sparsity_lambda = sparsity_lambda))
      coefficients <- as.matrix(result$coefficients)
      x_mean <- as.numeric(result$x_mean)
      y_mean <- as.numeric(result$y_mean)
      if (!is.numeric(coefficients) ||
          !identical(dim(coefficients), c(ncol(X), length(classes))) ||
          length(x_mean) != ncol(X) || length(y_mean) != length(classes) ||
          any(!is.finite(coefficients)) || any(!is.finite(x_mean)) ||
          any(!is.finite(y_mean)))
        stop("n4m returned an invalid sparse PLS-DA model", call. = FALSE)
      list(coefficients = coefficients, x_mean = x_mean, y_mean = y_mean,
           classes = classes)
    },
    predict = function(state, X)
      factor(state$classes[max.col(scores(state, X), ties.method = "first")],
             levels = state$classes),
    predict_proba = function(state, X) {
      logits <- scores(state, X)
      logits <- logits - apply(logits, 1L, max)
      probabilities <- exp(logits)
      probabilities <- probabilities / rowSums(probabilities)
      colnames(probabilities) <- state$classes
      probabilities
    },
    task = "classification", name = "n4m:sparse_pls_da")
  controller$spec <- list(learner = "sparse_pls_da",
                          n_components = as.integer(n_components),
                          sparsity_lambda = sparsity_lambda)
  controller
}
