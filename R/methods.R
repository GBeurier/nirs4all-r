#' Native n4m linear-method controller
#'
#' Exposes MethodResult regressors whose fitted coefficients provide an
#' affine prediction on new X. Most use
#' `(X - x_mean) %*% coefficients + y_mean`; MB-PLS instead returns
#' original-scale coefficients and an explicit intercept. Other n4m methods
#' may need algorithm-specific prediction state and are deliberately excluded.
#' MethodResult coefficients are wrapped as an affine N4MM predictor when
#' serialized. The native bytes attest predictions, not the fitting algorithm.
#'
#' @param method Supported native regression method.
#' @param n_components Positive component count; ignored by ridge's solver.
#' @param params Named method-specific parameter list. N-PLS requires
#'   positive `mode_j` and `mode_k` whose product equals the fitted feature
#'   width; MB-PLS requires a positive integer `block_sizes` vector summing
#'   to the fitted feature width; boosting `learning_rate` must be in `(0, 1]`.
#'   DI-PLS requires a finite target-domain matrix `X_target` in the same
#'   feature space received by the learner. Target spectra are used in fitting;
#'   avoid including evaluation samples unless transductive fitting is intended.
#' @export
nirs4all_n4m_method <- function(method, n_components = 2L, params = list()) {
  allowed <- list(
    ridge = "ridge_lambda", ridge_pls = "ridge_lambda",
    robust_pls = c("huber_k", "max_irls_iter"), cppls = "gamma",
    sparse_simpls = "sparsity_lambda", ecr = "alpha",
    continuum_regression = "tau", mir_pls = character(),
    fused_sparse_pls = c("l1_lambda", "fusion_lambda"),
    bagging_pls = c("n_estimators", "seed"),
    boosting_pls = c("n_estimators", "learning_rate"),
    random_subspace_pls = c("n_estimators", "features_per_subspace", "seed"),
    n_pls = c("mode_j", "mode_k"), mb_pls = "block_sizes",
    di_pls = c("X_target", "di_lambda"))
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
  if (identical(method, "mb_pls")) {
    sizes <- params$block_sizes
    if (is.list(sizes) && is.null(names(sizes)) && length(sizes) &&
        all(vapply(sizes, function(value)
          is.numeric(value) && length(value) == 1L, logical(1))))
      sizes <- unlist(sizes, use.names = FALSE)
    if (!is.numeric(sizes) || length(sizes) < 2L ||
        anyNA(sizes) || any(!is.finite(sizes)) || any(sizes < 1L) ||
        any(sizes != floor(sizes)) || any(sizes > .Machine$integer.max) ||
        sum(as.double(sizes)) > .Machine$integer.max)
      stop("mb_pls requires at least two positive integer block_sizes",
           call. = FALSE)
    params$block_sizes <- as.integer(sizes)
  }
  if (identical(method, "di_pls")) {
    target <- params$X_target
    if (!is.matrix(target) || !is.numeric(target) ||
        nrow(target) < 2L || ncol(target) < 2L ||
        anyNA(target) || any(!is.finite(target)))
      stop("di_pls requires a finite numeric target-domain matrix",
           call. = FALSE)
    storage.mode(target) <- "double"
    params$X_target <- target
    if (!is.null(params$di_lambda) &&
        (!is.numeric(params$di_lambda) ||
         length(params$di_lambda) != 1L ||
         !is.finite(params$di_lambda) || params$di_lambda < 0))
      stop("di_lambda must be a finite non-negative number", call. = FALSE)
  }
  scalar_params <- params[setdiff(names(params), c("block_sizes", "X_target"))]
  if (length(scalar_params) && !all(vapply(scalar_params, function(value)
      (is.numeric(value) || is.logical(value)) && length(value) == 1L &&
      !is.na(value) && is.finite(value), logical(1))))
    stop("method parameters must be finite numeric or logical scalars",
         call. = FALSE)
  if (identical(method, "n_pls") &&
      !setequal(names(params), c("mode_j", "mode_k")))
    stop("n_pls requires mode_j and mode_k", call. = FALSE)
  for (name in intersect(names(params),
                         c("max_irls_iter", "n_estimators",
                           "features_per_subspace", "seed",
                           "mode_j", "mode_k"))) {
    value <- params[[name]]
    minimum <- if (identical(name, "seed")) 0L else 1L
    if (!is.numeric(value) || value < minimum || value != floor(value) ||
        value > .Machine$integer.max)
      stop(sprintf("%s must be a bounded %s integer", name,
        if (identical(name, "seed")) "non-negative" else "positive"),
        call. = FALSE)
    params[[name]] <- as.integer(value)
  }
  for (name in intersect(names(params),
                         c("l1_lambda", "fusion_lambda", "learning_rate"))) {
    value <- params[[name]]
    if (!is.numeric(value) || value < 0 ||
        (identical(name, "learning_rate") &&
         (value == 0 || value > 1)))
      stop(sprintf("%s must be %s", name,
        if (identical(name, "learning_rate")) "in (0, 1]" else "non-negative"),
        call. = FALSE)
  }
  controller <- nirs4all_controller(
    fit = function(X, y) {
      if (identical(method, "random_subspace_pls")) {
        subspace <- if (is.null(params$features_per_subspace)) 10L else
          params$features_per_subspace
        if (subspace > ncol(X))
          stop("features_per_subspace exceeds the input feature count",
               call. = FALSE)
      }
      if (identical(method, "n_pls") &&
          as.double(params$mode_j) * as.double(params$mode_k) != ncol(X))
        stop("mode_j times mode_k must equal the input feature count",
             call. = FALSE)
      if (identical(method, "mb_pls") && sum(params$block_sizes) != ncol(X))
        stop("sum(block_sizes) must equal the input feature count",
             call. = FALSE)
      if (identical(method, "di_pls") && ncol(params$X_target) != ncol(X))
        stop("X_target width must equal transformed input feature count",
             call. = FALSE)
      if (identical(method, "di_pls") &&
          !is.null(colnames(params$X_target)) &&
          !identical(colnames(params$X_target), colnames(X)))
        stop("X_target feature names or order differ from transformed X",
             call. = FALSE)
      result <- n4m::n4m_method(method, X, y, as.integer(n_components),
                                params = params)
      coefficients <- as.matrix(result$coefficients)
      direct_intercept <- identical(method, "mb_pls")
      x_mean <- if (direct_intercept) NULL else as.numeric(result$x_mean)
      y_mean <- if (direct_intercept) NULL else as.numeric(result$y_mean)
      intercept <- if (direct_intercept) as.numeric(result$intercept) else
        as.numeric(y_mean - drop(x_mean %*% coefficients))
      if (!is.numeric(coefficients) || !identical(dim(coefficients), c(ncol(X), 1L)) ||
          (!direct_intercept && (length(x_mean) != ncol(X) ||
           length(y_mean) != 1L || any(!is.finite(x_mean)) ||
           any(!is.finite(y_mean)))) || length(intercept) != 1L ||
          any(!is.finite(coefficients)) || any(!is.finite(intercept)))
        stop("n4m method returned an unsupported regression model", call. = FALSE)
      native_model <- n4m::n4m_model_import_linear_predictor(
        coefficients, intercept, nrow(X))
      state <- list(coefficients = coefficients, x_mean = x_mean,
        y_mean = y_mean, source_training_samples = nrow(X),
        native_model = native_model)
      if (direct_intercept) state$intercept <- intercept
      state
    },
    predict = function(state, X) {
      if (is.null(state$native_model))
        stop("n4m affine predictor is missing its native model", call. = FALSE)
      as.numeric(n4m::n4m_predict(state$native_model, X))
    },
    name = paste0("n4m:", method))
  controller$format <- "n4mm_affine"
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
    if (!is.list(state) || typeof(state$native_model) != "externalptr" ||
        !is.character(state$classes) || length(state$classes) < 2L)
      stop("invalid sparse PLS-DA model state", call. = FALSE)
    out <- n4m::n4m_predict(state$native_model, X)
    if (!is.matrix(out) || !identical(dim(out), c(nrow(X), length(state$classes))) ||
        any(!is.finite(out)))
      stop("n4m returned invalid sparse PLS-DA scores", call. = FALSE)
    out
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
      intercept <- as.numeric(y_mean - drop(x_mean %*% coefficients))
      native_model <- n4m::n4m_model_import_linear_predictor(
        coefficients, intercept, nrow(X))
      list(coefficients = coefficients, x_mean = x_mean, y_mean = y_mean,
           native_model = native_model, classes = classes)
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
  controller$format <- "n4mm_sparse_pls_da"
  controller$spec <- list(learner = "sparse_pls_da",
                          n_components = as.integer(n_components),
                          sparsity_lambda = sparsity_lambda)
  controller
}
