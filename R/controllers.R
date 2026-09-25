#' Define a learner controller
#' @param fit Function accepting transformed `X` and `y`, returning state.
#' @param predict Function accepting state and transformed `X`, returning predictions.
#' @param name Stable controller label.
#' @export
nirs4all_controller <- function(fit, predict, name) {
  if (!is.function(fit) || !is.function(predict))
    stop("fit and predict must be functions", call. = FALSE)
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name))
    stop("name must be a non-empty string", call. = FALSE)
  structure(list(fit = fit, predict = predict, name = name, portable = FALSE),
            class = "nirs4all_controller")
}

#' Portable n4m PLS controller
#' @param n_components Positive number of PLS components.
#' @param algo Native n4m PLS solver.
#' @param center_x,scale_x,center_y,scale_y Native n4m scaling flags.
#' @export
nirs4all_pls <- function(n_components = 2L, algo = "pls_simpls",
                         center_x = TRUE, scale_x = TRUE,
                         center_y = TRUE, scale_y = TRUE) {
  if (length(n_components) != 1L || !is.numeric(n_components) ||
      is.na(n_components) || !is.finite(n_components) ||
      n_components < 1L || n_components > .Machine$integer.max ||
      n_components != floor(n_components))
    stop("n_components must be a positive integer", call. = FALSE)
  algorithms <- c("pls_nipals", "pls_orthogonal_scores", "pls_simpls",
                  "pls_kernel_algorithm", "pls_wide_kernel", "pls_svd",
                  "pls_power", "pls_randomized_svd", "pcr_svd", "opls_nipals")
  if (!is.character(algo) || length(algo) != 1L || is.na(algo) || !(algo %in% algorithms))
    stop("unsupported n4m PLS algorithm", call. = FALSE)
  flags <- list(center_x, scale_x, center_y, scale_y)
  if (!all(vapply(flags, function(x) is.logical(x) && length(x) == 1L && !is.na(x), logical(1))))
    stop("scaling flags must be TRUE or FALSE", call. = FALSE)
  controller <- nirs4all_controller(
    fit = function(X, y) n4m::n4m_fit(X, y, algo = algo,
      n_components = as.integer(n_components), center_x = center_x,
      scale_x = scale_x, center_y = center_y, scale_y = scale_y),
    predict = function(state, X) as.numeric(n4m::n4m_predict(state, X)),
    name = paste0("n4m:", algo))
  controller$portable <- TRUE
  controller$format <- "n4mm"
  controller$spec <- list(learner = "pls", n_components = as.integer(n_components),
                          algo = algo, center_x = center_x, scale_x = scale_x,
                          center_y = center_y, scale_y = scale_y)
  controller
}

#' Base R linear-model controller
#' @export
nirs4all_lm <- function() {
  controller <- nirs4all_controller(
    fit = function(X, y) {
      frame <- as.data.frame(X)
      names(frame) <- paste0("x", seq_len(ncol(X)))
      frame$y <- y
      model <- stats::lm(y ~ ., data = frame)
      if (anyNA(stats::coef(model)))
        stop("linear model design is rank-deficient; use PLS or reduce features",
             call. = FALSE)
      model
    },
    predict = function(state, X) {
      frame <- as.data.frame(X)
      names(frame) <- paste0("x", seq_len(ncol(X)))
      as.numeric(stats::predict(state, newdata = frame))
    }, name = "stats:lm")
  controller$spec <- list(learner = "lm")
  controller
}

#' Optional ranger random-forest controller
#' @param num.trees Number of trees.
#' @param seed Explicit random seed (the global R RNG state is not used).
#' @param ... Additional `ranger::ranger()` parameters. Avoid parameters that
#'   override `formula`, `data`, `num.trees`, or `seed`.
#' @export
nirs4all_ranger <- function(num.trees = 500L, seed = 1L, ...) {
  if (!requireNamespace("ranger", quietly = TRUE))
    stop("Install the optional 'ranger' package first", call. = FALSE)
  if (length(num.trees) != 1L || !is.numeric(num.trees) || !is.finite(num.trees) ||
      num.trees < 1L || num.trees > .Machine$integer.max ||
      num.trees != floor(num.trees))
    stop("num.trees must be a positive integer", call. = FALSE)
  if (length(seed) != 1L || !is.numeric(seed) || !is.finite(seed) ||
      seed < 0L || seed > .Machine$integer.max || seed != floor(seed))
    stop("seed must be a non-negative integer", call. = FALSE)
  extra <- list(...)
  if (any(names(extra) %in% c("formula", "data", "num.trees", "seed", "dependent.variable.name")))
    stop("reserved ranger parameters cannot be overridden", call. = FALSE)
  controller <- nirs4all_controller(
    fit = function(X, y) {
      frame <- as.data.frame(X)
      names(frame) <- paste0("x", seq_len(ncol(X)))
      frame$y <- y
      do.call(ranger::ranger, c(list(formula = y ~ ., data = frame,
        num.trees = as.integer(num.trees), seed = as.integer(seed)), extra))
    },
    predict = function(state, X) {
      frame <- as.data.frame(X)
      names(frame) <- paste0("x", seq_len(ncol(X)))
      as.numeric(stats::predict(state, data = frame)$predictions)
    }, name = "ranger:regression")
  controller$spec <- list(learner = "ranger", num_trees = as.integer(num.trees),
                          seed = as.integer(seed), extra = extra)
  controller
}

#' Optional glmnet regularized-regression controller
#' @param lambda Strictly positive selected regularization strength. This
#'   controller fits an explicit decreasing path ending at `lambda`, avoiding
#'   interpolation when predicting at that value.
#' @param alpha Elastic-net mixing parameter: zero for ridge, one for lasso.
#' @param standardize Whether glmnet standardizes features internally.
#' @export
nirs4all_glmnet <- function(lambda, alpha = 1, standardize = TRUE) {
  if (!requireNamespace("glmnet", quietly = TRUE))
    stop("Install the optional 'glmnet' package first", call. = FALSE)
  if (!is.numeric(lambda) || length(lambda) != 1L ||
      !is.finite(lambda) || lambda <= 0)
    stop("lambda must be a strictly positive finite number", call. = FALSE)
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) ||
      alpha < 0 || alpha > 1)
    stop("alpha must be between zero and one", call. = FALSE)
  if (!is.logical(standardize) || length(standardize) != 1L || is.na(standardize))
    stop("standardize must be TRUE or FALSE", call. = FALSE)
  path <- unique(lambda * exp(seq(log(1000), 0, length.out = 32L)))
  controller <- nirs4all_controller(
    fit = function(X, y) {
      if (ncol(X) < 2L)
        stop("glmnet requires at least two features", call. = FALSE)
      model <- glmnet::glmnet(X, y, family = "gaussian", alpha = alpha,
                             lambda = path, standardize = standardize)
      if (!any(abs(model$lambda - lambda) <= .Machine$double.eps * max(1, lambda)))
        stop("glmnet did not fit the requested lambda", call. = FALSE)
      list(model = model, lambda = lambda)
    },
    predict = function(state, X) as.numeric(stats::predict(state$model,
                                                          newx = X,
                                                          s = state$lambda)),
    name = "glmnet:gaussian")
  controller$spec <- list(learner = "glmnet", lambda = lambda, alpha = alpha,
                          standardize = standardize)
  controller
}
