#' Define a learner controller
#' @param fit Function accepting transformed `X` and `y`, returning state.
#' @param predict Function accepting state and transformed `X`, returning predictions.
#' @param name Stable controller label.
#' @param task Either `"regression"` or `"classification"`.
#' @param predict_proba Optional classification probability function.
#' @export
nirs4all_controller <- function(fit, predict, name, task = "regression",
                                predict_proba = NULL) {
  if (!is.function(fit) || !is.function(predict))
    stop("fit and predict must be functions", call. = FALSE)
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name))
    stop("name must be a non-empty string", call. = FALSE)
  if (!is.character(task) || length(task) != 1L || is.na(task) ||
      !(task %in% c("regression", "classification")))
    stop("task must be regression or classification", call. = FALSE)
  if (!is.null(predict_proba) && !is.function(predict_proba))
    stop("predict_proba must be a function", call. = FALSE)
  structure(list(fit = fit, predict = predict, predict_proba = predict_proba,
                 task = task, name = name, portable = FALSE),
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

#' Optional ranger probability-forest classification controller
#'
#' Fits a factor response with fixed class levels. Predictions are factors and
#' probabilities retain those levels as column names. The explicit seed makes
#' refits independent of the caller's global R RNG state.
#' @param num.trees Number of trees.
#' @param seed Explicit non-negative random seed.
#' @param ... Additional `ranger::ranger()` parameters; model identity and
#'   probability mode cannot be overridden.
#' @export
nirs4all_ranger_classifier <- function(num.trees = 500L, seed = 1L, ...) {
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
  if (anyNA(names(extra)) || any(!nzchar(names(extra))) ||
      any(names(extra) %in% c("formula", "data", "num.trees", "seed",
                              "dependent.variable.name", "probability")))
    stop("ranger extra parameters must be named and cannot override reserved fields",
         call. = FALSE)
  probabilities <- function(state, X) {
    frame <- as.data.frame(X)
    names(frame) <- paste0("x", seq_len(ncol(X)))
    values <- stats::predict(state$model, data = frame)$predictions
    if (!is.matrix(values) || !identical(colnames(values), state$classes))
      stop("ranger returned probabilities with unexpected class columns", call. = FALSE)
    values
  }
  controller <- nirs4all_controller(
    fit = function(X, y) {
      if (!is.factor(y) || length(levels(y)) < 2L ||
          any(tabulate(as.integer(y), nbins = nlevels(y)) == 0L))
        stop("each class must occur in the training fold", call. = FALSE)
      frame <- as.data.frame(X)
      names(frame) <- paste0("x", seq_len(ncol(X)))
      frame$y <- y
      model <- do.call(ranger::ranger, c(list(formula = y ~ ., data = frame,
        num.trees = as.integer(num.trees), seed = as.integer(seed),
        probability = TRUE), extra))
      list(model = model, classes = levels(y))
    },
    predict = function(state, X) {
      values <- probabilities(state, X)
      factor(state$classes[max.col(values, ties.method = "first")],
             levels = state$classes)
    }, predict_proba = probabilities, task = "classification",
    name = "ranger:classification")
  controller$spec <- list(learner = "ranger_classifier",
                          num_trees = as.integer(num.trees),
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

#' Optional parsnip regression controller
#'
#' Accepts a fully specified `parsnip` model specification (including its
#' engine). The model is fitted separately within every nirs4all training
#' scope; no preprocessing is fitted on validation or prediction samples.
#' Only one-column numeric regression predictions are currently supported.
#' @param spec A `parsnip` regression `model_spec` with an engine selected.
#' @export
nirs4all_parsnip <- function(spec) {
  if (!requireNamespace("parsnip", quietly = TRUE))
    stop("Install the optional 'parsnip' package first", call. = FALSE)
  if (!inherits(spec, "model_spec") || !identical(spec$mode, "regression") ||
      !is.character(spec$engine) || length(spec$engine) != 1L ||
      is.na(spec$engine) || !nzchar(spec$engine))
    stop("spec must be a parsnip regression model with a selected engine",
         call. = FALSE)
  controller <- nirs4all_controller(
    fit = function(X, y) {
      predictors <- as.data.frame(X)
      names(predictors) <- paste0("x", seq_len(ncol(X)))
      parsnip::fit_xy(spec, x = predictors, y = y)
    },
    predict = function(state, X) {
      predictors <- as.data.frame(X)
      names(predictors) <- paste0("x", seq_len(ncol(X)))
      result <- stats::predict(state, new_data = predictors, type = "numeric")
      if (!is.data.frame(result) || !identical(names(result), ".pred") ||
          !is.numeric(result$.pred))
        stop("parsnip returned an unsupported regression prediction",
             call. = FALSE)
      as.numeric(result$.pred)
    }, name = paste0("parsnip:", spec$engine))
  controller$spec <- list(learner = "parsnip", engine = spec$engine)
  controller$model_spec <- spec
  controller
}

#' Optional mlr3 regression learner controller
#'
#' Clones the supplied learner before each fit so every fold owns independent
#' mutable state. DAG-ML, not mlr3, controls the CV/OOF/refit phases.
#' Only numeric one-target regression responses are currently supported.
#' @param learner An untrained `mlr3` `LearnerRegr` instance.
#' @export
nirs4all_mlr3 <- function(learner) {
  if (!requireNamespace("mlr3", quietly = TRUE))
    stop("Install the optional 'mlr3' package first", call. = FALSE)
  if (!inherits(learner, "LearnerRegr") ||
      !is.character(learner$id) || length(learner$id) != 1L ||
      is.na(learner$id) || !nzchar(learner$id) ||
      !is.null(learner$model))
    stop("learner must be an untrained mlr3 LearnerRegr", call. = FALSE)
  template <- learner$clone(deep = TRUE)
  controller <- nirs4all_controller(
    fit = function(X, y) {
      model <- template$clone(deep = TRUE)
      model$predict_type <- "response"
      frame <- as.data.frame(X)
      names(frame) <- paste0("x", seq_len(ncol(X)))
      frame$y <- y
      task <- mlr3::TaskRegr$new(id = "nirs4all_fit", backend = frame,
                                 target = "y")
      model$train(task)
      model
    },
    predict = function(state, X) {
      frame <- as.data.frame(X)
      names(frame) <- paste0("x", seq_len(ncol(X)))
      response <- state$predict_newdata(frame)$response
      if (!is.numeric(response))
        stop("mlr3 returned an unsupported regression prediction",
             call. = FALSE)
      as.numeric(response)
    }, name = paste0("mlr3:", template$id))
  controller$spec <- list(learner = "mlr3", engine = template$id)
  controller$model_spec <- template
  controller
}
