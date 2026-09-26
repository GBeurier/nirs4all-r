#' Optional XGBoost regression controller
#'
#' Fits a fresh CPU booster in each training scope. The booster is stored as
#' XGBoost model bytes within the R state, so RDS replay does not depend on
#' serializing an external pointer. This is an R model sidecar, not an N4MM
#' model or a portable pipeline recipe.
#' @param nrounds Positive number of boosting rounds.
#' @param max_depth Non-negative tree depth (zero means no depth limit).
#' @param eta Learning rate in (0, 1].
#' @param seed Non-negative XGBoost seed.
#' @param nthread Positive number of CPU threads.
#' @export
nirs4all_xgboost <- function(nrounds = 100L, max_depth = 6L, eta = 0.3,
                            seed = 1L, nthread = 1L) {
  nirs4all_xgboost_check(nrounds, max_depth, eta, seed, nthread)
  params <- list(objective = "reg:squarederror", max_depth = as.integer(max_depth),
                 eta = eta, seed = as.integer(seed), nthread = as.integer(nthread))
  controller <- nirs4all_controller(
    fit = function(X, y) {
      data <- xgboost::xgb.DMatrix(X, label = y, nthread = as.integer(nthread))
      booster <- xgboost::xgb.train(params = params, data = data,
                                   nrounds = as.integer(nrounds), verbose = 0L)
      list(model = xgboost::xgb.save.raw(booster), n_features = ncol(X))
    },
    predict = function(state, X) {
      nirs4all_xgboost_predict(state, X, nthread)
    }, name = "xgboost:regression")
  controller$spec <- list(learner = "xgboost", nrounds = as.integer(nrounds),
                          max_depth = as.integer(max_depth), eta = eta,
                          seed = as.integer(seed), nthread = as.integer(nthread))
  controller
}

#' Optional XGBoost probability classifier
#'
#' Uses binary logistic or multiclass soft probabilities according to the
#' training fold's factor levels. Every training fold must contain all classes.
#' @inheritParams nirs4all_xgboost
#' @export
nirs4all_xgboost_classifier <- function(nrounds = 100L, max_depth = 6L,
                                       eta = 0.3, seed = 1L, nthread = 1L) {
  nirs4all_xgboost_check(nrounds, max_depth, eta, seed, nthread)
  probabilities <- function(state, X) {
    values <- nirs4all_xgboost_predict(state, X, nthread)
    if (length(state$classes) == 2L) {
      if (!is.numeric(values) || length(values) != nrow(X))
        stop("xgboost returned invalid binary probabilities", call. = FALSE)
      values <- cbind(1 - values, values)
    } else if (!is.matrix(values) ||
               !identical(dim(values), c(nrow(X), length(state$classes)))) {
      stop("xgboost returned invalid multiclass probabilities", call. = FALSE)
    }
    if (anyNA(values) || any(!is.finite(values)) ||
        any(values < 0 | values > 1) ||
        any(abs(rowSums(values) - 1) > 1e-5))
      stop("xgboost returned invalid class probabilities", call. = FALSE)
    colnames(values) <- state$classes
    values
  }
  controller <- nirs4all_controller(
    fit = function(X, y) {
      if (!is.factor(y) || nlevels(y) < 2L ||
          any(tabulate(as.integer(y), nbins = nlevels(y)) == 0L))
        stop("each class must occur in the training fold", call. = FALSE)
      params <- list(objective = if (nlevels(y) == 2L) "binary:logistic" else
                       "multi:softprob", max_depth = as.integer(max_depth),
                     eta = eta, seed = as.integer(seed),
                     nthread = as.integer(nthread))
      if (nlevels(y) > 2L) params$num_class <- nlevels(y)
      data <- xgboost::xgb.DMatrix(X, label = as.integer(y) - 1L,
                                   nthread = as.integer(nthread))
      booster <- xgboost::xgb.train(params = params, data = data,
                                   nrounds = as.integer(nrounds), verbose = 0L)
      list(model = xgboost::xgb.save.raw(booster), n_features = ncol(X),
           classes = levels(y))
    },
    predict = function(state, X) {
      values <- probabilities(state, X)
      factor(state$classes[max.col(values, ties.method = "first")],
             levels = state$classes)
    }, predict_proba = probabilities, task = "classification",
    name = "xgboost:classification")
  controller$spec <- list(learner = "xgboost_classifier",
                          nrounds = as.integer(nrounds),
                          max_depth = as.integer(max_depth), eta = eta,
                          seed = as.integer(seed), nthread = as.integer(nthread))
  controller
}

nirs4all_xgboost_predict <- function(state, X, nthread) {
  if (!is.list(state) || !is.raw(state$model) ||
      !identical(ncol(X), state$n_features))
    stop("xgboost state or feature count is invalid", call. = FALSE)
  booster <- xgboost::xgb.load.raw(state$model)
  data <- xgboost::xgb.DMatrix(X, nthread = as.integer(nthread))
  stats::predict(booster, newdata = data)
}

nirs4all_xgboost_check <- function(nrounds, max_depth, eta, seed, nthread) {
  if (!requireNamespace("xgboost", quietly = TRUE))
    stop("Install the optional 'xgboost' package first", call. = FALSE)
  integer_arg <- function(value, minimum, name) {
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        value < minimum || value > .Machine$integer.max || value != floor(value))
      stop(sprintf("%s must be an integer of at least %d", name, minimum),
           call. = FALSE)
  }
  integer_arg(nrounds, 1L, "nrounds")
  integer_arg(max_depth, 0L, "max_depth")
  integer_arg(seed, 0L, "seed")
  integer_arg(nthread, 1L, "nthread")
  if (!is.numeric(eta) || length(eta) != 1L || !is.finite(eta) ||
      eta <= 0 || eta > 1)
    stop("eta must be in (0, 1]", call. = FALSE)
  invisible(NULL)
}
