# Generic n4m role recipes (steps "n4m:<catalog method id>") and their trained
# envelope, version 7: each fitted step travels as its native N4ME state, so a
# pipeline trained in Python, R, JS/WASM or Rust predicts identically in the
# others. Numerics and parameter validation stay in n4m.

.nirs4all_role_prefix <- "n4m:"
.nirs4all_role_schema <- "nirs4all.n4m.trained_pipeline.v7"

# Fit-input requirements of every n4m method, read once from the manifest.
.nirs4all_role_inputs <- local({
  cache <- NULL
  function(method_id) {
    if (is.null(cache)) {
      methods <- jsonlite::fromJSON(n4m::n4m_manifest_json(), simplifyVector = FALSE)$methods
      cache <<- stats::setNames(lapply(methods, `[[`, "inputs"),
                                vapply(methods, `[[`, "", "method_id"))
    }
    cache[[method_id]]
  }
})

nirs4all_role_token <- function(step) {
  token <- if (is.list(step)) step$class else step
  if (!is.character(token) || length(token) != 1L ||
      !startsWith(token, .nirs4all_role_prefix))
    stop("portable n4m role recipes contain n4m:<method id> steps only", call. = FALSE)
  list(method_id = substring(token, nchar(.nirs4all_role_prefix) + 1L),
       params = if (is.list(step) && !is.null(step$params)) step$params else list())
}

nirs4all_role_step <- function(step) {
  token <- nirs4all_role_token(step)
  do.call(n4m::n4m_constructor(token$method_id), token$params)
}

nirs4all_role_transform <- function(estimators, X) {
  for (est in estimators[-length(estimators)])
    X <- n4m::n4m_estimator_transform(est, X)
  X
}

#' Fit a recipe of generic n4m role steps
#'
#' Steps are `"n4m:<catalog method id>"` tokens (or `list(class = token,
#' params = list(...))`): sample filters (they drop training rows), then
#' transformers and selectors, then one regressor or classifier. Every step is
#' fitted by n4m; the result predicts with [nirs4all_predict()] and exports
#' with [nirs4all_export_trained_pipeline()] as a version 7 envelope.
#' @param recipe A recipe list, or JSON/YAML text or file (see
#'   [nirs4all_load_pipeline()]).
#' @param X Numeric samples-by-features matrix.
#' @param y Numeric targets (regression) or class labels (classification).
#' @return A fitted pipeline.
#' @export
nirs4all_fit_role_recipe <- function(recipe, X, y) {
  definition <- nirs4all_load_pipeline(recipe)
  X <- nirs4all_matrix(X)
  n_features <- ncol(X)
  steps <- lapply(definition$pipeline, nirs4all_role_step)
  model <- steps[[length(steps)]]
  if (!inherits(model, "n4m_regressor") && !inherits(model, "n4m_classifier"))
    stop("a portable n4m role recipe ends with one regressor or classifier", call. = FALSE)
  fitted <- list()
  for (step in steps[-length(steps)]) {
    # Intermediate steps see y only when their method needs it.
    step_y <- if (identical(.nirs4all_role_inputs(step$method_id)$y, "required")) y
    if (inherits(step, "n4m_sample_filter")) {
      keep <- n4m::n4m_sample_mask(n4m::n4m_estimator_fit(step, X, step_y), X, step_y)
      X <- X[keep, , drop = FALSE]
      y <- y[keep]
    } else if (inherits(step, "n4m_transformer") || inherits(step, "n4m_selector")) {
      step <- n4m::n4m_estimator_fit(step, X, step_y)
      X <- n4m::n4m_estimator_transform(step, X)
      fitted[[length(fitted) + 1L]] <- step
    } else {
      stop(sprintf("%s is not a portable pipeline step", step$method_id), call. = FALSE)
    }
  }
  classification <- inherits(model, "n4m_classifier")
  model <- n4m::n4m_estimator_fit(model, X, y)
  nirs4all_role_fitted(definition, c(fitted, list(model)), n_features, classification)
}

nirs4all_role_fitted <- function(definition, estimators, n_features, classification) {
  model <- estimators[[length(estimators)]]
  classes <- if (classification) as.character(n4m::n4m_classes(model))
  structure(list(
    recipe = list(pipeline = definition$pipeline), role_estimators = estimators,
    learner = list(predict = function(state, X) {
      out <- stats::predict(model, X)
      if (classification) factor(as.character(out), levels = classes) else out
    }),
    state = NULL, steps = list(), step_states = list(),
    preprocessing_owner = "n4m_roles",
    task = if (classification) "classification" else "regression",
    classes = classes, n_features = n_features, feature_names = NULL),
    class = "nirs4all_fitted")
}

nirs4all_export_trained_roles <- function(object, file = NULL) {
  states <- lapply(object$role_estimators, function(est) {
    bytes <- n4m::n4m_estimator_export(est)
    state <- list(method_id = est$method_id,
      n4me_base64 = gsub("\n", "", jsonlite::base64_enc(bytes), fixed = TRUE),
      sha256 = digest::digest(bytes, algo = "sha256", serialize = FALSE))
    levels <- est$state$levels
    if (!is.null(levels)) state$class_names <- as.list(levels)
    state
  })
  document <- list(schema = .nirs4all_role_schema, recipe = object$recipe,
                   n_features = object$n_features, states = states)
  value <- as.character(jsonlite::toJSON(document, auto_unbox = TRUE,
                                         null = "null", digits = NA, pretty = TRUE))
  if (!is.null(file)) {
    writeLines(value, file, useBytes = TRUE)
    return(invisible(value))
  }
  value
}

nirs4all_import_trained_roles <- function(document) {
  definition <- nirs4all_load_pipeline(document$recipe)
  steps <- lapply(definition$pipeline, nirs4all_role_step)
  stateful <- Filter(function(step) !inherits(step, "n4m_sample_filter"), steps)
  if (length(document$states) != length(stateful))
    stop("envelope states do not match the recipe steps", call. = FALSE)
  estimators <- Map(function(step, state) {
    bytes <- jsonlite::base64_dec(state$n4me_base64)
    if (!identical(digest::digest(bytes, algo = "sha256", serialize = FALSE), state$sha256))
      stop(sprintf("N4ME state of %s fails its checksum", state$method_id), call. = FALSE)
    est <- n4m::n4m_estimator_import(bytes)
    if (!identical(est$method_id, state$method_id) ||
        !identical(est$method_id, step$method_id))
      stop(sprintf("N4ME state %s does not match its recipe step", state$method_id),
           call. = FALSE)
    if (!is.null(state$class_names)) est$state$levels <- unlist(state$class_names)
    est
  }, stateful, document$states)
  model <- estimators[[length(estimators)]]
  nirs4all_role_fitted(definition, unname(estimators), as.integer(document$n_features),
                       inherits(model, "n4m_classifier"))
}
