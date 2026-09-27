# Generic n4m role recipes (steps "n4m:<catalog method id>") and their trained
# envelope, version 8. The pipeline is n4m's native role pipeline (ABI 2.14):
# recipe validation, fit-input routing, feature-name checks, the per-step N4ME
# states, their consistency with the recipe and the training-row export policy
# are owned by n4m, identically in Python, R, JS/WASM and Rust. This file only
# writes and reads the envelope JSON: schema, recipe tokens, base64 states and
# their SHA-256, class names, and the additive feature names and training-row
# flags.

.nirs4all_role_prefix <- "n4m:"
.nirs4all_role_schema <- "nirs4all.n4m.trained_pipeline.v8"

# The recipe of a fitted role pipeline, as n4m role-pipeline steps. Only
# "n4m:<method id>" tokens are portable envelope steps.
nirs4all_role_steps <- function(pipeline) {
  lapply(pipeline, function(step) {
    token <- if (is.list(step)) step$class else step
    if (!is.character(token) || length(token) != 1L ||
        !startsWith(token, .nirs4all_role_prefix))
      stop("portable n4m role recipes contain n4m:<method id> steps only", call. = FALSE)
    list(method_id = substring(token, nchar(.nirs4all_role_prefix) + 1L),
         params = if (is.list(step) && !is.null(step$params)) step$params else list())
  })
}

#' Fit a recipe of generic n4m role steps
#'
#' Steps are `"n4m:<catalog method id>"` tokens (or `list(class = token,
#' params = list(...))`): sample filters (they drop training rows), then
#' transformers and selectors, then one regressor or classifier. The recipe is
#' an n4m native role pipeline: n4m validates it, fits every step, routes every
#' response column to the supervised steps, and stores the column names of `X`
#' (a matrix without column names is positional). The result predicts with
#' [nirs4all_predict()] and exports with [nirs4all_export_trained_pipeline()]
#' as a version 8 envelope.
#' @param recipe A recipe list, or JSON/YAML text or file (see
#'   [nirs4all_load_pipeline()]).
#' @param X Numeric samples-by-features matrix.
#' @param y Responses (a numeric vector, or a matrix with one row per sample)
#'   for a final regressor, or class labels for a final classifier. Their
#'   length must equal `nrow(X)`: nothing is recycled.
#' @return A fitted pipeline.
#' @export
nirs4all_fit_role_recipe <- function(recipe, X, y) {
  definition <- nirs4all_load_pipeline(recipe)
  pipeline <- n4m::n4m_role_pipeline(nirs4all_role_steps(definition$pipeline))
  nirs4all_role_fitted(definition, n4m::n4m_estimator_fit(pipeline, nirs4all_matrix(X), y))
}

nirs4all_role_fitted <- function(definition, pipeline) {
  steps <- n4m::n4m_role_pipeline_steps(pipeline)
  classification <- identical(steps$role[nrow(steps)], "classifier")
  info <- pipeline$state$info
  classes <- if (classification) {
    if (is.null(pipeline$state$levels)) as.character(info$classes) else pipeline$state$levels
  }
  structure(list(
    recipe = list(pipeline = definition$pipeline), role_pipeline = pipeline,
    learner = NULL, state = NULL, steps = list(), step_states = list(),
    preprocessing_owner = "n4m_roles",
    task = if (classification) "classification" else "regression",
    classes = classes, n_features = as.integer(info$n_features),
    feature_names = pipeline$state$feature_names),
    class = "nirs4all_fitted")
}

# Class labels, a numeric vector (one response) or a matrix (several).
nirs4all_role_predict <- function(object, X) {
  out <- stats::predict(object$role_pipeline, X)
  if (identical(object$task, "classification"))
    return(factor(as.character(out), levels = object$classes))
  out
}

nirs4all_export_trained_roles <- function(object, file, allow_training_rows) {
  steps <- n4m::n4m_role_pipeline_steps(object$role_pipeline)
  retained <- steps$method_id[steps$contains_training_rows]
  if (length(retained) && !isTRUE(allow_training_rows))
    stop(sprintf(paste0("the fitted state of %s keeps training rows; export it with ",
                        "allow_training_rows = TRUE only if those rows may be shared"),
                 paste(retained, collapse = ", ")), call. = FALSE)
  exported <- n4m::n4m_role_pipeline_export(object$role_pipeline, allow_training_rows)
  states <- lapply(exported, function(state) list(
    method_id = state$method_id,
    n4me_base64 = gsub("\n", "", jsonlite::base64_enc(state$n4me), fixed = TRUE),
    sha256 = digest::digest(state$n4me, algo = "sha256", serialize = FALSE),
    contains_training_rows = state$contains_training_rows))
  levels <- object$role_pipeline$state$levels
  if (!is.null(levels)) states[[length(states)]]$class_names <- as.list(levels)
  document <- list(schema = .nirs4all_role_schema, recipe = object$recipe,
                   n_features = object$n_features)
  if (!is.null(object$feature_names)) document$feature_names <- as.list(object$feature_names)
  document$states <- states
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
  n_features <- document$n_features
  if (!is.numeric(n_features) || length(n_features) != 1L || !is.finite(n_features))
    stop("a v8 envelope needs n_features", call. = FALSE)
  states <- document$states
  if (!is.list(states) || !length(states) || !all(vapply(states, is.list, logical(1))))
    stop("a v8 envelope lists one state per stateful recipe step", call. = FALSE)
  payloads <- lapply(states, function(state) {
    bytes <- jsonlite::base64_dec(state$n4me_base64)
    if (!identical(digest::digest(bytes, algo = "sha256", serialize = FALSE), state$sha256))
      stop(sprintf("N4ME state of %s fails its checksum", state$method_id), call. = FALSE)
    bytes
  })
  feature_names <- document$feature_names
  if (!is.null(feature_names)) {
    feature_names <- unlist(feature_names)
    if (!is.character(feature_names) || length(feature_names) != n_features)
      stop("envelope feature_names must name each of the n_features columns", call. = FALSE)
  }
  class_names <- states[[length(states)]]$class_names
  if (!is.null(class_names)) class_names <- as.character(unlist(class_names))
  pipeline <- n4m::n4m_role_pipeline_import(nirs4all_role_steps(definition$pipeline),
                                            payloads, feature_names, class_names)
  # The native import checks the states against the recipe; the envelope's own
  # fields must describe those states too.
  steps <- n4m::n4m_role_pipeline_steps(pipeline)
  stateful <- steps[steps$state_index >= 0L, , drop = FALSE]
  for (k in seq_along(states)) {
    if (!identical(states[[k]]$method_id, stateful$method_id[k]))
      stop(sprintf("envelope state %d is labelled %s but holds %s", k,
                   states[[k]]$method_id, stateful$method_id[k]), call. = FALSE)
    flag <- states[[k]]$contains_training_rows
    if (!is.null(flag) && !identical(flag, stateful$contains_training_rows[k]))
      stop(sprintf("envelope state %d (%s) misreports its training rows", k,
                   stateful$method_id[k]), call. = FALSE)
  }
  if (!identical(as.integer(n_features), as.integer(pipeline$state$info$n_features)))
    stop("envelope n_features differs from its states", call. = FALSE)
  if (!is.null(class_names) && length(class_names) != length(pipeline$state$info$classes))
    stop("envelope class_names do not match the classifier classes", call. = FALSE)
  nirs4all_role_fitted(definition, pipeline)
}
