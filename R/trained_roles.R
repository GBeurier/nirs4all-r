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
  info <- n4m::n4m_role_pipeline_info(pipeline)
  classes <- if (classification) as.character(info$classes)
  structure(list(
    recipe = list(pipeline = definition$pipeline), role_pipeline = pipeline,
    learner = NULL, state = NULL, steps = list(), step_states = list(),
    preprocessing_owner = "n4m_roles",
    task = if (classification) "classification" else "regression",
    classes = classes, n_features = as.integer(info$n_features),
    feature_names = info$feature_names),
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
  # Label names travel in the envelope (N4ME holds integer class ids only).
  labels <- n4m::n4m_role_pipeline_info(object$role_pipeline)$label_names
  if (!is.null(labels))
    states[[length(states)]]$class_names <- as.list(nirs4all_role_check_labels(
      as.list(labels), nirs4all_role_class_ids(nirs4all_role_steps(object$recipe$pipeline),
                                               exported)))
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
  # A JSON integer (jsonlite reads it as an R integer): never a fraction, a
  # string or a boolean, and compared exactly to the native width.
  n_features <- document$n_features
  if (!is.integer(n_features) || length(n_features) != 1L || is.na(n_features) ||
      n_features < 1L)
    stop("a v8 envelope needs n_features as a positive JSON integer", call. = FALSE)
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
  steps <- nirs4all_role_steps(definition$pipeline)
  class_names <- states[[length(states)]]$class_names
  if (!is.null(class_names))
    class_names <- nirs4all_role_check_labels(class_names,
                                              nirs4all_role_class_ids(steps, payloads))
  pipeline <- n4m::n4m_role_pipeline_import(steps, payloads, feature_names, class_names)
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
  info <- n4m::n4m_role_pipeline_info(pipeline)
  if (!identical(n_features, info$n_features))
    stop("envelope n_features differs from its states", call. = FALSE)
  nirs4all_role_fitted(definition, pipeline)
}

# The native class IDs of a fitted classifier pipeline (NULL for a regressor):
# its states imported without a label table report them unmapped.
nirs4all_role_class_ids <- function(steps, states)
  n4m::n4m_role_pipeline_info(n4m::n4m_role_pipeline_import(steps, states))$classes

# The class label table of a v8 envelope, checked on import and export (shared
# label contract): a non-empty list of unique strings or finite numbers indexed
# by the native class IDs, every one of which names a slot. The table may hold
# more labels than the fitted classes (labels whose rows a filter removed).
nirs4all_role_check_labels <- function(labels, ids) {
  if (is.null(ids))
    stop("envelope class_names label a pipeline that does not end with a classifier",
         call. = FALSE)
  if (!is.list(labels) || !length(labels))
    stop("envelope class_names must be a non-empty list of labels", call. = FALSE)
  valid <- vapply(labels, function(label) length(label) == 1L &&
    ((is.character(label) && !is.na(label)) || (is.numeric(label) && is.finite(label))),
    logical(1))
  if (!all(valid))
    stop(sprintf("envelope class_names entry %d is not a string or a finite number",
                 which(!valid)[1L]), call. = FALSE)
  if (length(unique(vapply(labels, is.character, logical(1)))) > 1L)
    stop("envelope class_names mixes strings and numbers", call. = FALSE)
  names <- vapply(labels, as.character, character(1))
  if (anyDuplicated(names))
    stop(sprintf("envelope class_names repeat the label '%s'", names[anyDuplicated(names)]),
         call. = FALSE)
  outside <- ids[ids < 0 | ids >= length(names) | ids != floor(ids)]
  if (length(outside))
    stop(sprintf("the classifier has class id %s but envelope class_names has %d labels",
                 format(outside[1L]), length(names)), call. = FALSE)
  names
}
