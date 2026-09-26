#' Export a trained native pipeline for R/Python replay
#'
#' Serializes a portable n4m recipe, fitted MSC/EMSC references and the exact
#' N4MM model payload in one JSON document. No RDS, Python pickle or executable
#' code is included. Version 1 covers n4m PLS regression; version 2 adds
#' sparse PLS-DA with ordered class labels and a native affine N4MM predictor.
#' Version 3 covers PLS regression with train-fitted external SPA selection.
#' Version 4 covers PLS regression with external native `n4m.Selector` steps.
#' Version 5 carries a prediction-only affine N4MM model plus an asserted
#' recipe for refitting one of fourteen native MethodResult regressors; the
#' N4MM payload does not attest which fitting method produced it.
#' The JSON recipe also permits fitting the pipeline again where its model
#' alias is supported.
#' @param object A fitted native n4m PLS, affine MethodResult or sparse
#'   PLS-DA pipeline.
#' @param file Optional output path. If omitted, returns JSON text.
#' @return JSON text, invisibly when `file` is supplied.
#' @export
nirs4all_export_trained_pipeline <- function(object, file = NULL) {
  classification <- inherits(object, "nirs4all_fitted") &&
    identical(object$learner$format, "n4mm_sparse_pls_da") &&
    identical(object$task, "classification")
  affine <- inherits(object, "nirs4all_fitted") &&
    identical(object$learner$format, "n4mm_affine") &&
    identical(object$task, "regression")
  if (!inherits(object, "nirs4all_fitted") ||
      !(classification || affine || (identical(object$learner$format, "n4mm") &&
        identical(object$task, "regression"))))
    stop("trained export requires a native n4m PLS, affine, or sparse PLS-DA pipeline",
         call. = FALSE)
  recipe_pipeline <- structure(list(steps = object$steps,
    learner = object$learner), class = "nirs4all_pipeline")
  selected <- nirs4all_portable_has_spa(object$steps)
  generic <- nirs4all_portable_has_selector(object$steps)
  if (selected && generic)
    stop("trained selector schemas cannot mix SPA and n4m.Selector", call. = FALSE)
  if (classification && (selected || generic))
    stop("trained selector export requires PLS regression", call. = FALSE)
  recipe <- jsonlite::fromJSON(nirs4all_export_pipeline(recipe_pipeline),
                              simplifyVector = FALSE)
  width <- as.integer(object$n_features)
  if (length(width) != 1L || is.na(width) || width < 2L)
    stop("invalid fitted input width", call. = FALSE)
  if (!is.null(object$feature_names) &&
      (length(object$feature_names) != width ||
       anyNA(object$feature_names) ||
       any(!nzchar(object$feature_names)) ||
       anyDuplicated(object$feature_names)))
    stop("invalid fitted feature names", call. = FALSE)
  owner <- object$preprocessing_owner
  if (!(owner %in% c("external_r", "embedded_methods")))
    stop("unsupported preprocessing ownership", call. = FALSE)
  if (classification && (!identical(owner, "external_r") ||
      !is.character(object$classes) || length(object$classes) < 2L ||
      anyNA(object$classes) || any(!nzchar(object$classes)) ||
      anyDuplicated(object$classes) ||
      !identical(object$classes, object$state$classes)))
    stop("invalid sparse PLS-DA class state", call. = FALSE)
  if (affine && !identical(owner, "external_r"))
    stop("affine trained export requires external preprocessing", call. = FALSE)
  state <- nirs4all_portable_encode_states(object$steps, object$step_states,
    width, owner, allow_selector = selected, allow_generic = generic)
  bytes <- if (affine) nirs4all_affine_export(object$state) else
    n4m::n4m_model_export(if (classification)
      object$state$native_model else object$state)
  nirs4all_portable_validate_model(bytes, recipe_pipeline, width,
                                   state$output_width, owner,
                                   if (classification) object$classes else NULL)
  manifest <- list(recipe = recipe, input_n_features = width,
    feature_names = if (is.null(object$feature_names)) NULL else
      unname(as.list(object$feature_names)),
    preprocessing_owner = if (identical(owner, "external_r")) "external" else owner,
    step_states = state$states)
  if (classification) {
    manifest$task <- "classification"
    manifest$classes <- unname(as.list(object$classes))
  }
  if (affine)
    manifest$fit_recipe_assertion <- list(kind = "affine_recipe",
      recipe_class = unname(.nirs4all_portable_affine[[
        object$learner$spec$method]]))
  if (affine && identical(object$learner$spec$method, "mb_pls"))
    manifest$fit_recipe_assertion$block_sizes <- unname(as.list(
      object$learner$spec$params$block_sizes))
  manifest_json <- as.character(jsonlite::toJSON(manifest,
    auto_unbox = TRUE, null = "null", digits = 17L))
  document <- list(schema = if (classification)
    "nirs4all.n4m.trained_pipeline.v2" else if (affine)
    "nirs4all.n4m.trained_pipeline.v5" else if (selected)
    "nirs4all.n4m.trained_pipeline.v3" else if (generic)
    "nirs4all.n4m.trained_pipeline.v4" else
    "nirs4all.n4m.trained_pipeline.v1",
    manifest_json = manifest_json,
    manifest_sha256 = digest::digest(manifest_json, algo = "sha256",
                                     serialize = FALSE),
    model = list(kind = "n4m_model", encoding = "base64-n4mm",
      sha256 = digest::digest(bytes, algo = "sha256", serialize = FALSE),
      payload = gsub("\n", "", jsonlite::base64_enc(bytes), fixed = TRUE)))
  value <- as.character(jsonlite::toJSON(document, auto_unbox = TRUE,
                                         null = "null", digits = 17L,
                                         pretty = TRUE))
  if (!is.null(file)) {
    if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file))
      stop("file must be a non-empty path", call. = FALSE)
    writeLines(value, file, useBytes = TRUE)
    return(invisible(value))
  }
  value
}

#' Import a trained native pipeline from R or Python
#'
#' Validates the closed portable envelope, recipe, fitted references, payload
#' hash and native N4MM descriptor before returning a predictor. The source
#' must be trusted as model data; no host-language code is deserialized.
#' @param source JSON text or a path to a JSON document.
#' @return A fitted pipeline accepted by [nirs4all_predict()].
#' @export
nirs4all_import_trained_pipeline <- function(source) {
  if (!is.character(source) || length(source) != 1L || is.na(source))
    stop("source must be JSON text or a path", call. = FALSE)
  input <- if (file.exists(source)) paste(readLines(source, warn = FALSE),
                                          collapse = "\n") else source
  document <- jsonlite::fromJSON(input, simplifyVector = FALSE)
  if (!is.list(document) ||
      !setequal(names(document), c("schema", "manifest_json",
        "manifest_sha256", "model")) ||
      !is.character(document$schema) || length(document$schema) != 1L ||
      !(document$schema %in% c("nirs4all.n4m.trained_pipeline.v1",
                               "nirs4all.n4m.trained_pipeline.v2",
                               "nirs4all.n4m.trained_pipeline.v3",
                               "nirs4all.n4m.trained_pipeline.v4",
                               "nirs4all.n4m.trained_pipeline.v5")))
    stop("unsupported trained pipeline envelope", call. = FALSE)
  classification <- identical(document$schema, "nirs4all.n4m.trained_pipeline.v2")
  selected_schema <- identical(document$schema,
                               "nirs4all.n4m.trained_pipeline.v3")
  generic_schema <- identical(document$schema,
                              "nirs4all.n4m.trained_pipeline.v4")
  affine_schema <- identical(document$schema,
                             "nirs4all.n4m.trained_pipeline.v5")
  if (!is.character(document$manifest_json) ||
      length(document$manifest_json) != 1L ||
      !is.character(document$manifest_sha256) ||
      length(document$manifest_sha256) != 1L ||
      !grepl("^[0-9a-f]{64}$", document$manifest_sha256) ||
      !identical(digest::digest(document$manifest_json, algo = "sha256",
                               serialize = FALSE), document$manifest_sha256))
    stop("trained pipeline manifest hash mismatch", call. = FALSE)
  manifest <- jsonlite::fromJSON(document$manifest_json, simplifyVector = FALSE)
  required <- c("recipe", "input_n_features", "feature_names",
                "preprocessing_owner", "step_states")
  if (classification) required <- c(required, "task", "classes")
  if (affine_schema) required <- c(required, "fit_recipe_assertion")
  if (!is.list(manifest) || !setequal(names(manifest), required))
    stop("invalid trained pipeline manifest", call. = FALSE)
  classes <- NULL
  if (classification) {
    values <- manifest$classes
    if (!identical(manifest$task, "classification") || !is.list(values) ||
        length(values) < 2L || !all(vapply(values, function(value)
          is.character(value) && length(value) == 1L && !is.na(value) &&
            nzchar(value), logical(1))))
      stop("invalid ordered classification classes", call. = FALSE)
    classes <- unlist(values, use.names = FALSE)
    if (anyDuplicated(classes))
      stop("duplicate classification classes", call. = FALSE)
  }
  width <- manifest$input_n_features
  if (!is.numeric(width) || length(width) != 1L || is.na(width) ||
      width != as.integer(width) || width < 2L)
    stop("invalid input feature width", call. = FALSE)
  width <- as.integer(width)
  feature_names <- NULL
  if (!is.null(manifest$feature_names)) {
    values <- manifest$feature_names
    if (!is.list(values) || length(values) != width ||
        !all(vapply(values, function(x)
          is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x),
          logical(1))))
      stop("invalid feature names", call. = FALSE)
    feature_names <- unlist(values, use.names = FALSE)
    if (anyDuplicated(feature_names))
      stop("duplicate feature names", call. = FALSE)
  }
  pipeline <- nirs4all_pipeline_from_portable(manifest$recipe)
  if (classification != identical(pipeline$learner$spec$learner,
                                  "sparse_pls_da"))
    stop("trained model task differs from recipe", call. = FALSE)
  method <- pipeline$learner$spec$method
  expected_class <- if (affine_schema &&
    identical(pipeline$learner$spec$learner, "n4m_method"))
    unname(.nirs4all_portable_affine[[method]]) else NULL
  if (affine_schema) {
    assertion <- manifest$fit_recipe_assertion
    recipe_steps <- manifest$recipe$pipeline
    recipe_model <- if (is.list(recipe_steps) && length(recipe_steps))
      recipe_steps[[length(recipe_steps)]]$model else NULL
    if (is.null(expected_class) ||
        !identical(pipeline$learner$format, "n4mm_affine") ||
        !is.list(assertion) ||
        !setequal(names(assertion), c("kind", "recipe_class",
          if (identical(method, "mb_pls")) "block_sizes" else character())) ||
        !identical(assertion$kind, "affine_recipe") ||
        !identical(assertion$recipe_class, expected_class) ||
        (identical(method, "mb_pls") &&
         !identical(assertion$block_sizes, unname(as.list(
           pipeline$learner$spec$params$block_sizes)))) ||
        !identical(recipe_model$class, expected_class) ||
        (!identical(method, "ridge") &&
         is.null(recipe_model$params$n_components)))
      stop("invalid affine fit recipe assertion", call. = FALSE)
  } else if (identical(pipeline$learner$spec$learner, "n4m_method")) {
    stop("affine recipe requires trained schema v5", call. = FALSE)
  }
  has_spa <- nirs4all_portable_has_spa(pipeline$steps)
  has_generic <- nirs4all_portable_has_selector(pipeline$steps)
  if (has_spa && has_generic)
    stop("trained selector schemas cannot mix SPA and n4m.Selector", call. = FALSE)
  if (!affine_schema && selected_schema != has_spa)
    stop("trained selector schema differs from recipe", call. = FALSE)
  if (!affine_schema && generic_schema != has_generic)
    stop("trained generic selector schema differs from recipe", call. = FALSE)
  wire_owner <- manifest$preprocessing_owner
  if (!(is.character(wire_owner) && length(wire_owner) == 1L &&
        wire_owner %in% c("external", "embedded_methods")))
    stop("unsupported preprocessing ownership", call. = FALSE)
  owner <- if (identical(wire_owner, "external")) "external_r" else wire_owner
  if (classification && !identical(owner, "external_r"))
    stop("sparse PLS-DA requires external preprocessing", call. = FALSE)
  if (selected_schema && !identical(owner, "external_r"))
    stop("trained SPA requires external preprocessing", call. = FALSE)
  if (generic_schema && !identical(owner, "external_r"))
    stop("trained n4m.Selector requires external preprocessing", call. = FALSE)
  if (affine_schema && !identical(owner, "external_r"))
    stop("trained affine model requires external preprocessing", call. = FALSE)
  state <- nirs4all_portable_decode_states(pipeline$steps,
    manifest$step_states, width, owner,
    allow_selector = selected_schema || (affine_schema && has_spa),
    allow_generic = generic_schema || (affine_schema && has_generic))
  model <- document$model
  if (!is.list(model) || !setequal(names(model), c("kind", "encoding",
      "sha256", "payload")) || !identical(model$kind, "n4m_model") ||
      !identical(model$encoding, "base64-n4mm") ||
      !is.character(model$payload) || length(model$payload) != 1L ||
      !is.character(model$sha256) || length(model$sha256) != 1L ||
      !grepl("^[0-9a-f]{64}$", model$sha256))
    stop("invalid native model payload metadata", call. = FALSE)
  bytes <- jsonlite::base64_dec(model$payload)
  if (!length(bytes) || !identical(digest::digest(bytes, algo = "sha256",
      serialize = FALSE), model$sha256))
    stop("native model payload hash mismatch", call. = FALSE)
  nirs4all_portable_validate_model(bytes, pipeline, width,
                                   state$output_width, owner, classes)
  native <- n4m::n4m_model_import(bytes)
  structure(list(steps = pipeline$steps, learner = pipeline$learner,
    state = if (classification) list(native_model = native,
      classes = classes) else if (affine_schema)
      list(native_model = native) else native,
    step_states = state$states,
    preprocessing_owner = owner,
    task = if (classification) "classification" else "regression",
    classes = classes,
    n_features = width, feature_names = feature_names),
    class = "nirs4all_fitted")
}

nirs4all_portable_reference <- function(value, width, label) {
  if (!is.list(value) || length(value) != width ||
      !all(vapply(value, function(x)
        is.numeric(x) && length(x) == 1L && is.finite(x), logical(1))))
    stop(sprintf("invalid fitted %s reference", label), call. = FALSE)
  unlist(value, use.names = FALSE)
}

nirs4all_portable_has_spa <- function(steps) {
  any(vapply(steps, function(step)
    identical(step$kind, "spa") ||
      (identical(step$kind, "concat") &&
        any(vapply(step$branches, nirs4all_portable_has_spa, logical(1)))),
    logical(1)))
}

nirs4all_portable_has_selector <- function(steps) {
  any(vapply(steps, function(step)
    identical(step$kind, "n4m_selector") ||
      (identical(step$kind, "concat") &&
        any(vapply(step$branches, nirs4all_portable_has_selector, logical(1)))),
    logical(1)))
}

nirs4all_portable_selector_state <- function(state, width, top_k, decode) {
  if (decode) {
    if (!is.list(state) ||
        !identical(sort(names(state)), sort(c("kind", "selected_indices"))) ||
        !identical(state$kind, "selector") ||
        !is.list(state$selected_indices) ||
        !length(state$selected_indices) ||
        length(state$selected_indices) > width ||
        (!is.null(top_k) && length(state$selected_indices) != top_k) ||
        !all(vapply(state$selected_indices, function(value)
          is.numeric(value) && length(value) == 1L &&
            is.finite(value) && value == floor(value), logical(1))))
      stop("invalid fitted selector state", call. = FALSE)
    indices <- unlist(state$selected_indices, use.names = FALSE)
    if (any(indices < 0 | indices >= width) || anyDuplicated(indices))
      stop("invalid fitted selector selected_indices", call. = FALSE)
    return(as.integer(indices) + 1L)
  }
  if (!is.integer(state) || !length(state) || length(state) > width ||
      (!is.null(top_k) && length(state) != top_k) ||
      anyNA(state) || any(state < 1L | state > width) ||
      anyDuplicated(state))
    stop("invalid fitted selector selected_indices", call. = FALSE)
  list(kind = "selector",
       selected_indices = unname(as.list(as.integer(state) - 1L)))
}

nirs4all_portable_states <- function(steps, states, width, decode = FALSE,
                                    allow_selector = FALSE,
                                    allow_generic = FALSE) {
  if (!is.list(states) || length(states) != length(steps))
    stop("fitted preprocessing state does not match recipe", call. = FALSE)
  output <- vector("list", length(steps))
  for (index in seq_along(steps)) {
    step <- steps[[index]]
    state <- states[[index]]
    if (step$kind %in% c("msc", "emsc")) {
      label <- toupper(step$kind)
      if (decode) {
        if (!is.list(state) || !setequal(names(state), c("kind", "reference")) ||
            !identical(state$kind, label))
          stop(sprintf("invalid fitted %s state", label), call. = FALSE)
        output[[index]] <- nirs4all_portable_reference(state$reference,
                                                       width, label)
      } else {
        if (!is.numeric(state) || length(state) != width ||
            anyNA(state) || any(!is.finite(state)))
          stop(sprintf("missing fitted %s reference", label), call. = FALSE)
        output[[index]] <- list(kind = label,
                                reference = unname(as.list(as.numeric(state))))
      }
    } else if (identical(step$kind, "spa")) {
      if (!allow_selector)
        stop("trained SPA requires selector schema v3", call. = FALSE)
      output[[index]] <- nirs4all_portable_selector_state(state, width,
        step$top_k, decode)
      width <- step$top_k
    } else if (identical(step$kind, "n4m_selector")) {
      if (!allow_generic)
        stop("trained n4m.Selector requires selector schema v4", call. = FALSE)
      selected <- nirs4all_portable_selector_state(state, width,
        NULL, decode)
      output[[index]] <- selected
      width <- if (decode) length(selected) else length(state)
    } else if (identical(step$kind, "concat")) {
      branches <- step$branches
      value <- if (decode) {
        if (!is.list(state) || !setequal(names(state), c("kind", "branches")) ||
            !identical(state$kind, "branch"))
          stop("invalid fitted branch state", call. = FALSE)
        state$branches
      } else state
      if (!is.list(value) || !identical(names(value), names(branches)))
        stop("fitted branch states do not match recipe", call. = FALSE)
      branch_states <- lapply(names(branches), function(name)
        nirs4all_portable_states(branches[[name]], value[[name]], width,
                                decode = decode,
                                allow_selector = allow_selector,
                                allow_generic = allow_generic))
      names(branch_states) <- names(branches)
      output[[index]] <- if (decode)
        lapply(branch_states, `[[`, "states") else
        list(kind = "branch", branches = lapply(branch_states, `[[`, "states"))
      width <- sum(vapply(branch_states, `[[`, integer(1), "output_width"))
    } else {
      if (!is.null(state))
        stop("stateless preprocessing has unexpected fitted state", call. = FALSE)
      output[index] <- list(NULL)
    }
  }
  list(states = output, output_width = as.integer(width))
}

nirs4all_portable_encode_states <- function(steps, states, width, owner,
                                           allow_selector = FALSE,
                                           allow_generic = FALSE) {
  if (identical(owner, "embedded_methods")) {
    if (!is.list(states) || length(states) != length(steps) ||
        !all(vapply(states, is.null, logical(1))))
      stop("embedded preprocessing must not carry external state", call. = FALSE)
    return(list(states = states, output_width = width))
  }
  nirs4all_portable_states(steps, states, width,
    allow_selector = allow_selector, allow_generic = allow_generic)
}

nirs4all_portable_decode_states <- function(steps, states, width, owner,
                                           allow_selector = FALSE,
                                           allow_generic = FALSE) {
  if (identical(owner, "embedded_methods")) {
    if (!is.list(states) || length(states) != length(steps) ||
        !all(vapply(states, is.null, logical(1))))
      stop("embedded preprocessing must not carry external state", call. = FALSE)
    return(list(states = states, output_width = width))
  }
  nirs4all_portable_states(steps, states, width, decode = TRUE,
    allow_selector = allow_selector, allow_generic = allow_generic)
}

nirs4all_portable_validate_model <- function(bytes, pipeline, input_width,
                                             model_width, owner,
                                             classes = NULL) {
  descriptor <- n4m::n4m_model_descriptor(bytes)
  embedded <- identical(owner, "embedded_methods")
  if (identical(pipeline$learner$spec$learner, "n4m_method")) {
    spec <- pipeline$learner$spec
    if (embedded || !identical(pipeline$learner$format, "n4mm_affine") ||
        (identical(spec$method, "mb_pls") &&
         sum(as.double(spec$params$block_sizes)) != model_width) ||
        (identical(spec$method, "n_pls") &&
         as.double(spec$params$mode_j) * as.double(spec$params$mode_k) !=
           model_width) ||
        !identical(descriptor$format_version, 1L) ||
        !identical(descriptor$algorithm, 11L) ||
        !identical(descriptor$solver, 0L) ||
        !identical(descriptor$deflation, 0L) ||
        !identical(descriptor$n_components, 0L) ||
        !identical(descriptor$n_targets, 1L) ||
        !identical(descriptor$n_features, model_width) ||
        !identical(as.integer(descriptor$capabilities), 5L) ||
        isTRUE(n4m::n4m_model_pipeline_info(bytes)$present))
      stop("N4MM affine predictor does not match trained recipe dimensions",
           call. = FALSE)
    return(invisible(descriptor))
  }
  if (!is.null(classes)) {
    if (embedded || !identical(pipeline$learner$spec$learner,
                               "sparse_pls_da") ||
        !identical(descriptor$format_version, 1L) ||
        !identical(descriptor$algorithm, 11L) ||
        !identical(descriptor$solver, 0L) ||
        !identical(descriptor$deflation, 0L) ||
        !identical(descriptor$n_targets, as.integer(length(classes))) ||
        !identical(descriptor$n_components, 0L) ||
        !identical(descriptor$n_features, model_width) ||
        bitwAnd(as.integer(descriptor$capabilities), 5L) != 5L ||
        isTRUE(n4m::n4m_model_pipeline_info(bytes)$present))
      stop("N4MM sparse PLS-DA state does not match trained recipe",
           call. = FALSE)
    return(invisible(descriptor))
  }
  if (!identical(pipeline$learner$spec$learner, "pls"))
    stop("N4MM PLS state requires a PLS recipe", call. = FALSE)
  required_capabilities <- if (embedded) 9L else 1L
  if (!identical(descriptor$format_version, if (embedded) 2L else 1L) ||
      !identical(descriptor$algorithm, 0L) ||
      !identical(descriptor$solver, 1L) ||
      !identical(descriptor$deflation, 0L) ||
      !identical(descriptor$n_targets, 1L) ||
      bitwAnd(as.integer(descriptor$capabilities),
              required_capabilities) != required_capabilities ||
      !identical(descriptor$n_components,
                 as.integer(pipeline$learner$spec$n_components)) ||
      !identical(descriptor$n_features,
                 if (embedded) input_width else model_width))
    stop("N4MM model does not match the trained recipe", call. = FALSE)
  info <- n4m::n4m_model_pipeline_info(bytes)
  if (embedded) {
    profile <- nirs4all_native_model_profile(pipeline)
    if (is.null(profile) || !identical(profile$owner, "embedded_methods") ||
        !nirs4all_native_model_pipeline_matches(info, profile, input_width))
      stop("N4MM embedded preprocessing does not match recipe", call. = FALSE)
  } else if (isTRUE(info$present)) {
    stop("external preprocessing cannot import embedded N4MM state",
         call. = FALSE)
  }
  invisible(descriptor)
}
