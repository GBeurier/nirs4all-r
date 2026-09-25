#' Export a trained native pipeline for R/Python replay
#'
#' Serializes a portable n4m recipe, fitted MSC/EMSC references and the exact
#' N4MM model payload in one JSON document. No RDS, Python pickle or executable
#' code is included. Version 1 covers n4m PLS regression; version 2 adds
#' sparse PLS-DA with ordered class labels and a native affine N4MM predictor.
#' The JSON recipe also permits fitting the pipeline again in either language.
#' @param object A fitted native n4m PLS or sparse PLS-DA pipeline.
#' @param file Optional output path. If omitted, returns JSON text.
#' @return JSON text, invisibly when `file` is supplied.
#' @export
nirs4all_export_trained_pipeline <- function(object, file = NULL) {
  classification <- inherits(object, "nirs4all_fitted") &&
    identical(object$learner$format, "n4mm_sparse_pls_da") &&
    identical(object$task, "classification")
  if (!inherits(object, "nirs4all_fitted") ||
      !(classification || (identical(object$learner$format, "n4mm") &&
        identical(object$task, "regression"))))
    stop("trained export requires a native n4m PLS or sparse PLS-DA pipeline",
         call. = FALSE)
  recipe_pipeline <- structure(list(steps = object$steps,
    learner = object$learner), class = "nirs4all_pipeline")
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
  state <- nirs4all_portable_encode_states(object$steps, object$step_states,
                                           width, owner)
  bytes <- n4m::n4m_model_export(if (classification)
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
  manifest_json <- as.character(jsonlite::toJSON(manifest,
    auto_unbox = TRUE, null = "null", digits = 17L))
  document <- list(schema = if (classification)
    "nirs4all.n4m.trained_pipeline.v2" else
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
                               "nirs4all.n4m.trained_pipeline.v2")))
    stop("unsupported trained pipeline envelope", call. = FALSE)
  classification <- identical(document$schema, "nirs4all.n4m.trained_pipeline.v2")
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
  wire_owner <- manifest$preprocessing_owner
  if (!(is.character(wire_owner) && length(wire_owner) == 1L &&
        wire_owner %in% c("external", "embedded_methods")))
    stop("unsupported preprocessing ownership", call. = FALSE)
  owner <- if (identical(wire_owner, "external")) "external_r" else wire_owner
  if (classification && !identical(owner, "external_r"))
    stop("sparse PLS-DA requires external preprocessing", call. = FALSE)
  state <- nirs4all_portable_decode_states(pipeline$steps,
    manifest$step_states, width, owner)
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
      classes = classes) else native, step_states = state$states,
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

nirs4all_portable_states <- function(steps, states, width, decode = FALSE) {
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
                                decode = decode))
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

nirs4all_portable_encode_states <- function(steps, states, width, owner) {
  if (identical(owner, "embedded_methods")) {
    if (!is.list(states) || length(states) != length(steps) ||
        !all(vapply(states, is.null, logical(1))))
      stop("embedded preprocessing must not carry external state", call. = FALSE)
    return(list(states = states, output_width = width))
  }
  nirs4all_portable_states(steps, states, width)
}

nirs4all_portable_decode_states <- function(steps, states, width, owner) {
  if (identical(owner, "embedded_methods")) {
    if (!is.list(states) || length(states) != length(steps) ||
        !all(vapply(states, is.null, logical(1))))
      stop("embedded preprocessing must not carry external state", call. = FALSE)
    return(list(states = states, output_width = width))
  }
  nirs4all_portable_states(steps, states, width, decode = TRUE)
}

nirs4all_portable_validate_model <- function(bytes, pipeline, input_width,
                                             model_width, owner,
                                             classes = NULL) {
  descriptor <- n4m::n4m_model_descriptor(bytes)
  embedded <- identical(owner, "embedded_methods")
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
