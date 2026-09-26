# A deliberately narrow bridge from product steps to the fitted native N4MP
# operator chain. Keep this mapping explicit: similar names do not imply equal
# numerical semantics (notably EMSC).
nirs4all_n4mp_steps <- function(steps) {
  if (!is.list(steps) || !length(steps))
    stop("native N4MP requires a nonempty linear preprocessing chain", call. = FALSE)
  lapply(steps, function(step) {
    if (!inherits(step, "nirs4all_step"))
      stop("invalid native N4MP preprocessing step", call. = FALSE)
    if (identical(step$kind, "snv")) {
      if (!identical(step$ddof, 0L) || !identical(step$with_mean, TRUE) ||
          !identical(step$with_std, TRUE))
        stop("native N4MP supports only default SNV semantics", call. = FALSE)
      return(n4m::n4m_preprocess_step("snv"))
    }
    if (identical(step$kind, "msc"))
      return(n4m::n4m_preprocess_step("msc"))
    if (identical(step$kind, "detrend")) {
      if (step$polyorder > 5L)
        stop("native N4MP detrend degree must be at most 5", call. = FALSE)
      return(n4m::n4m_preprocess_step("detrend_poly", step$polyorder))
    }
    if (identical(step$kind, "savgol")) {
      if (!identical(step$mode, "interp") || !identical(step$cval, 0) ||
          step$window_length > 501L || step$deriv > 2L)
        stop("native N4MP supports only interp SavGol with cval 0, window <= 501 and derivative <= 2", call. = FALSE)
      if (!identical(step$delta, 1))
        stop("portable native N4MP SavGol requires delta 1", call. = FALSE)
      if (step$deriv == 0L) {
        return(n4m::n4m_preprocess_step("savgol_smooth",
          c(step$window_length, step$polyorder)))
      }
      return(n4m::n4m_preprocess_step("savgol_derivative",
        c(step$window_length, step$polyorder, step$deriv, step$delta)))
    }
    stop(sprintf("native N4MP does not preserve '%s' semantics", step$kind),
         call. = FALSE)
  })
}

nirs4all_n4mp_equal_plan <- function(actual, expected) {
  is.list(actual) && length(actual) == length(expected) &&
    all(vapply(seq_along(expected), function(i)
      identical(actual[[i]]$kind, expected[[i]]$kind) &&
        identical(as.numeric(actual[[i]]$params),
                  as.numeric(expected[[i]]$params)), logical(1)))
}

nirs4all_n4mp_transform <- function(preprocessing, X) {
  transformed <- n4m::n4m_preprocess_transform(preprocessing, X)
  # Every qualified v6 operator preserves both feature count and spectral
  # order. Reattach the input schema before controllers inspect named feature
  # assignments (notably GroupSparsePLS). The native matrix API itself does
  # not carry R dimnames.
  if (!is.matrix(transformed) || !identical(dim(transformed), dim(X)) ||
      anyNA(transformed) || any(!is.finite(transformed)))
    stop("native N4MP changed the qualified feature shape or finiteness",
         call. = FALSE)
  dimnames(transformed) <- dimnames(X)
  transformed
}

nirs4all_n4mp_payload <- function(kind, encoding, bytes) {
  list(kind = kind, encoding = encoding,
       sha256 = digest::digest(bytes, algo = "sha256", serialize = FALSE),
       payload = gsub("\n", "", jsonlite::base64_enc(bytes), fixed = TRUE))
}

nirs4all_n4mp_decode_payload <- function(value, kind, encoding) {
  if (!is.list(value) ||
      !setequal(names(value), c("kind", "encoding", "sha256", "payload")) ||
      !identical(value$kind, kind) || !identical(value$encoding, encoding) ||
      !is.character(value$sha256) || length(value$sha256) != 1L ||
      !grepl("^[0-9a-f]{64}$", value$sha256) ||
      !is.character(value$payload) || length(value$payload) != 1L)
    stop("invalid native payload metadata", call. = FALSE)
  bytes <- tryCatch(jsonlite::base64_dec(value$payload), error = identity)
  if (inherits(bytes, "error") || !is.raw(bytes) || !length(bytes) ||
      !identical(digest::digest(bytes, algo = "sha256", serialize = FALSE),
                 value$sha256))
    stop("native payload hash mismatch", call. = FALSE)
  bytes
}

nirs4all_export_trained_n4mp <- function(object, file = NULL) {
  if (!inherits(object, "nirs4all_fitted") ||
      !identical(object$preprocessing_owner, "native_n4mp") ||
      !identical(object$task, "regression") ||
      !(object$learner$format %in% c("n4mm", "n4mm_affine")))
    stop("v6 export requires a native N4MP regression fit", call. = FALSE)
  pipeline <- structure(list(steps = object$steps, learner = object$learner),
                        class = "nirs4all_pipeline")
  expected <- nirs4all_n4mp_steps(object$steps)
  if (!nirs4all_n4mp_equal_plan(
      n4m::n4m_preprocess_plan(object$native_preprocessing), expected) ||
      !identical(object$native_preprocessing$n_features, object$n_features))
    stop("native preprocessing plan differs from recipe", call. = FALSE)
  width <- as.integer(object$n_features)
  if (length(width) != 1L || is.na(width) || width < 2L ||
      (!is.null(object$feature_names) &&
       (length(object$feature_names) != width ||
        anyNA(object$feature_names) || any(!nzchar(object$feature_names)) ||
        anyDuplicated(object$feature_names))))
    stop("invalid fitted input schema", call. = FALSE)
  pre_bytes <- n4m::n4m_preprocess_export(object$native_preprocessing)
  model_bytes <- if (identical(object$learner$format, "n4mm_affine"))
    nirs4all_affine_export(object$state) else n4m::n4m_model_export(object$state)
  nirs4all_portable_validate_model(model_bytes, pipeline, width, width,
                                   "external_r")
  recipe <- jsonlite::fromJSON(nirs4all_export_pipeline(pipeline),
                                simplifyVector = FALSE)
  manifest <- list(recipe = recipe, input_n_features = width,
    feature_names = if (is.null(object$feature_names)) NULL else
      unname(as.list(object$feature_names)),
    preprocessing_owner = "native_n4mp", step_states = list())
  if (identical(object$learner$format, "n4mm_affine")) {
    method <- object$learner$spec$method
    manifest$fit_recipe_assertion <- list(kind = "affine_recipe",
      recipe_class = unname(.nirs4all_portable_affine[[method]]))
    if (identical(method, "mb_pls"))
      manifest$fit_recipe_assertion$block_sizes <-
        unname(as.list(object$learner$spec$params$block_sizes))
  }
  manifest_json <- as.character(jsonlite::toJSON(manifest, auto_unbox = TRUE,
                                                null = "null", digits = 17L))
  document <- list(schema = "nirs4all.n4m.trained_pipeline.v6",
    manifest_json = manifest_json,
    manifest_sha256 = digest::digest(manifest_json, algo = "sha256",
                                     serialize = FALSE),
    preprocessing = nirs4all_n4mp_payload("n4m_preprocessing", "base64-n4mp",
                                          pre_bytes),
    model = nirs4all_n4mp_payload("n4m_model", "base64-n4mm", model_bytes))
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

nirs4all_import_trained_n4mp <- function(document) {
  if (!is.list(document) ||
      !setequal(names(document), c("schema", "manifest_json", "manifest_sha256",
                                 "preprocessing", "model")) ||
      !identical(document$schema, "nirs4all.n4m.trained_pipeline.v6") ||
      !is.character(document$manifest_json) ||
      length(document$manifest_json) != 1L ||
      !is.character(document$manifest_sha256) ||
      length(document$manifest_sha256) != 1L ||
      !grepl("^[0-9a-f]{64}$", document$manifest_sha256) ||
      !identical(digest::digest(document$manifest_json, algo = "sha256",
                                serialize = FALSE), document$manifest_sha256))
    stop("invalid v6 trained pipeline envelope or manifest hash", call. = FALSE)
  manifest <- jsonlite::fromJSON(document$manifest_json, simplifyVector = FALSE)
  affine <- "fit_recipe_assertion" %in% names(manifest)
  required <- c("recipe", "input_n_features", "feature_names",
                "preprocessing_owner", "step_states",
                if (affine) "fit_recipe_assertion")
  if (!is.list(manifest) || !setequal(names(manifest), required) ||
      !identical(manifest$preprocessing_owner, "native_n4mp") ||
      !is.list(manifest$step_states) || length(manifest$step_states))
    stop("invalid v6 trained pipeline manifest", call. = FALSE)
  width <- manifest$input_n_features
  if (!is.numeric(width) || length(width) != 1L || is.na(width) ||
      !is.finite(width) || width != floor(width) || width < 2L ||
      width > .Machine$integer.max)
    stop("invalid input feature width", call. = FALSE)
  width <- as.integer(width)
  names_in <- manifest$feature_names
  if (!is.null(names_in) && (!is.list(names_in) || length(names_in) != width ||
      !all(vapply(names_in, function(x) is.character(x) && length(x) == 1L &&
        !is.na(x) && nzchar(x), logical(1))) ||
      anyDuplicated(unlist(names_in, use.names = FALSE))))
    stop("invalid ordered feature names", call. = FALSE)
  feature_names <- if (is.null(names_in)) NULL else
    unlist(names_in, use.names = FALSE)
  pipeline <- nirs4all_pipeline_from_portable(manifest$recipe)
  if (!identical(pipeline$learner$task, "regression") ||
      !(pipeline$learner$format %in% c("n4mm", "n4mm_affine")) ||
      affine != identical(pipeline$learner$format, "n4mm_affine"))
    stop("v6 requires a portable regression recipe", call. = FALSE)
  if (affine) {
    method <- pipeline$learner$spec$method
    assertion <- manifest$fit_recipe_assertion
    expected <- list(kind = "affine_recipe",
      recipe_class = unname(.nirs4all_portable_affine[[method]]))
    if (identical(method, "mb_pls")) expected$block_sizes <-
      unname(as.list(pipeline$learner$spec$params$block_sizes))
    if (!identical(assertion, expected))
      stop("affine fit recipe assertion mismatch", call. = FALSE)
  }
  expected_plan <- nirs4all_n4mp_steps(pipeline$steps)
  pre_bytes <- nirs4all_n4mp_decode_payload(document$preprocessing,
    "n4m_preprocessing", "base64-n4mp")
  model_bytes <- nirs4all_n4mp_decode_payload(document$model,
    "n4m_model", "base64-n4mm")
  preprocessing <- n4m::n4m_preprocess_import(pre_bytes)
  if (!identical(preprocessing$n_features, width) ||
      !nirs4all_n4mp_equal_plan(n4m::n4m_preprocess_plan(preprocessing),
                                expected_plan))
    stop("N4MP width or ordered plan differs from recipe", call. = FALSE)
  nirs4all_portable_validate_model(model_bytes, pipeline, width, width,
                                   "external_r")
  native_model <- n4m::n4m_model_import(model_bytes)
  structure(list(steps = pipeline$steps, learner = pipeline$learner,
    state = if (affine) list(native_model = native_model) else native_model,
    step_states = rep(list(NULL), length(pipeline$steps)),
    native_preprocessing = preprocessing, preprocessing_owner = "native_n4mp",
    task = "regression", classes = NULL, n_features = width,
    feature_names = feature_names), class = "nirs4all_fitted")
}
