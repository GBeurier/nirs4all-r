#' Define a row-wise standard normal variate step
#' @param ddof Degrees-of-freedom correction, normally zero.
#' @param with_mean Center each spectrum before scaling.
#' @param with_std Scale each spectrum by its standard deviation.
#' @export
nirs4all_snv <- function(ddof = 0L, with_mean = TRUE, with_std = TRUE) {
  if (length(ddof) != 1L || !is.numeric(ddof) || is.na(ddof) ||
      !is.finite(ddof) || ddof < 0L || ddof > .Machine$integer.max ||
      ddof != floor(ddof))
    stop("ddof must be a non-negative integer", call. = FALSE)
  if (!is.logical(with_mean) || length(with_mean) != 1L || is.na(with_mean) ||
      !is.logical(with_std) || length(with_std) != 1L || is.na(with_std))
    stop("with_mean and with_std must be single logical values", call. = FALSE)
  structure(list(kind = "snv", ddof = as.integer(ddof),
                 with_mean = with_mean, with_std = with_std),
            class = "nirs4all_step")
}

#' Define a local standard normal variate step
#' @param window Odd sliding-window length.
#' @param pad_mode One of `"reflect"`, `"edge"`, or `"constant"`.
#' @param constant_value Padding value in constant mode.
#' @export
nirs4all_local_snv <- function(window = 11L, pad_mode = "reflect",
                               constant_value = 0) {
  if (!is.numeric(window) || length(window) != 1L || !is.finite(window) ||
      window < 3L || window > .Machine$integer.max ||
      window != floor(window) || window %% 2L != 1L)
    stop("window must be an odd integer >= 3", call. = FALSE)
  if (!is.character(pad_mode) || length(pad_mode) != 1L || is.na(pad_mode) ||
      !(pad_mode %in% c("reflect", "edge", "constant")))
    stop("unsupported local SNV pad_mode", call. = FALSE)
  if (!is.numeric(constant_value) || length(constant_value) != 1L ||
      !is.finite(constant_value))
    stop("constant_value must be finite", call. = FALSE)
  structure(list(kind = "local_snv", window = as.integer(window),
                 pad_mode = pad_mode, constant_value = constant_value),
            class = "nirs4all_step")
}

#' Define a robust standard normal variate step
#' @param with_center Center by the row median.
#' @param with_scale Scale by robust dispersion.
#' @param k Positive robust scale factor.
#' @export
nirs4all_robust_snv <- function(with_center = TRUE, with_scale = TRUE,
                                k = 1.4826) {
  if (!is.logical(with_center) || length(with_center) != 1L || is.na(with_center) ||
      !is.logical(with_scale) || length(with_scale) != 1L || is.na(with_scale))
    stop("with_center and with_scale must be single logical values", call. = FALSE)
  if (!is.numeric(k) || length(k) != 1L || !is.finite(k) || k <= 0)
    stop("k must be positive and finite", call. = FALSE)
  structure(list(kind = "robust_snv", with_center = with_center,
                 with_scale = with_scale, k = k), class = "nirs4all_step")
}

#' Define an area normalization step
#' @param method One of `"sum"`, `"abs_sum"`, or `"trapz"`.
#' @export
nirs4all_area_normalization <- function(method = "sum") {
  if (!is.character(method) || length(method) != 1L || is.na(method) ||
      !(method %in% c("sum", "abs_sum", "trapz")))
    stop("unsupported area normalization method", call. = FALSE)
  structure(list(kind = "area_normalization", method = method),
            class = "nirs4all_step")
}

#' Define a polynomial detrend step
#' @param polyorder Non-negative baseline polynomial order.
#' @export
nirs4all_detrend <- function(polyorder = 1L) {
  if (!is.numeric(polyorder) || length(polyorder) != 1L ||
      !is.finite(polyorder) || polyorder < 0L ||
      polyorder > .Machine$integer.max || polyorder != floor(polyorder))
    stop("polyorder must be a non-negative integer", call. = FALSE)
  structure(list(kind = "detrend", polyorder = as.integer(polyorder)),
            class = "nirs4all_step")
}

#' Define a train-fitted Multiplicative Scatter Correction step
#'
#' The reference spectrum is learned from the training rows only and saved
#' with the fitted pipeline. Validation and prediction reuse that reference.
#' @export
nirs4all_msc <- function() {
  structure(list(kind = "msc"), class = "nirs4all_step")
}

#' Define a train-fitted Extended Multiplicative Scatter Correction step
#' @param degree Positive polynomial degree; requires at least `degree + 2` features.
#' @export
nirs4all_emsc <- function(degree = 2L) {
  if (length(degree) != 1L || !is.numeric(degree) || !is.finite(degree) ||
      degree < 1L || degree > .Machine$integer.max - 2L ||
      degree != floor(degree))
    stop("degree must be a positive integer", call. = FALSE)
  structure(list(kind = "emsc", degree = as.integer(degree)),
            class = "nirs4all_step")
}

#' Define a Savitzky-Golay step
#' @param window_length Odd window length.
#' @param polyorder Polynomial order below the window length.
#' @param deriv Derivative order at most polyorder.
#' @param delta Wavelength spacing.
#' @param mode Boundary mode supported by `n4m`.
#' @param cval Fill value for constant boundary mode.
#' @export
nirs4all_savgol <- function(window_length, polyorder = 2L, deriv = 0L,
                            delta = 1, mode = "interp", cval = 0) {
  integer_scalar <- function(x) length(x) == 1L && is.numeric(x) &&
    is.finite(x) && x >= 0 && x <= .Machine$integer.max && x == floor(x)
  if (!integer_scalar(window_length) || window_length < 3L || window_length %% 2L != 1L)
    stop("window_length must be an odd integer >= 3", call. = FALSE)
  if (!integer_scalar(polyorder) || polyorder < 0L || polyorder >= window_length)
    stop("polyorder must be an integer below window_length", call. = FALSE)
  if (!integer_scalar(deriv) || deriv < 0L || deriv > polyorder)
    stop("deriv must be an integer between zero and polyorder", call. = FALSE)
  if (length(delta) != 1L || !is.numeric(delta) || !is.finite(delta) || delta <= 0)
    stop("delta must be positive and finite", call. = FALSE)
  if (length(mode) != 1L || is.na(mode) ||
      !(mode %in% c("mirror", "constant", "nearest", "wrap", "interp")))
    stop("unsupported Savitzky-Golay boundary mode", call. = FALSE)
  if (length(cval) != 1L || !is.numeric(cval) || !is.finite(cval))
    stop("cval must be finite", call. = FALSE)
  structure(list(kind = "savgol", window_length = as.integer(window_length),
                 polyorder = as.integer(polyorder), deriv = as.integer(deriv),
                 delta = delta, mode = mode, cval = cval), class = "nirs4all_step")
}

#' Concatenate parallel preprocessing branches
#'
#' Each branch starts from the same input matrix. Train-fitted states, such as
#' MSC references, are learned independently on the training rows of their
#' branch. Output columns are prefixed by the branch name.
#'
#' @param branches Named list of at least two non-empty lists of nirs4all steps.
#' @return A composable nirs4all preprocessing step.
#' @export
nirs4all_concat <- function(branches) {
  if (!is.list(branches) || length(branches) < 2L ||
      is.null(names(branches)) || anyNA(names(branches)) ||
      anyDuplicated(names(branches)) ||
      !all(grepl("^[A-Za-z][A-Za-z0-9_.-]*$", names(branches))) ||
      !all(vapply(branches, function(branch)
        is.list(branch) && length(branch) > 0L &&
          all(vapply(branch, inherits, logical(1), "nirs4all_step")), logical(1))))
    stop("branches must be named, non-empty preprocessing step lists", call. = FALSE)
  structure(list(kind = "concat", branches = branches), class = "nirs4all_step")
}

#' Define an R pipeline
#' @param steps List of preprocessing steps, applied in order.
#' @param learner One controller from [nirs4all_pls()], [nirs4all_lm()],
#'   [nirs4all_ranger()] or [nirs4all_controller()].
#' @export
nirs4all_pipeline <- function(steps = list(), learner = nirs4all_pls()) {
  if (!is.list(steps) || !all(vapply(steps, inherits, logical(1), "nirs4all_step")))
    stop("steps must be a list of nirs4all preprocessing steps", call. = FALSE)
  if (!inherits(learner, "nirs4all_controller"))
    stop("learner must be a nirs4all controller", call. = FALSE)
  structure(list(steps = steps, learner = learner), class = "nirs4all_pipeline")
}

nirs4all_matrix <- function(X, n_features = NULL) {
  if (!is.matrix(X) || !is.numeric(X) || length(dim(X)) != 2L ||
      nrow(X) < 1L || ncol(X) < 1L || anyNA(X) || any(!is.finite(X)))
    stop("X must be a non-empty finite numeric matrix", call. = FALSE)
  if (!is.null(n_features) && ncol(X) != n_features)
    stop(sprintf("X has %d features; expected %d", ncol(X), n_features), call. = FALSE)
  storage.mode(X) <- "double"
  X
}

nirs4all_concat_matrices <- function(matrices) {
  if (!is.list(matrices) || length(matrices) < 2L ||
      is.null(names(matrices)) || anyDuplicated(names(matrices)))
    stop("concat requires named branch matrices", call. = FALSE)
  rows <- vapply(matrices, nrow, integer(1))
  if (length(unique(rows)) != 1L)
    stop("concat branch row counts differ", call. = FALSE)
  labelled <- lapply(names(matrices), function(name) {
    output <- nirs4all_matrix(matrices[[name]])
    columns <- colnames(output)
    if (is.null(columns)) columns <- sprintf("feature:%08d", seq_len(ncol(output)))
    colnames(output) <- paste(name, columns, sep = "::")
    output
  })
  do.call(cbind, labelled)
}

nirs4all_transform <- function(X, steps, step_states = NULL) {
  if (is.null(step_states)) step_states <- rep(list(NULL), length(steps))
  if (!is.list(step_states) || length(step_states) != length(steps))
    stop("fitted preprocessing state does not match pipeline steps", call. = FALSE)
  for (index in seq_along(steps)) {
    step <- steps[[index]]
    if (identical(step$kind, "snv") && step$ddof >= ncol(X))
      stop("SNV ddof must be smaller than the feature count", call. = FALSE)
    X <- switch(step$kind,
      snv = n4m::snv_transform(
        X, with_mean = if (is.null(step$with_mean)) TRUE else step$with_mean,
        with_std = if (is.null(step$with_std)) TRUE else step$with_std,
        ddof = step$ddof),
      local_snv = n4m::local_snv_transform(
        X, window = step$window, pad_mode = step$pad_mode,
        constant_value = step$constant_value),
      robust_snv = n4m::robust_snv_transform(
        X, with_center = step$with_center, with_scale = step$with_scale,
        k = step$k),
      area_normalization = n4m::area_normalization_transform(X, step$method),
      detrend = n4m::detrend_transform(X, step$polyorder),
      msc = {
        reference <- step_states[[index]]
        if (is.null(reference))
          stop("MSC requires a fitted training reference", call. = FALSE)
        n4m::msc_transform(X, reference)
      },
      emsc = {
        reference <- step_states[[index]]
        if (is.null(reference))
          stop("EMSC requires a fitted training reference", call. = FALSE)
        n4m::emsc_transform(X, reference, step$degree)
      },
      concat = {
        branch_states <- step_states[[index]]
        if (!is.list(branch_states) ||
            !identical(names(branch_states), names(step$branches)))
          stop("concat requires fitted state for each branch", call. = FALSE)
        matrices <- lapply(names(step$branches), function(name)
          nirs4all_transform(X, step$branches[[name]], branch_states[[name]]))
        names(matrices) <- names(step$branches)
        nirs4all_concat_matrices(matrices)
      },
      savgol = n4m::savgol_transform(X, step$window_length,
        step$polyorder, step$deriv, step$delta, step$mode, step$cval),
      stop("unknown preprocessing step", call. = FALSE))
    if (!is.matrix(X) || !is.numeric(X) || anyNA(X) || any(!is.finite(X)))
      stop("preprocessing produced non-finite or invalid data", call. = FALSE)
  }
  X
}

nirs4all_fit_transform <- function(X, steps) {
  states <- rep(list(NULL), length(steps))
  for (index in seq_along(steps)) {
    if (identical(steps[[index]]$kind, "msc"))
      states[[index]] <- n4m::msc_fit(X)
    if (identical(steps[[index]]$kind, "emsc"))
      states[[index]] <- n4m::emsc_fit(X, steps[[index]]$degree)
    if (identical(steps[[index]]$kind, "concat")) {
      branch_fits <- lapply(steps[[index]]$branches, function(branch)
        nirs4all_fit_transform(X, branch))
      states[[index]] <- lapply(branch_fits, `[[`, "states")
      X <- nirs4all_transform(X, list(steps[[index]]), list(states[[index]]))
      next
    }
    X <- nirs4all_transform(X, list(steps[[index]]), list(states[[index]]))
  }
  list(X = X, states = states)
}

nirs4all_embedded_snv_savgol <- function(pipeline) {
  steps <- pipeline$steps
  spec <- pipeline$learner$spec
  if (length(steps) != 2L || !identical(steps[[1L]]$kind, "snv") ||
      !identical(steps[[1L]]$ddof, 0L) ||
      !identical(steps[[1L]]$with_mean, TRUE) ||
      !identical(steps[[1L]]$with_std, TRUE) ||
      !identical(steps[[2L]]$kind, "savgol") ||
      !identical(steps[[2L]]$deriv, 0L) ||
      !isTRUE(steps[[2L]]$delta == 1) ||
      !identical(steps[[2L]]$mode, "interp") ||
      !isTRUE(steps[[2L]]$cval == 0) ||
      !is.list(spec) || !identical(spec$learner, "pls") ||
      !identical(spec$algo, "pls_simpls") ||
      !all(vapply(spec[c("center_x", "scale_x", "center_y", "scale_y")],
                  identical, logical(1), TRUE)))
    return(NULL)
  c(steps[[2L]]$window_length, steps[[2L]]$polyorder)
}

#' Fit a pipeline on R matrices
#' @param pipeline A [nirs4all_pipeline()] definition.
#' @param X Numeric samples-by-features matrix.
#' @param y Finite numeric target vector for regression, or factor/character
#'   class labels for classification.
#' @return Fitted pipeline. Use [nirs4all_predict()] or [nirs4all_save()].
#' @export
nirs4all_fit <- function(pipeline, X, y = NULL) {
  if (!inherits(pipeline, "nirs4all_pipeline"))
    stop("pipeline must be a nirs4all_pipeline", call. = FALSE)
  if (inherits(X, "nirs4all_dataset")) {
    if (!is.null(y)) stop("y must come from the nirs4all_dataset", call. = FALSE)
    y <- X$y
    X <- X$X
  }
  X <- nirs4all_matrix(X)
  task <- pipeline$learner$task
  if (is.null(task)) task <- "regression"
  if (identical(task, "classification")) {
    if (is.character(y)) y <- factor(y)
    if (!is.factor(y) || is.ordered(y) || is.matrix(y) ||
        length(y) != nrow(X) || anyNA(y) || nlevels(y) < 2L ||
        any(tabulate(as.integer(y), nbins = nlevels(y)) == 0L))
      stop("classification y must be a factor or character vector with at least two observed classes", call. = FALSE)
  } else if (!identical(task, "regression") || !is.numeric(y) ||
             is.matrix(y) || length(y) != nrow(X) || anyNA(y) ||
             any(!is.finite(y))) {
    stop("y must be one finite numeric value per row of X", call. = FALSE)
  }
  if (!is.null(rownames(X)) && !is.null(names(y)) &&
      !identical(rownames(X), names(y)))
    stop("X row names and y sample names differ", call. = FALSE)
  embedded <- if (identical(task, "regression"))
    nirs4all_embedded_snv_savgol(pipeline) else NULL
  if (!is.null(embedded)) {
    state <- n4m::n4m_fit(X, as.numeric(y), algo = "pls_simpls",
      n_components = pipeline$learner$spec$n_components,
      embedded_snv_savgol = embedded)
    states <- rep(list(NULL), length(pipeline$steps))
    owner <- "embedded_methods"
  } else {
    transformed <- nirs4all_fit_transform(X, pipeline$steps)
    state <- pipeline$learner$fit(transformed$X,
      if (identical(task, "classification")) y else as.numeric(y))
    states <- transformed$states
    owner <- "external_r"
  }
  structure(list(steps = pipeline$steps, learner = pipeline$learner,
                 state = state, step_states = states,
                 preprocessing_owner = owner,
                 task = task,
                 classes = if (identical(task, "classification")) levels(y) else NULL,
                 n_features = ncol(X),
                 feature_names = colnames(X)), class = "nirs4all_fitted")
}

#' Predict from a fitted pipeline
#' @param object Fitted pipeline.
#' @param X Numeric samples-by-features matrix.
#' @export
nirs4all_predict <- function(object, X) {
  if (!inherits(object, "nirs4all_fitted"))
    stop("object must be a fitted nirs4all pipeline", call. = FALSE)
  if (inherits(X, "nirs4all_dataset")) X <- X$X
  X <- nirs4all_matrix(X, object$n_features)
  if (!is.null(object$feature_names) && !identical(colnames(X), object$feature_names))
    stop("X feature names or order differ from training", call. = FALSE)
  transformed <- if (identical(object$preprocessing_owner, "embedded_methods"))
    X else nirs4all_transform(X, object$steps, object$step_states)
  out <- object$learner$predict(object$state, transformed)
  if (identical(object$task, "classification")) {
    if (!is.factor(out) || length(out) != nrow(X) || anyNA(out) ||
        !identical(levels(out), object$classes))
      stop("controller returned invalid class predictions", call. = FALSE)
    return(out)
  }
  if (!is.numeric(out) || length(out) != nrow(X) || anyNA(out) || any(!is.finite(out)))
    stop("controller returned invalid predictions", call. = FALSE)
  as.numeric(out)
}

#' Predict class probabilities from a fitted classifier
#' @param object Fitted classification pipeline.
#' @param X Numeric samples-by-features matrix.
#' @return A samples-by-classes matrix with columns in training class order.
#' @export
nirs4all_predict_proba <- function(object, X) {
  if (!inherits(object, "nirs4all_fitted") ||
      !identical(object$task, "classification") ||
      !is.function(object$learner$predict_proba))
    stop("object must be a fitted classifier with probabilities", call. = FALSE)
  if (inherits(X, "nirs4all_dataset")) X <- X$X
  X <- nirs4all_matrix(X, object$n_features)
  if (!is.null(object$feature_names) && !identical(colnames(X), object$feature_names))
    stop("X feature names or order differ from training", call. = FALSE)
  transformed <- nirs4all_transform(X, object$steps, object$step_states)
  out <- object$learner$predict_proba(object$state, transformed)
  if (!is.matrix(out) || !is.numeric(out) ||
      !identical(dim(out), c(nrow(X), length(object$classes))) ||
      !identical(colnames(out), object$classes) || anyNA(out) ||
      any(!is.finite(out)) || any(out < 0) || any(out > 1) ||
      any(abs(rowSums(out) - 1) > 1e-6))
    stop("controller returned invalid class probabilities", call. = FALSE)
  out
}

#' @export
predict.nirs4all_fitted <- function(object, newdata, ...) {
  if (length(list(...))) stop("unused prediction arguments", call. = FALSE)
  nirs4all_predict(object, newdata)
}
