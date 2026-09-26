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

#' Define a train-fitted n4m SPA variable-selection step
#'
#' Successive Projections Algorithm selects wavelengths using the training
#' predictors and target. The selected column indices are stored in the fitted
#' pipeline and reused unchanged for validation and prediction.
#' @param top_k Number of input features to retain.
#' @param n_components Number of PLS components used by the n4m selector.
#' @export
nirs4all_spa <- function(top_k, n_components = 2L) {
  positive_integer <- function(value) is.numeric(value) && length(value) == 1L &&
    is.finite(value) && value >= 1L && value <= .Machine$integer.max &&
    value == floor(value)
  if (!positive_integer(top_k) || !positive_integer(n_components))
    stop("top_k and n_components must be positive integers", call. = FALSE)
  structure(list(kind = "spa", top_k = as.integer(top_k),
                 n_components = as.integer(n_components)),
            class = "nirs4all_step")
}

# The dispatcher currently exposes 25 names despite describing 24 selectors.
# This is an input schema, not a claim of per-method cross-language parity.
.nirs4all_selector_params <- list(
  spa_select = "top_k", cars_select = c("n_iterations", "min_features"),
  interval_select = c("interval_width", "step"), stability_select = "top_k",
  uve_select = c("noise_features", "noise_seed"),
  random_frog_select = c("n_iterations", "initial_size", "min_size",
                         "max_size", "top_k", "seed"),
  scars_select = c("n_iterations", "min_features", "sample_fraction", "seed"),
  ga_select = c("n_generations", "population_size", "min_features",
                "max_features", "mutation_rate", "seed"),
  pso_select = c("n_swarm", "n_iterations", "w", "c1", "c2", "v_max", "seed"),
  vissa_select = c("n_iterations", "n_submodels", "ratio_kept", "threshold",
                   "floor_probability", "seed"),
  shaving_select = c("n_steps", "min_features", "shave_fraction"),
  bve_select = c("n_steps", "min_features"),
  t2_select = c("alpha_thresholds", "min_selected"),
  wvc_select = c("top_k", "normalize"),
  wvc_threshold_select = c("normalize", "threshold", "threshold_factor",
                           "min_selected"),
  emcuve_select = c("noise_features", "noise_seed", "n_ensembles",
                    "vote_threshold"),
  randomization_select = c("n_permutations", "randomization_seed", "alpha"),
  bipls_select = c("interval_width", "min_intervals"),
  sipls_select = c("interval_width", "combination_size"),
  rep_select = c("n_steps", "min_features", "remove_count"),
  ipw_select = c("n_iterations", "top_k", "damping", "weight_floor"),
  st_select = c("thresholds", "min_selected"),
  iriv_select = c("max_rounds", "seed"),
  irf_select = c("n_iterations", "window_size", "initial_intervals", "top_k", "seed"),
  vip_spa_select = c("vip_threshold", "top_k"))

#' Define a train-fitted native n4m variable selector
#'
#' The selector sees only the training matrix and target. Its returned
#' `selected_indices` are checked, stored, and projected in original spectral
#' order on every later matrix. Methods with internal validation use n4m's
#' native validation plan. Availability here does not imply qualified portable
#' parity for each algorithm.
#' @param method One of the n4m dispatcher selector names ending in `_select`.
#' @param n_components Positive native component count.
#' @param params Named list of native method parameters.
#' @export
nirs4all_n4m_selector <- function(method, n_components = 2L, params = list()) {
  if (!is.character(method) || length(method) != 1L || is.na(method) ||
      !(method %in% names(.nirs4all_selector_params)))
    stop("unsupported n4m selector", call. = FALSE)
  positive_integer <- function(value) is.numeric(value) && length(value) == 1L &&
    is.finite(value) && value >= 1L && value <= .Machine$integer.max &&
    value == floor(value)
  if (!positive_integer(n_components))
    stop("n_components must be a positive integer", call. = FALSE)
  allowed <- .nirs4all_selector_params[[method]]
  if (!is.list(params) || (length(params) &&
      (is.null(names(params)) || anyNA(names(params)) ||
       any(!nzchar(names(params))) || anyDuplicated(names(params)) ||
       !all(names(params) %in% allowed))))
    stop("unsupported or duplicate n4m selector parameter", call. = FALSE)
  if (!length(params)) names(params) <- character()
  vectors <- c("alpha_thresholds", "thresholds")
  integers <- c("top_k", "n_iterations", "min_features", "interval_width",
    "step", "noise_features", "noise_seed", "initial_size", "min_size",
    "max_size", "seed", "n_generations", "population_size", "n_swarm",
    "n_submodels", "n_steps", "min_selected", "normalize", "n_ensembles",
    "n_permutations", "randomization_seed", "min_intervals", "combination_size",
    "remove_count", "max_rounds", "window_size", "initial_intervals")
  seeds <- c("seed", "noise_seed", "randomization_seed")
  for (name in names(params)) {
    value <- params[[name]]
    if (!(is.numeric(value) || (identical(name, "normalize") && is.logical(value))) ||
        !length(value) || anyNA(value) || any(!is.finite(value)) ||
        (length(value) != 1L && !(name %in% vectors)))
      stop(sprintf("invalid n4m selector parameter '%s'", name), call. = FALSE)
    if (name %in% integers &&
        (any(value != floor(value)) || any(value < if (name %in% c(seeds, "normalize")) 0 else 1) ||
         any(value > .Machine$integer.max)))
      stop(sprintf("invalid integer selector parameter '%s'", name), call. = FALSE)
    if (identical(name, "normalize") && !(value %in% c(0, 1)))
      stop("normalize must be TRUE or FALSE", call. = FALSE)
    if (name %in% integers) params[[name]] <- as.integer(value)
    if (name %in% vectors) params[[name]] <- as.numeric(value)
  }
  if (method %in% c("t2_select", "st_select") &&
      is.null(params[[if (identical(method, "t2_select"))
        "alpha_thresholds" else "thresholds"]]))
    stop("selector requires a threshold vector", call. = FALSE)
  structure(list(kind = "n4m_selector", method = method,
                 n_components = as.integer(n_components), params = params),
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
#' @param augmentations Optional ordered list of seeded
#'   [nirs4all_native_augmentation()] specifications. Applied only to
#'   training X during fit, before preprocessing; never to Y or prediction X.
#' @param learner One controller from [nirs4all_pls()], [nirs4all_lm()],
#'   [nirs4all_ranger()] or [nirs4all_controller()].
#' @export
nirs4all_pipeline <- function(steps = list(), learner = nirs4all_pls(),
                             augmentations = list()) {
  if (!is.list(steps) || !all(vapply(steps, inherits, logical(1), "nirs4all_step")))
    stop("steps must be a list of nirs4all preprocessing steps", call. = FALSE)
  if (!inherits(learner, "nirs4all_controller"))
    stop("learner must be a nirs4all controller", call. = FALSE)
  if (!is.list(augmentations) || !all(vapply(augmentations, inherits,
                                           logical(1),
                                           "nirs4all_native_augmentation")))
    stop("augmentations must be native training-only augmentation specifications",
         call. = FALSE)
  structure(list(steps = steps, learner = learner,
                 augmentations = augmentations), class = "nirs4all_pipeline")
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
    input_dimnames <- dimnames(X)
    input_dim <- dim(X)
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
      spa = {
        selected <- step_states[[index]]
        if (!is.integer(selected) || length(selected) != step$top_k ||
            anyNA(selected) || anyDuplicated(selected) ||
            any(selected < 1L | selected > ncol(X)))
          stop("SPA requires valid fitted feature indices", call. = FALSE)
        # Python's SelectorMixin projects in original feature order.
        X[, sort(selected), drop = FALSE]
      },
      n4m_selector = {
        selected <- step_states[[index]]
        if (!is.integer(selected) || !length(selected) || anyNA(selected) ||
            anyDuplicated(selected) || any(selected < 1L | selected > ncol(X)))
          stop("n4m selector requires valid fitted feature indices", call. = FALSE)
        X[, sort(selected), drop = FALSE]
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
    if (step$kind %in% c("snv", "local_snv", "robust_snv",
                         "area_normalization", "detrend", "msc", "emsc",
                         "savgol")) {
      if (!identical(dim(X), input_dim))
        stop("width-preserving preprocessing changed the feature shape",
             call. = FALSE)
      dimnames(X) <- input_dimnames
    }
  }
  X
}

nirs4all_fit_transform <- function(X, steps, y = NULL) {
  states <- rep(list(NULL), length(steps))
  for (index in seq_along(steps)) {
    if (identical(steps[[index]]$kind, "msc"))
      states[[index]] <- n4m::msc_fit(X)
    if (identical(steps[[index]]$kind, "emsc"))
      states[[index]] <- n4m::emsc_fit(X, steps[[index]]$degree)
    if (identical(steps[[index]]$kind, "spa")) {
      step <- steps[[index]]
      if (!is.numeric(y) || is.matrix(y) || length(y) != nrow(X) ||
          anyNA(y) || any(!is.finite(y)))
        stop("SPA requires a finite numeric training target", call. = FALSE)
      if (step$top_k > ncol(X) ||
          step$n_components > min(nrow(X) - 1L, ncol(X)))
        stop("SPA parameters exceed the training matrix dimensions", call. = FALSE)
      selected <- n4m::spa_select(X, y, step$n_components, step$top_k)$selected_indices
      states[[index]] <- as.integer(selected)
    }
    if (identical(steps[[index]]$kind, "n4m_selector")) {
      step <- steps[[index]]
      if (!is.numeric(y) || is.matrix(y) || length(y) != nrow(X) ||
          anyNA(y) || any(!is.finite(y)))
        stop("n4m selector requires a finite numeric training target", call. = FALSE)
      if (step$n_components > min(nrow(X) - 1L, ncol(X)))
        stop("selector n_components exceeds the training matrix rank", call. = FALSE)
      if (!is.null(step$params$top_k) && step$params$top_k > ncol(X))
        stop("selector top_k exceeds the input feature count", call. = FALSE)
      result <- n4m::n4m_method(step$method, X, y,
                                step$n_components, params = step$params)
      selected <- result$selected_indices
      if (!is.numeric(selected) || !length(selected) || anyNA(selected) ||
          any(!is.finite(selected)) || any(selected != floor(selected)) ||
          any(selected < 1L | selected > ncol(X)) || anyDuplicated(selected))
        stop("n4m selector returned invalid selected_indices", call. = FALSE)
      states[[index]] <- as.integer(selected)
    }
    if (identical(steps[[index]]$kind, "concat")) {
      branch_fits <- lapply(steps[[index]]$branches, function(branch)
        nirs4all_fit_transform(X, branch, y))
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
#' @param preprocessing `"legacy"` preserves the existing preprocessing path;
#'   `"native_n4mp"` fits a bounded, linear native N4MP chain for portable
#'   regression. Unsupported step semantics are rejected.
#' @return Fitted pipeline. Use [nirs4all_predict()] or [nirs4all_save()].
#' @export
nirs4all_fit <- function(pipeline, X, y = NULL,
                         preprocessing = c("legacy", "native_n4mp")) {
  preprocessing <- match.arg(preprocessing)
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
  di_pls <- identical(pipeline$learner$spec$method, "di_pls")
  if (di_pls && length(pipeline$steps)) {
    if (!identical(preprocessing, "legacy"))
      stop("DI-PLS with preprocessing requires the qualified legacy step path",
           call. = FALSE)
    allowed <- c("snv", "local_snv", "robust_snv", "area_normalization",
                 "detrend", "msc", "emsc", "savgol")
    if (!all(vapply(pipeline$steps, function(step)
        inherits(step, "nirs4all_step") && step$kind %in% allowed,
        logical(1))))
      stop("DI-PLS target cohort cannot share selector or branch preprocessing state",
           call. = FALSE)
    target <- pipeline$learner$spec$params$X_target
    if (ncol(target) != ncol(X) ||
        (!is.null(colnames(target)) &&
         !identical(colnames(target), colnames(X))))
      stop("DI-PLS target cohort feature width, names or order differ from source X",
           call. = FALSE)
  }
  if (identical(preprocessing, "native_n4mp") &&
      (!identical(task, "regression") ||
       !(pipeline$learner$format %in% c("n4mm", "n4mm_affine"))))
    stop("native N4MP currently requires a portable regression learner", call. = FALSE)
  if (di_pls && length(pipeline$augmentations))
    stop("DI-PLS cannot augment source X without a qualified target-domain policy",
         call. = FALSE)
  if (length(pipeline$augmentations))
    X <- nirs4all_augment_training(X, pipeline$augmentations)
  embedded <- if (identical(task, "regression") &&
                  identical(preprocessing, "legacy"))
    nirs4all_embedded_snv_savgol(pipeline) else NULL
  if (identical(preprocessing, "native_n4mp")) {
    native_steps <- nirs4all_n4mp_steps(pipeline$steps)
    native_preprocessing <- n4m::n4m_preprocess_fit(X, native_steps)
    transformed <- nirs4all_n4mp_transform(native_preprocessing, X)
    state <- pipeline$learner$fit(transformed, as.numeric(y))
    states <- rep(list(NULL), length(pipeline$steps))
    owner <- "native_n4mp"
  } else if (!is.null(embedded)) {
    state <- n4m::n4m_fit(X, as.numeric(y), algo = "pls_simpls",
      n_components = pipeline$learner$spec$n_components,
      embedded_snv_savgol = embedded)
    states <- rep(list(NULL), length(pipeline$steps))
    owner <- "embedded_methods"
  } else {
    transformed <- nirs4all_fit_transform(X, pipeline$steps, y)
    fit_learner <- pipeline$learner
    if (di_pls && length(pipeline$steps)) {
      fit_params <- fit_learner$spec$params
      fit_params$X_target <- nirs4all_transform(target, pipeline$steps,
                                                transformed$states)
      fit_learner <- nirs4all_n4m_method("di_pls",
        fit_learner$spec$n_components, fit_params)
    }
    state <- fit_learner$fit(transformed$X,
      if (identical(task, "classification")) y else as.numeric(y))
    states <- transformed$states
    owner <- "external_r"
  }
  structure(list(steps = pipeline$steps, learner = pipeline$learner,
                 augmentations = pipeline$augmentations,
                 state = state, step_states = states,
                 native_preprocessing = if (identical(owner, "native_n4mp"))
                   native_preprocessing else NULL,
                 preprocessing_owner = owner,
                 task = task,
                 classes = if (identical(task, "classification")) levels(y) else NULL,
                 n_features = ncol(X),
                 feature_names = colnames(X)), class = "nirs4all_fitted")
}

#' Retrain a fitted pipeline on fresh data
#'
#' Reuses only the recipe (preprocessing steps and learner configuration), not
#' any fitted preprocessing state or model weights. The ordered input feature
#' schema must match the original fit. This also accepts a pipeline imported
#' with [nirs4all_import_trained_pipeline()].
#' @param object A fitted [nirs4all_fit()] result.
#' @param X Numeric samples-by-features matrix or a [nirs4all_from_formats()]
#'   dataset.
#' @param y Target vector, or `NULL` when `X` is a nirs4all dataset.
#' @return A newly fitted pipeline with independent state.
#' @export
nirs4all_retrain <- function(object, X, y = NULL) {
  if (!inherits(object, "nirs4all_fitted"))
    stop("object must be a fitted nirs4all pipeline", call. = FALSE)
  input <- if (inherits(X, "nirs4all_dataset")) X$X else X
  input <- nirs4all_matrix(input, object$n_features)
  if (!is.null(object$feature_names) &&
      !identical(colnames(input), object$feature_names))
    stop("X feature names or order differ from training", call. = FALSE)
  nirs4all_fit(nirs4all_pipeline(object$steps, object$learner,
                                augmentations = object$augmentations), X, y,
    preprocessing = if (identical(object$preprocessing_owner, "native_n4mp"))
      "native_n4mp" else "legacy")
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
    X else if (identical(object$preprocessing_owner, "native_n4mp"))
      nirs4all_n4mp_transform(object$native_preprocessing, X) else
        nirs4all_transform(X, object$steps, object$step_states)
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
