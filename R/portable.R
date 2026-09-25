# Compatibility reader for the historical Python PipelineConfigs JSON/YAML
# subset. Numerical operations still go through n4m via the product pipeline.
.nirs4all_portable_classes <- list(
  kennard_stone = c("nirs4all.operators.splitters.KennardStoneSplitter",
                    "nirs4all.operators.splitters.splitters.KennardStoneSplitter",
                    "n4m.KennardStone", "splitters.kennard_stone"),
  snv = c("nirs4all.operators.transforms.SNV",
          "nirs4all.operators.transforms.StandardNormalVariate",
          "nirs4all.operators.transforms.scalers.StandardNormalVariate",
          "n4m.SNV", "preprocessing.snv"),
  savgol = c("nirs4all.operators.transforms.SavitzkyGolay",
             "nirs4all.operators.transforms.nirs.SavitzkyGolay",
             "n4m.SavitzkyGolay", "preprocessing.savgol"),
  pls = c("sklearn.cross_decomposition.PLSRegression",
          "sklearn.cross_decomposition._pls.PLSRegression",
          "n4m.PLS", "n4m.PLSRegression", "models.pls.pls_fit_simple")
)

nirs4all_portable_named <- function(value) {
  is.list(value) && !is.null(names(value)) && any(nzchar(names(value)))
}

nirs4all_portable_or <- function(value, fallback) {
  if (is.null(value)) fallback else value
}

nirs4all_portable_allowed_params <- function(params, allowed, label) {
  if (!is.list(params) || (length(params) &&
      (is.null(names(params)) || anyNA(names(params)) ||
       any(!nzchar(names(params))) || anyDuplicated(names(params)) ||
       !all(names(params) %in% allowed))))
    stop(sprintf("unsupported or duplicate %s parameter", label), call. = FALSE)
  params
}

nirs4all_portable_parse_text <- function(value, extension = "") {
  if (identical(extension, "json"))
    return(jsonlite::fromJSON(value, simplifyVector = FALSE))
  if (extension %in% c("yaml", "yml"))
    return(yaml::yaml.load(value, eval.expr = FALSE))
  parsed <- tryCatch(jsonlite::fromJSON(value, simplifyVector = FALSE), error = identity)
  if (!inherits(parsed, "error")) return(parsed)
  yaml::yaml.load(value, eval.expr = FALSE)
}

nirs4all_portable_strip_comments <- function(value) {
  if (!is.list(value)) return(value)
  if (!nirs4all_portable_named(value)) {
    keep <- !vapply(value, function(step)
      nirs4all_portable_named(step) && identical(names(step), "_comment"),
      logical(1))
    return(lapply(value[keep], nirs4all_portable_strip_comments))
  }
  value[["_comment"]] <- NULL
  lapply(value, nirs4all_portable_strip_comments)
}

#' Read a Python-style portable pipeline definition
#'
#' Accepts a list, JSON/YAML text or a `.json`, `.yaml`, or `.yml` file. This
#' reader preserves the historical nirs4all-core R subset; unsupported steps
#' are rejected by [nirs4all_parse_execution_plan()] rather than ignored.
#' @param source Pipeline list, serialized text or path.
#' @return A `nirs4all_pipeline_definition`.
#' @export
nirs4all_load_pipeline <- function(source) {
  data <- source
  if (is.character(source) && length(source) == 1L) {
    path <- if (!grepl("[\r\n]", source) &&
                (file.exists(source) ||
                 tolower(tools::file_ext(source)) %in% c("json", "yaml", "yml")))
      source else NULL
    if (!is.null(path)) {
      if (!file.exists(path))
        stop(sprintf("Configuration file does not exist: %s", path), call. = FALSE)
      data <- nirs4all_portable_parse_text(
        paste(readLines(path, warn = FALSE), collapse = "\n"),
        tolower(tools::file_ext(path)))
    } else {
      data <- nirs4all_portable_parse_text(source)
    }
  }
  if (!is.list(data))
    stop("Pipeline definition must be a list or mapping", call. = FALSE)
  if (!nirs4all_portable_named(data)) data <- list(pipeline = data)
  if (is.null(data$pipeline)) data$pipeline <- data$steps
  if (!is.list(data$pipeline) || nirs4all_portable_named(data$pipeline))
    stop("Pipeline definition needs a list under 'pipeline' or 'steps'", call. = FALSE)
  random_state <- data$random_state
  if (!is.null(random_state) &&
      (!is.numeric(random_state) || length(random_state) != 1L ||
       !is.finite(random_state) || random_state != floor(random_state) ||
       abs(random_state) > .Machine$integer.max))
    stop("random_state must be an integer", call. = FALSE)
  structure(list(name = nirs4all_portable_or(data$name, "pipeline"),
                 description = nirs4all_portable_or(data$description, ""),
                 random_state = if (is.null(random_state)) NULL else as.integer(random_state),
                 pipeline = nirs4all_portable_strip_comments(data$pipeline)),
            class = "nirs4all_pipeline_definition")
}

#' List operator classes in a portable definition
#' @param definition A loaded pipeline or nested definition.
#' @return Character vector of fully qualified class names.
#' @export
nirs4all_portable_class_names <- function(definition) {
  root <- if (inherits(definition, "nirs4all_pipeline_definition"))
    definition$pipeline else definition
  classes <- character()
  collect <- function(item) {
    if (!is.list(item)) return(invisible(NULL))
    if (nirs4all_portable_named(item) && is.character(item$class) &&
        length(item$class) == 1L) classes <<- c(classes, item$class)
    invisible(lapply(item, collect))
  }
  collect(root)
  classes
}

nirs4all_portable_number <- function(value, fallback, name, integer = FALSE,
                                     minimum = NULL) {
  value <- nirs4all_portable_or(value, fallback)
  if (is.null(value) || is.logical(value) || length(value) != 1L)
    stop(sprintf("%s must be a finite number", name), call. = FALSE)
  number <- suppressWarnings(as.numeric(value))
  if (!is.finite(number) || (integer &&
      (number != floor(number) || abs(number) > .Machine$integer.max)) ||
      (!is.null(minimum) && number < minimum))
    stop(sprintf("%s is outside its allowed range", name), call. = FALSE)
  if (integer) as.integer(number) else number
}

nirs4all_portable_components <- function(step) {
  if ("_range_" %in% names(step)) {
    if (!identical(step$param, "n_components") || length(step$`_range_`) != 3L)
      stop("only a three-value n_components range is supported", call. = FALSE)
    values <- vapply(step$`_range_`, nirs4all_portable_number, integer(1),
                     fallback = NULL, name = "n_components range",
                     integer = TRUE, minimum = 1L)
    if (values[[1L]] > values[[2L]])
      stop("n_components range start exceeds stop", call. = FALSE)
    count <- floor((as.double(values[[2L]]) - values[[1L]]) /
                   values[[3L]]) + 1
    if (count > 10000L) stop("n_components sweep exceeds 10000 variants", call. = FALSE)
    return(as.integer(values[[1L]] + (seq_len(count) - 1L) * values[[3L]]))
  }
  nirs4all_portable_number(step$model$params$n_components, 2L,
                           "n_components", integer = TRUE, minimum = 1L)
}

#' Translate the portable JSON/YAML subset into a product execution plan
#' @param source Definition accepted by [nirs4all_load_pipeline()].
#' @return Splitter, preprocessing and PLS component variants.
#' @export
nirs4all_parse_execution_plan <- function(source) {
  definition <- nirs4all_load_pipeline(source)
  splitter <- NULL
  preprocessing <- list()
  model <- NULL
  for (step in definition$pipeline) {
    if (!nirs4all_portable_named(step) || !is.null(model))
      stop("portable steps must be mappings and the model must be final", call. = FALSE)
    if (any(!names(step) %in% c("class", "params", "model", "_range_",
                                 "param", "name")))
      stop("unsupported portable step field", call. = FALSE)
    if (!is.null(step$class) && !is.null(step$model))
      stop("a step cannot contain both class and model", call. = FALSE)
    class_name <- step$class
    if (is.character(class_name) && length(class_name) == 1L) {
      params <- nirs4all_portable_or(step$params, list())
      if (class_name %in% .nirs4all_portable_classes$kennard_stone) {
        params <- nirs4all_portable_allowed_params(params, "test_size", "Kennard-Stone")
        if (!is.null(splitter)) stop("splitter may appear only once", call. = FALSE)
        if (length(preprocessing))
          stop("Kennard-Stone splitter must precede preprocessing", call. = FALSE)
        size <- nirs4all_portable_number(params$test_size, 0.25, "test_size")
        if (size <= 0 || size >= 1) stop("test_size must be between zero and one", call. = FALSE)
        splitter <- list(type = "KennardStone", params = list(test_size = size))
      } else if (class_name %in% .nirs4all_portable_classes$snv) {
        params <- nirs4all_portable_allowed_params(params,
          c("ddof", "with_mean", "with_std"), "SNV")
        ddof <- nirs4all_portable_number(params$ddof, 0L, "ddof",
                                          integer = TRUE, minimum = 0L)
        with_mean <- nirs4all_portable_or(params$with_mean, TRUE)
        with_std <- nirs4all_portable_or(params$with_std, TRUE)
        nirs4all_snv(ddof, with_mean, with_std)
        preprocessing[[length(preprocessing) + 1L]] <-
          list(type = "StandardNormalVariate", params = list(
            ddof = ddof, with_mean = with_mean, with_std = with_std))
      } else if (class_name %in% .nirs4all_portable_classes$savgol) {
        params <- nirs4all_portable_allowed_params(params,
          c("window_length", "window", "polyorder", "deriv", "delta", "mode", "cval"),
          "Savitzky-Golay")
        window <- nirs4all_portable_number(nirs4all_portable_or(params$window_length,
          params$window), 11L, "window_length", integer = TRUE, minimum = 3L)
        order <- nirs4all_portable_number(params$polyorder, 3L,
          "polyorder", integer = TRUE, minimum = 0L)
        deriv <- nirs4all_portable_number(params$deriv, 0L,
          "deriv", integer = TRUE, minimum = 0L)
        delta <- nirs4all_portable_number(params$delta, 1, "delta", minimum = 0)
        if (delta != 1)
          stop("portable Savitzky-Golay execution supports delta=1 only", call. = FALSE)
        mode <- nirs4all_portable_or(params$mode, "interp")
        if (is.numeric(mode) && length(mode) == 1L && is.finite(mode) &&
            mode == floor(mode) && mode >= 0L && mode <= 4L)
          mode <- c("mirror", "constant", "nearest", "wrap", "interp")[[mode + 1L]]
        cval <- nirs4all_portable_number(params$cval, 0, "cval")
        # Constructor validates odd window, derivative, degree and mode.
        nirs4all_savgol(window, order, deriv, delta, mode, cval)
        preprocessing[[length(preprocessing) + 1L]] <- list(
          type = "SavitzkyGolay", params = list(window, order, deriv,
                                                 match(mode, c("mirror", "constant", "nearest", "wrap", "interp")) - 1L,
                                                 cval))
      } else {
        stop(sprintf("unsupported portable class: %s", class_name), call. = FALSE)
      }
    } else if (is.list(step$model) &&
               is.character(step$model$class) &&
               length(step$model$class) == 1L &&
               step$model$class %in% .nirs4all_portable_classes$pls) {
      if (any(!names(step$model) %in% c("class", "params")))
        stop("unsupported portable model field", call. = FALSE)
      model <- step
      model$params <- nirs4all_portable_allowed_params(
        nirs4all_portable_or(step$model$params, list()),
        c("n_components", "algo", "center_x", "scale_x", "center_y", "scale_y"),
        "PLS")
    } else {
      stop("portable execution requires a PLSRegression model", call. = FALSE)
    }
  }
  if (is.null(model)) stop("portable execution requires PLSRegression", call. = FALSE)
  learner <- do.call(nirs4all_pls, c(list(n_components = 2L),
    model$params[intersect(names(model$params),
      c("algo", "center_x", "scale_x", "center_y", "scale_y"))]))
  list(splitter = splitter, preprocessing = preprocessing,
       n_components = nirs4all_portable_components(model),
       learner = learner$spec)
}

nirs4all_portable_dataset <- function(dataset) {
  if (inherits(dataset, "nirs4all_dataset"))
    return(list(X = dataset$X, y = dataset$y))
  if (!is.list(dataset) || is.null(dataset$X) || is.null(dataset$y))
    stop("dataset must contain X and y", call. = FALSE)
  X <- dataset$X
  if (is.list(X) && length(X) && is.list(X[[1L]])) {
    X <- do.call(rbind, lapply(X, function(row)
      as.numeric(unlist(row, use.names = FALSE))))
  } else if (!is.matrix(X)) {
    rows <- nirs4all_portable_or(dataset$rows, dataset$n_samples)
    cols <- nirs4all_portable_or(dataset$cols, dataset$n_features)
    values <- as.numeric(unlist(X, use.names = FALSE))
    if (is.null(rows) || is.null(cols) || length(values) != rows * cols)
      stop("flat X requires matching rows and cols", call. = FALSE)
    X <- matrix(values, as.integer(rows), as.integer(cols), byrow = TRUE)
  }
  X <- nirs4all_matrix(X)
  y <- as.numeric(unlist(dataset$y, use.names = FALSE))
  if (length(y) != nrow(X) || anyNA(y) || any(!is.finite(y)))
    stop("dataset y must be finite and row-aligned", call. = FALSE)
  list(X = X, y = y)
}

#' Run a portable Python-style JSON/YAML pipeline on R data
#'
#' This bounded compatibility path supports optional Kennard-Stone holdout,
#' SNV, Savitzky-Golay and PLS component sweeps. It delegates splitting and
#' numerical work to `n4m`; selection on the holdout is *not* an independent
#' test estimate. For general CV/OOF/refit use [nirs4all_dag_cv_refit_predict()].
#' @param source Definition accepted by [nirs4all_load_pipeline()].
#' @param dataset List with `X`, `y` and optional `rows`/`cols`, or a
#'   `nirs4all_dataset` from [nirs4all_from_formats()].
#' @return Split indices, per-variant predictions/RMSE and selected variant.
#' @export
nirs4all_run_portable_pipeline <- function(source, dataset) {
  definition <- nirs4all_load_pipeline(source)
  plan <- nirs4all_parse_execution_plan(definition)
  data <- nirs4all_portable_dataset(dataset)
  if (is.null(plan$splitter)) {
    indices <- seq.int(0L, nrow(data$X) - 1L)
    split <- list(kind = "all", trainIndices = indices, testIndices = indices)
  } else {
    indices <- n4m::kennard_stone_split(data$X,
      test_size = plan$splitter$params$test_size, zero_based = TRUE)
    split <- list(kind = "KennardStone", trainIndices = as.integer(indices$train),
                  testIndices = as.integer(indices$test))
  }
  train <- split$trainIndices + 1L
  validation <- split$testIndices + 1L
  steps <- lapply(plan$preprocessing, function(item) {
    if (identical(item$type, "StandardNormalVariate"))
      return(do.call(nirs4all_snv, item$params))
    params <- item$params
    nirs4all_savgol(params[[1L]], params[[2L]], params[[3L]], 1,
                    c("mirror", "constant", "nearest", "wrap", "interp")[[params[[4L]] + 1L]],
                    params[[5L]])
  })
  targets <- data$y[validation]
  variants <- lapply(plan$n_components, function(n) {
    learner <- plan$learner
    learner$n_components <- n
    pipeline <- nirs4all_pipeline(steps, do.call(nirs4all_pls,
      learner[setdiff(names(learner), "learner")] ))
    fitted <- nirs4all_fit(pipeline, data$X[train, , drop = FALSE], data$y[train])
    predictions <- as.numeric(nirs4all_predict(fitted,
      data$X[validation, , drop = FALSE]))
    list(n_components = as.integer(n),
         rmse = sqrt(mean((predictions - targets)^2)),
         predictions = predictions)
  })
  scores <- vapply(variants, `[[`, numeric(1), "rmse")
  list(name = definition$name, rows = as.integer(nrow(data$X)),
       cols = as.integer(ncol(data$X)), split = split,
       preprocessing = plan$preprocessing, variants = variants,
       selected = variants[[which.min(scores)]], targets = as.numeric(targets),
       evaluation = list(scope = if (identical(split$kind, "all"))
         "training" else "selection_validation", independent_test = FALSE))
}
