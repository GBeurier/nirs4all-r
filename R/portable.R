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
  local_snv = c("n4m.LSNV", "preprocessing.local_snv"),
  robust_snv = c("n4m.RNV", "preprocessing.robust_snv"),
  area_normalization = c("n4m.AreaNormalization",
                         "preprocessing.area_normalization"),
  detrend = c("n4m.Detrend", "preprocessing.detrend"),
  msc = c("n4m.MSC", "preprocessing.msc"),
  emsc = c("n4m.EMSC", "preprocessing.emsc"),
  spa = "n4m.SPA",
  pls = c("sklearn.cross_decomposition.PLSRegression",
          "sklearn.cross_decomposition._pls.PLSRegression",
          "n4m.PLS", "n4m.PLSRegression", "models.pls.pls_fit_simple"),
  sparse_pls_da = c("n4m.SparsePLSDA", "pls4all.sklearn.SparsePLSDAClassifier")
)

# Only affine MethodResult regressors with a qualified held-out prediction
# path in both R and Python belong in the shared recipe vocabulary.
.nirs4all_portable_affine <- c(
  ridge = "n4m.Ridge", ridge_pls = "n4m.RidgePLS",
  robust_pls = "n4m.RobustPLS", cppls = "n4m.CPPLS",
  sparse_simpls = "n4m.SparseSIMPLS", ecr = "n4m.ECR",
  continuum_regression = "n4m.ContinuumRegression",
  mir_pls = "n4m.MIRPLS")

.nirs4all_portable_affine_params <- list(
  ridge = "alpha", ridge_pls = "ridge_lambda",
  robust_pls = c("huber_k", "max_irls_iter"), cppls = "gamma",
  sparse_simpls = "sparsity_lambda", ecr = "alpha",
  continuum_regression = "tau", mir_pls = character())

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

nirs4all_portable_branch_plan <- function(branch_value) {
  if (!is.list(branch_value) || length(branch_value) < 2L)
    stop("portable feature branch needs at least two branches", call. = FALSE)
  if (nirs4all_portable_named(branch_value)) {
    branch_names <- names(branch_value)
  } else {
    branch_names <- character(length(branch_value))
    branch_value <- lapply(seq_along(branch_value), function(index) {
      branch <- branch_value[[index]]
      default_name <- sprintf("branch_%d", index - 1L)
      if (nirs4all_portable_named(branch) && "steps" %in% names(branch)) {
        if (any(!names(branch) %in% c("name", "steps")) ||
            !is.character(branch$name) || length(branch$name) != 1L)
          stop("unsupported named portable feature branch", call. = FALSE)
        branch_names[[index]] <<- branch$name
        return(branch$steps)
      }
      branch_names[[index]] <<- default_name
      if (nirs4all_portable_named(branch) && "class" %in% names(branch))
        return(list(branch))
      branch
    })
  }
  if (anyNA(branch_names) || anyDuplicated(branch_names) ||
      !all(grepl("^[A-Za-z][A-Za-z0-9_.-]*$", branch_names)) ||
      any(branch_names %in% c("parallel", "n_jobs")))
    stop("unsupported portable feature branch name", call. = FALSE)
  branches <- lapply(branch_value, function(branch) {
    if (!is.list(branch) || !length(branch) || nirs4all_portable_named(branch) ||
        !all(vapply(branch, function(step)
          nirs4all_portable_named(step) &&
            is.character(step$class) && length(step$class) == 1L &&
            all(names(step) %in% c("class", "params", "name")), logical(1))))
      stop("portable feature branches require preprocessing-only step lists",
           call. = FALSE)
    fake <- list(pipeline = c(branch, list(list(model = list(
      class = "n4m.PLS", params = list(n_components = 2L))))))
    plan <- nirs4all_parse_execution_plan(fake)
    if (!is.null(plan$splitter) || length(plan$preprocessing) != length(branch))
      stop("portable feature branches cannot contain splitters or models",
           call. = FALSE)
    plan$preprocessing
  })
  names(branches) <- branch_names
  list(type = "FeatureConcat", params = list(branches = branches))
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

#' Export a qualified R pipeline recipe
#'
#' Writes n4m-backed preprocessing with named feature branches and
#' `merge: features` syntax when needed. The default `cross_language` scope
#' qualifies native PLS or sparse PLS-DA recipes; LSNV, RNV, area
#' normalization, detrend, MSC, EMSC and SPA are tested against Python n4m but not
#' yet Core/WASM. Non-default SNV is refused in that scope because Core/WASM
#' ignores its parameters. The `r_native` scope permits selected ranger,
#' glmnet and torch MLP learners under explicit R-only aliases. It does not
#' transfer their trained binaries or assert Python/WASM equivalence.
#' @param pipeline An unfitted [nirs4all_pipeline()].
#' @param format `"json"` or `"yaml"`.
#' @param file Optional output path. If omitted, returns serialized text.
#' @param name Pipeline name in the exported definition.
#' @param scope `"cross_language"` for the qualified n4m-only recipe (the
#'   default), or `"r_native"` for an R-specific learner recipe. The latter
#'   is not a Python/WASM model alias and permits non-default SNV parameters.
#' @return Serialized text, invisibly if `file` is provided.
#' @export
nirs4all_export_pipeline <- function(pipeline, format = c("json", "yaml"),
                                    file = NULL, name = "pipeline",
                                    scope = c("cross_language", "r_native")) {
  if (!inherits(pipeline, "nirs4all_pipeline"))
    stop("pipeline must be an unfitted nirs4all_pipeline", call. = FALSE)
  format <- match.arg(format)
  scope <- match.arg(scope)
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name))
    stop("name must be a non-empty string", call. = FALSE)
  if (!is.null(file) && (!is.character(file) || length(file) != 1L ||
                        is.na(file) || !nzchar(file)))
    stop("file must be a non-empty path", call. = FALSE)
  encode_step <- function(step) {
    if (identical(step$kind, "snv")) {
      if (!identical(step$ddof, 0L) || !identical(step$with_mean, TRUE) ||
          !identical(step$with_std, TRUE)) {
        if (identical(scope, "cross_language"))
          stop("cross-language recipe export supports default SNV only; Core/WASM currently ignores SNV parameters", call. = FALSE)
        return(list(class = "n4m.SNV", params = list(ddof = step$ddof,
          with_mean = step$with_mean, with_std = step$with_std)))
      }
      return(list(class = "n4m.SNV"))
    }
    if (identical(step$kind, "savgol")) {
      if (!isTRUE(step$delta == 1))
        stop("cross-language recipe export supports Savitzky-Golay delta=1 only",
             call. = FALSE)
      return(list(class = "n4m.SavitzkyGolay",
                  params = list(window_length = step$window_length,
                                polyorder = step$polyorder, deriv = step$deriv,
                                delta = step$delta, mode = step$mode,
                                cval = step$cval)))
    }
    if (identical(step$kind, "local_snv"))
      return(list(class = "n4m.LSNV", params = list(
        window = step$window, pad_mode = step$pad_mode,
        constant_value = step$constant_value)))
    if (identical(step$kind, "robust_snv"))
      return(list(class = "n4m.RNV", params = list(
        with_center = step$with_center, with_scale = step$with_scale,
        k = step$k)))
    if (identical(step$kind, "area_normalization"))
      return(list(class = "n4m.AreaNormalization",
                  params = list(method = step$method)))
    if (identical(step$kind, "detrend"))
      return(list(class = "n4m.Detrend",
                  params = list(polyorder = step$polyorder)))
    if (identical(step$kind, "msc")) return(list(class = "n4m.MSC"))
    if (identical(step$kind, "emsc"))
      return(list(class = "n4m.EMSC",
                  params = list(degree = step$degree)))
    if (identical(step$kind, "spa"))
      return(list(class = "n4m.SPA", params = list(
        top_k = step$top_k, n_components = step$n_components)))
    stop(sprintf("cross-language recipe export does not support step '%s'",
                 step$kind), call. = FALSE)
  }
  steps <- list()
  for (step in pipeline$steps) {
    if (identical(step$kind, "concat")) {
      if (any(names(step$branches) %in% c("parallel", "n_jobs")))
        stop("portable feature branch uses a reserved name", call. = FALSE)
      branches <- lapply(step$branches, function(branch)
        lapply(branch, encode_step))
      steps <- c(steps, list(list(branch = branches), list(merge = "features")))
    } else {
      steps[[length(steps) + 1L]] <- encode_step(step)
    }
  }
  spec <- pipeline$learner$spec
  if (is.list(spec) && identical(spec$learner, "sparse_pls_da")) {
    steps[[length(steps) + 1L]] <- list(model = list(
      class = "n4m.SparsePLSDA",
      params = list(n_components = spec$n_components,
                    sparsity_lambda = spec$sparsity_lambda)))
  } else if (is.list(spec) && identical(spec$learner, "n4m_method") &&
             spec$method %in% names(.nirs4all_portable_affine)) {
    params <- spec$params
    if (identical(spec$method, "ridge") && !is.null(params$ridge_lambda)) {
      params$alpha <- params$ridge_lambda
      params$ridge_lambda <- NULL
    }
    component_params <- if (identical(spec$method, "ridge")) list() else
      list(n_components = spec$n_components)
    model <- list(class = unname(.nirs4all_portable_affine[[spec$method]]))
    params <- c(component_params, params)
    if (length(params)) model$params <- params
    steps[[length(steps) + 1L]] <- list(model = model)
  } else if (identical(scope, "r_native") && is.list(spec) &&
             is.character(spec$learner) && length(spec$learner) == 1L &&
             spec$learner %in% c("ranger", "ranger_classifier", "glmnet",
               "torch_mlp", "torch_mlp_classifier")) {
    steps[[length(steps) + 1L]] <- list(model =
      nirs4all_r_recipe_model(spec))
  } else {
    if (!is.list(spec) || !identical(spec$learner, "pls") ||
        !identical(spec$algo, "pls_simpls") ||
        !all(vapply(spec[c("center_x", "scale_x", "center_y", "scale_y")],
                    identical, logical(1), TRUE)))
      stop("cross-language recipe export supports qualified native n4m models only",
           call. = FALSE)
    steps[[length(steps) + 1L]] <- list(model = list(
      class = "n4m.PLS",
      params = list(n_components = spec$n_components)))
  }
  definition <- list(name = name, pipeline = steps)
  serialized <- if (identical(format, "json"))
    as.character(jsonlite::toJSON(definition, auto_unbox = TRUE, pretty = TRUE))
  else yaml::as.yaml(definition)
  if (!is.null(file)) writeLines(serialized, file, useBytes = TRUE)
  if (is.null(file)) serialized else invisible(serialized)
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
    if (values[[3L]] < 1L)
      stop("n_components range step must be positive", call. = FALSE)
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
#' @return Splitter, preprocessing and native model component variants.
#' @export
nirs4all_parse_execution_plan <- function(source) {
  definition <- nirs4all_load_pipeline(source)
  splitter <- NULL
  preprocessing <- list()
  model <- NULL
  pending_branch <- NULL
  for (step in definition$pipeline) {
    if (!nirs4all_portable_named(step) || !is.null(model))
      stop("portable steps must be mappings and the model must be final", call. = FALSE)
    if ("branch" %in% names(step)) {
      if (!identical(names(step), "branch") || !is.null(pending_branch))
        stop("unsupported portable feature branch structure", call. = FALSE)
      pending_branch <- nirs4all_portable_branch_plan(step$branch)
      next
    }
    if ("merge" %in% names(step)) {
      if (is.null(pending_branch) || !identical(names(step), "merge") ||
          !identical(step$merge, "features"))
        stop("portable feature branch requires merge: features", call. = FALSE)
      preprocessing[[length(preprocessing) + 1L]] <- pending_branch
      pending_branch <- NULL
      next
    }
    if (!is.null(pending_branch))
      stop("portable feature branch must be followed by merge: features",
           call. = FALSE)
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
      } else if (class_name %in% .nirs4all_portable_classes$local_snv) {
        params <- nirs4all_portable_allowed_params(params,
          c("window", "pad_mode", "constant_value"), "LSNV")
        values <- list(window = nirs4all_portable_number(params$window, 11L,
          "window", integer = TRUE, minimum = 3L),
          pad_mode = nirs4all_portable_or(params$pad_mode, "reflect"),
          constant_value = nirs4all_portable_number(params$constant_value, 0,
            "constant_value"))
        do.call(nirs4all_local_snv, values)
        preprocessing[[length(preprocessing) + 1L]] <-
          list(type = "LocalStandardNormalVariate", params = values)
      } else if (class_name %in% .nirs4all_portable_classes$robust_snv) {
        params <- nirs4all_portable_allowed_params(params,
          c("with_center", "with_scale", "k"), "RNV")
        values <- list(with_center = nirs4all_portable_or(params$with_center, TRUE),
          with_scale = nirs4all_portable_or(params$with_scale, TRUE),
          k = nirs4all_portable_number(params$k, 1.4826, "k", minimum = 0))
        do.call(nirs4all_robust_snv, values)
        preprocessing[[length(preprocessing) + 1L]] <-
          list(type = "RobustStandardNormalVariate", params = values)
      } else if (class_name %in% .nirs4all_portable_classes$area_normalization) {
        params <- nirs4all_portable_allowed_params(params, "method",
                                                   "AreaNormalization")
        values <- list(method = nirs4all_portable_or(params$method, "sum"))
        do.call(nirs4all_area_normalization, values)
        preprocessing[[length(preprocessing) + 1L]] <-
          list(type = "AreaNormalization", params = values)
      } else if (class_name %in% .nirs4all_portable_classes$detrend) {
        params <- nirs4all_portable_allowed_params(params, "polyorder", "Detrend")
        values <- list(polyorder = nirs4all_portable_number(params$polyorder,
          1L, "polyorder", integer = TRUE, minimum = 0L))
        do.call(nirs4all_detrend, values)
        preprocessing[[length(preprocessing) + 1L]] <-
          list(type = "Detrend", params = values)
      } else if (class_name %in% .nirs4all_portable_classes$msc) {
        nirs4all_portable_allowed_params(params, character(), "MSC")
        preprocessing[[length(preprocessing) + 1L]] <-
          list(type = "MultiplicativeScatterCorrection", params = list())
      } else if (class_name %in% .nirs4all_portable_classes$emsc) {
        params <- nirs4all_portable_allowed_params(params, "degree", "EMSC")
        values <- list(degree = nirs4all_portable_number(params$degree,
          2L, "degree", integer = TRUE, minimum = 1L))
        do.call(nirs4all_emsc, values)
        preprocessing[[length(preprocessing) + 1L]] <-
          list(type = "ExtendedMultiplicativeScatterCorrection", params = values)
      } else if (class_name %in% .nirs4all_portable_classes$spa) {
        params <- nirs4all_portable_allowed_params(params,
          c("top_k", "n_components"), "SPA")
        if (!is.numeric(params$top_k) ||
            (!is.null(params$n_components) && !is.numeric(params$n_components)))
          stop("SPA parameters must be numeric integers", call. = FALSE)
        values <- list(top_k = nirs4all_portable_number(params$top_k,
          NULL, "top_k", integer = TRUE, minimum = 1L),
          n_components = nirs4all_portable_number(params$n_components,
            2L, "n_components", integer = TRUE, minimum = 1L))
        do.call(nirs4all_spa, values)
        preprocessing[[length(preprocessing) + 1L]] <-
          list(type = "SuccessiveProjectionsAlgorithm", params = values)
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
    } else if (is.list(step$model) &&
               is.character(step$model$class) &&
               length(step$model$class) == 1L &&
               step$model$class %in% .nirs4all_portable_classes$sparse_pls_da) {
      if (any(!names(step$model) %in% c("class", "params")))
        stop("unsupported portable model field", call. = FALSE)
      model <- step
      model$params <- nirs4all_portable_allowed_params(
        nirs4all_portable_or(step$model$params, list()),
        c("n_components", "sparsity_lambda"), "sparse PLS-DA")
    } else if (is.list(step$model) &&
               is.character(step$model$class) &&
               length(step$model$class) == 1L &&
               step$model$class %in% .nirs4all_portable_affine) {
      if (any(!names(step$model) %in% c("class", "params")))
        stop("unsupported portable model field", call. = FALSE)
      model <- step
      method <- names(.nirs4all_portable_affine)[match(
        step$model$class, .nirs4all_portable_affine)]
      model$params <- nirs4all_portable_allowed_params(
        nirs4all_portable_or(step$model$params, list()),
        c(if (identical(method, "ridge")) character() else "n_components",
          .nirs4all_portable_affine_params[[method]]),
        method)
      if (identical(method, "ridge") && "_range_" %in% names(step))
        stop("ridge has no component sweep", call. = FALSE)
    } else {
      stop("portable execution requires a qualified native n4m model", call. = FALSE)
    }
  }
  if (!is.null(pending_branch))
    stop("portable feature branch is missing merge: features", call. = FALSE)
  if (is.null(model)) stop("portable execution requires a native model", call. = FALSE)
  classifier <- model$model$class %in% .nirs4all_portable_classes$sparse_pls_da
  affine <- model$model$class %in% .nirs4all_portable_affine
  learner <- if (classifier) nirs4all_sparse_pls_da(
    sparsity_lambda = nirs4all_portable_number(
      model$params$sparsity_lambda, 0.05, "sparsity_lambda", minimum = 0)) else if (affine) {
    method <- names(.nirs4all_portable_affine)[match(
      model$model$class, .nirs4all_portable_affine)]
    params <- model$params[setdiff(names(model$params), "n_components")]
    if (identical(method, "ridge") && !is.null(params$alpha)) {
      params$ridge_lambda <- params$alpha
      params$alpha <- NULL
    }
    if (!length(params)) params <- list()
    nirs4all_n4m_method(method, params = params)
  } else
    do.call(nirs4all_pls, c(list(n_components = 2L),
      model$params[intersect(names(model$params),
        c("algo", "center_x", "scale_x", "center_y", "scale_y"))]))
  list(splitter = splitter, preprocessing = preprocessing,
       n_components = nirs4all_portable_components(model),
       learner = learner$spec)
}

#' Convert a portable JSON/YAML recipe into an R pipeline
#'
#' The bounded reader accepts native SNV, Savitzky-Golay, LSNV, RNV, area
#' normalization, detrend, train-fitted MSC/EMSC/SPA, feature-only branches merged
#' by concatenation, PLS regression, qualified affine n4m regressions and
#' sparse PLS-DA classification.
#' Splitters and component sweeps are refused because a single fitted pipeline
#' cannot represent an entire selection experiment.
#' @param source Definition accepted by [nirs4all_load_pipeline()].
#' @return An unfitted [nirs4all_pipeline()].
#' @export
nirs4all_portable_steps <- function(preprocessing) {
  lapply(preprocessing, function(step) {
    if (identical(step$type, "FeatureConcat"))
      return(nirs4all_concat(lapply(step$params$branches,
        nirs4all_portable_steps)))
    if (identical(step$type, "StandardNormalVariate")) {
      params <- step$params
      return(nirs4all_snv(params$ddof, params$with_mean, params$with_std))
    }
    if (identical(step$type, "SavitzkyGolay")) {
      params <- step$params
      mode <- c("mirror", "constant", "nearest", "wrap", "interp")[[params[[4L]] + 1L]]
      return(nirs4all_savgol(params[[1L]], params[[2L]], params[[3L]],
                             delta = 1, mode = mode, cval = params[[5L]]))
    }
    constructors <- list(
      LocalStandardNormalVariate = nirs4all_local_snv,
      RobustStandardNormalVariate = nirs4all_robust_snv,
      AreaNormalization = nirs4all_area_normalization,
      Detrend = nirs4all_detrend,
      MultiplicativeScatterCorrection = nirs4all_msc,
      ExtendedMultiplicativeScatterCorrection = nirs4all_emsc,
      SuccessiveProjectionsAlgorithm = nirs4all_spa)
    constructor <- constructors[[step$type]]
    if (!is.null(constructor)) return(do.call(constructor, step$params))
    stop("unsupported portable preprocessing step", call. = FALSE)
  })
}

nirs4all_pipeline_from_portable <- function(source) {
  plan <- nirs4all_parse_execution_plan(source)
  if (!is.null(plan$splitter) || length(plan$n_components) != 1L)
    stop("a single fitted pipeline cannot include a splitter or component sweep",
         call. = FALSE)
  steps <- nirs4all_portable_steps(plan$preprocessing)
  spec <- plan$learner
  learner <- if (identical(spec$learner, "sparse_pls_da"))
    nirs4all_sparse_pls_da(plan$n_components[[1L]], spec$sparsity_lambda) else if (
      identical(spec$learner, "n4m_method"))
    nirs4all_n4m_method(spec$method, plan$n_components[[1L]], spec$params) else
    nirs4all_pls(plan$n_components[[1L]], algo = spec$algo,
      center_x = spec$center_x, scale_x = spec$scale_x,
      center_y = spec$center_y, scale_y = spec$scale_y)
  nirs4all_pipeline(steps, learner)
}

nirs4all_portable_generator_options <- function(node) {
  if (!nirs4all_portable_named(node))
    stop("portable generator stages must be mappings", call. = FALSE)
  if ("_or_" %in% names(node)) {
    if (!identical(names(node), "_or_") || !is.list(node$`_or_`) ||
        nirs4all_portable_named(node$`_or_`) || !length(node$`_or_`))
      stop("portable _or_ must contain a non-empty list of steps", call. = FALSE)
    if (!all(vapply(node$`_or_`, function(option)
        nirs4all_portable_named(option) &&
        !any(names(option) %in% c("_or_", "_cartesian_")), logical(1))))
      stop("nested portable generators are unsupported", call. = FALSE)
    return(lapply(node$`_or_`, list))
  }
  if ("_cartesian_" %in% names(node)) {
    if (!identical(names(node), "_cartesian_") ||
        !is.list(node$`_cartesian_`) ||
        nirs4all_portable_named(node$`_cartesian_`) ||
        !length(node$`_cartesian_`))
      stop("portable _cartesian_ needs non-empty stages without modifiers",
           call. = FALSE)
    variants <- list(list())
    for (stage in node$`_cartesian_`) {
      if (nirs4all_portable_named(stage) && "_cartesian_" %in% names(stage))
        stop("nested portable cartesian generators are unsupported", call. = FALSE)
      options <- nirs4all_portable_generator_options(stage)
      if (length(variants) > floor(10000 / length(options)))
        stop("portable generator exceeds 10000 variants", call. = FALSE)
      variants <- unlist(lapply(variants, function(prefix)
        lapply(options, function(option) c(prefix, option))), recursive = FALSE)
    }
    return(variants)
  }
  list(list(node))
}

#' Expand a bounded Python-style generator recipe into R pipelines
#'
#' Supports the n4m preprocessing classes accepted by the portable reader,
#' a PLS component `_range_`,
#' and preprocessing `_or_` or `_cartesian_` stages without selection modifiers.
#' Variant order matches Python's left-to-right Cartesian product. Unsupported
#' generators and splitters are rejected. Pass the resulting named list to
#' [nirs4all_dag_cv_refit_predict()] for native CV/OOF selection and refit.
#' @param source Definition accepted by [nirs4all_load_pipeline()].
#' @return Named list of unfitted [nirs4all_pipeline()] variants.
#' @export
nirs4all_expand_portable_pipelines <- function(source) {
  definition <- nirs4all_load_pipeline(source)
  variants <- list(list())
  for (node in definition$pipeline) {
    options <- nirs4all_portable_generator_options(node)
    if (length(variants) > floor(10000 / length(options)))
      stop("portable generator exceeds 10000 variants", call. = FALSE)
    variants <- unlist(lapply(variants, function(prefix)
      lapply(options, function(option) c(prefix, option))), recursive = FALSE)
  }
  pipelines <- list()
  for (steps in variants) {
    plan <- nirs4all_parse_execution_plan(list(pipeline = steps))
    if (!is.null(plan$splitter))
      stop("portable pipeline variants do not include splitters", call. = FALSE)
    if (length(pipelines) > 10000L - length(plan$n_components))
      stop("portable generator exceeds 10000 variants", call. = FALSE)
    for (components in plan$n_components) {
      recipe <- steps
      model_index <- length(recipe)
      recipe[[model_index]]$`_range_` <- NULL
      recipe[[model_index]]$param <- NULL
      if (is.null(recipe[[model_index]]$model$params))
        recipe[[model_index]]$model$params <- list()
      recipe[[model_index]]$model$params$n_components <- components
      pipelines[[length(pipelines) + 1L]] <-
        nirs4all_pipeline_from_portable(list(pipeline = recipe))
    }
  }
  names(pipelines) <- sprintf("variant_%04d", seq_along(pipelines))
  pipelines
}

nirs4all_portable_dataset <- function(dataset, classification = FALSE) {
  if (inherits(dataset, "nirs4all_dataset"))
    dataset <- list(X = dataset$X, y = dataset$y)
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
  class_values <- NULL
  if (classification) {
    y <- dataset$y
    if (is.list(y)) y <- unlist(y, use.names = FALSE)
    if (is.numeric(y) && !is.factor(y) && !is.matrix(y) &&
        length(y) == nrow(X) && !anyNA(y) && all(is.finite(y))) {
      class_values <- sort(unique(y))
      y <- factor(y, levels = class_values)
    }
    if (is.character(y)) y <- factor(y)
    if (!is.factor(y) || is.ordered(y) || length(y) != nrow(X) ||
        anyNA(y) || nlevels(y) < 2L ||
        any(tabulate(as.integer(y), nbins = nlevels(y)) == 0L))
      stop("classification dataset y must contain at least two observed classes and align with X",
           call. = FALSE)
    if (is.null(class_values)) class_values <- levels(y)
  } else {
    y <- as.numeric(unlist(dataset$y, use.names = FALSE))
    if (length(y) != nrow(X) || anyNA(y) || any(!is.finite(y)))
      stop("dataset y must be finite and row-aligned", call. = FALSE)
  }
  list(X = X, y = y, class_values = class_values)
}

#' Run a portable Python-style JSON/YAML pipeline on R data
#'
#' This bounded compatibility path supports optional Kennard-Stone holdout,
#' native n4m preprocessing steps and PLS component sweeps. Regression PLS
#' variants use RMSE; native sparse PLS-DA variants use classification accuracy.
#' It delegates splitting and numerical work to `n4m`; selection on the holdout is *not* an independent
#' test estimate. For general CV/OOF/refit use [nirs4all_dag_cv_refit_predict()].
#' @param source Definition accepted by [nirs4all_load_pipeline()].
#' @param dataset List with `X`, `y` and optional `rows`/`cols`, or a
#'   `nirs4all_dataset` from [nirs4all_from_formats()].
#' @return Split indices, per-variant predictions and task-specific score,
#'   plus the selected variant.
#' @export
nirs4all_run_portable_pipeline <- function(source, dataset) {
  definition <- nirs4all_load_pipeline(source)
  plan <- nirs4all_parse_execution_plan(definition)
  classification <- identical(plan$learner$learner, "sparse_pls_da")
  data <- nirs4all_portable_dataset(dataset, classification)
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
  steps <- nirs4all_portable_steps(plan$preprocessing)
  targets <- data$y[validation]
  render_labels <- function(values) {
    if (!classification) return(as.numeric(values))
    unname(data$class_values[as.integer(values)])
  }
  variants <- lapply(plan$n_components, function(n) {
    learner <- plan$learner
    learner$n_components <- n
    controller <- if (classification) nirs4all_sparse_pls_da(
      n, learner$sparsity_lambda) else if (identical(learner$learner, "n4m_method"))
      nirs4all_n4m_method(learner$method, n, learner$params) else
      do.call(nirs4all_pls, learner[setdiff(names(learner), "learner")])
    pipeline <- nirs4all_pipeline(steps, controller)
    fitted <- nirs4all_fit(pipeline, data$X[train, , drop = FALSE], data$y[train])
    predictions <- nirs4all_predict(fitted, data$X[validation, , drop = FALSE])
    if (classification)
      return(list(n_components = as.integer(n),
                  accuracy = mean(predictions == targets),
                  predictions = render_labels(predictions)))
    predictions <- as.numeric(predictions)
    list(n_components = as.integer(n), rmse = sqrt(mean((predictions - targets)^2)),
         predictions = predictions)
  })
  score_name <- if (classification) "accuracy" else "rmse"
  scores <- vapply(variants, `[[`, numeric(1), score_name)
  selected <- if (classification) which.max(scores) else which.min(scores)
  list(name = definition$name, rows = as.integer(nrow(data$X)),
       cols = as.integer(ncol(data$X)), split = split,
       preprocessing = plan$preprocessing, variants = variants,
       selected = variants[[selected]],
       targets = render_labels(targets),
       evaluation = list(scope = if (identical(split$kind, "all"))
         "training" else "selection_validation", independent_test = FALSE))
}
