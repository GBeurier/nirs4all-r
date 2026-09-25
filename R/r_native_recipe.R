.nirs4all_r_model_aliases <- c(
  ranger = "r.ranger.regression",
  ranger_classifier = "r.ranger.classification",
  glmnet = "r.glmnet.regression",
  torch_mlp = "r.torch.mlp.regression",
  torch_mlp_classifier = "r.torch.mlp.classification")

nirs4all_r_recipe_bool <- function(value, fallback, name) {
  value <- nirs4all_portable_or(value, fallback)
  if (!is.logical(value) || length(value) != 1L || is.na(value))
    stop(sprintf("%s must be TRUE or FALSE", name), call. = FALSE)
  value
}

nirs4all_r_recipe_ranger_extra <- function(params) {
  fields <- c(mtry = "mtry", min_node_size = "min.node.size",
              max_depth = "max.depth", replace = "replace",
              sample_fraction = "sample.fraction")
  params <- nirs4all_portable_allowed_params(params, names(fields), "ranger")
  extra <- list()
  for (name in names(params)) {
    value <- params[[name]]
    extra[[fields[[name]]]] <- if (identical(name, "replace"))
      nirs4all_r_recipe_bool(value, NULL, name) else if
      (identical(name, "sample_fraction")) {
        number <- nirs4all_portable_number(value, NULL, name, minimum = 0)
        if (number <= 0 || number > 1)
          stop("sample_fraction must be in (0, 1]", call. = FALSE)
        number
      } else nirs4all_portable_number(value, NULL, name,
                                     integer = TRUE, minimum = 1L)
  }
  extra
}

nirs4all_r_recipe_learner <- function(class_name, params) {
  if (class_name %in% .nirs4all_r_model_aliases[c("ranger", "ranger_classifier")]) {
    params <- nirs4all_portable_allowed_params(params,
      c("num_trees", "seed", "mtry", "min_node_size", "max_depth",
        "replace", "sample_fraction"), "ranger")
    extra <- nirs4all_r_recipe_ranger_extra(
      params[setdiff(names(params), c("num_trees", "seed"))])
    constructor <- if (identical(class_name, .nirs4all_r_model_aliases[["ranger"]]))
      nirs4all_ranger else nirs4all_ranger_classifier
    return(do.call(constructor, c(list(
      num.trees = nirs4all_portable_number(params$num_trees, 500L,
        "num_trees", integer = TRUE, minimum = 1L),
      seed = nirs4all_portable_number(params$seed, 1L,
        "seed", integer = TRUE, minimum = 0L)), extra)))
  }
  if (identical(class_name, .nirs4all_r_model_aliases[["glmnet"]])) {
    params <- nirs4all_portable_allowed_params(params,
      c("lambda", "alpha", "standardize"), "glmnet")
    if (is.null(params$lambda))
      stop("glmnet recipe requires lambda", call. = FALSE)
    lambda <- nirs4all_portable_number(params$lambda, NULL, "lambda",
                                       minimum = 0)
    if (lambda <= 0) stop("lambda must be positive", call. = FALSE)
    return(nirs4all_glmnet(
      lambda = lambda,
      alpha = nirs4all_portable_number(params$alpha, 1, "alpha",
        minimum = 0),
      standardize = nirs4all_r_recipe_bool(params$standardize, TRUE,
        "standardize")))
  }
  if (class_name %in% .nirs4all_r_model_aliases[c("torch_mlp",
                                                "torch_mlp_classifier")]) {
    params <- nirs4all_portable_allowed_params(params,
      c("hidden", "epochs", "learning_rate", "seed"), "torch MLP")
    constructor <- if (identical(class_name,
                                 .nirs4all_r_model_aliases[["torch_mlp"]]))
      nirs4all_torch_mlp else nirs4all_torch_mlp_classifier
    learning_rate <- nirs4all_portable_number(params$learning_rate, 0.001,
      "learning_rate", minimum = 0)
    if (learning_rate <= 0)
      stop("learning_rate must be positive", call. = FALSE)
    return(constructor(
      hidden = nirs4all_portable_number(params$hidden, 32L,
        "hidden", integer = TRUE, minimum = 1L),
      epochs = nirs4all_portable_number(params$epochs, 100L,
        "epochs", integer = TRUE, minimum = 1L),
      learning_rate = learning_rate,
      seed = nirs4all_portable_number(params$seed, 1L,
        "seed", integer = TRUE, minimum = 0L)))
  }
  stop("unsupported R-native model alias", call. = FALSE)
}

nirs4all_r_recipe_model <- function(spec) {
  class_name <- .nirs4all_r_model_aliases[[spec$learner]]
  if (is.null(class_name))
    stop("unsupported R-native learner recipe", call. = FALSE)
  params <- switch(spec$learner,
    ranger = , ranger_classifier = {
      extras <- spec$extra
      if (!is.list(extras) || (length(extras) &&
          (is.null(names(extras)) || anyNA(names(extras)) ||
           anyDuplicated(names(extras)))))
        stop("unsupported ranger extra parameters in recipe", call. = FALSE)
      reverse <- c(mtry = "mtry", "min.node.size" = "min_node_size",
                   "max.depth" = "max_depth", replace = "replace",
                   "sample.fraction" = "sample_fraction")
      if (!all(names(extras) %in% names(reverse)))
        stop("unsupported ranger extra parameters in recipe", call. = FALSE)
      names(extras) <- unname(reverse[names(extras)])
      extras <- nirs4all_r_recipe_ranger_extra(extras)
      names(extras) <- unname(reverse[names(extras)])
      c(list(num_trees = spec$num_trees, seed = spec$seed), extras)
    },
    glmnet = list(lambda = spec$lambda, alpha = spec$alpha,
                  standardize = spec$standardize),
    torch_mlp = , torch_mlp_classifier = list(
      hidden = spec$hidden, epochs = spec$epochs,
      learning_rate = spec$learning_rate, seed = spec$seed))
  list(class = class_name, params = params)
}

#' Read an R-native ML/DL pipeline recipe
#'
#' Parses the same n4m preprocessing and feature-branch grammar as the
#' cross-language reader, but requires an explicitly R-namespaced model alias:
#' `r.ranger.regression`, `r.ranger.classification`,
#' `r.glmnet.regression`, `r.torch.mlp.regression`, or
#' `r.torch.mlp.classification`. These aliases are R-local recipes; they do not
#' assert binary or numerical equivalence to Python frameworks. Model
#' parameters are closed scalar data, never executable R code.
#' @param source JSON/YAML text, path or loaded pipeline definition.
#' @return An unfitted [nirs4all_pipeline()].
#' @export
nirs4all_r_pipeline_from_recipe <- function(source) {
  definition <- nirs4all_load_pipeline(source)
  stages <- definition$pipeline
  if (!length(stages)) stop("R-native recipe has no stages", call. = FALSE)
  model <- stages[[length(stages)]]
  if (!nirs4all_portable_named(model) ||
      anyDuplicated(names(model)) ||
      !all(names(model) %in% c("model", "name")) ||
      is.null(model$model) || !is.list(model$model) ||
      anyDuplicated(names(model$model)) ||
      !all(names(model$model) %in% c("class", "params")) ||
      !is.character(model$model$class) || length(model$model$class) != 1L ||
      !(model$model$class %in% .nirs4all_r_model_aliases))
    stop("R-native recipe requires a final, closed R model stage", call. = FALSE)
  learner <- nirs4all_r_recipe_learner(model$model$class,
    nirs4all_portable_or(model$model$params, list()))
  skeleton <- definition
  skeleton$pipeline[[length(stages)]] <- list(model = list(
    class = "n4m.PLS", params = list(n_components = 2L)))
  pipeline <- nirs4all_pipeline_from_portable(skeleton)
  nirs4all_pipeline(pipeline$steps, learner)
}
