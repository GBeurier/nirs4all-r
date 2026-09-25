library(nirs4all)

strict <- identical(Sys.getenv("NIRS4ALL_REQUIRE_DAG_PARITY"), "1")
have_ml <- requireNamespace("ranger", quietly = TRUE) &&
  requireNamespace("glmnet", quietly = TRUE)
have_torch <- requireNamespace("torch", quietly = TRUE) &&
  torch::torch_is_installed()
if (strict && (!have_ml || !have_torch))
  stop("strict R-native recipe parity requires ranger, glmnet and torch CPU")

X <- outer(seq_len(21L), seq_len(8L), function(i, j)
  sin(i * j / 9) + cos(i / 3 + j) + i * j / 80)
y <- 1.3 + 0.6 * X[, 2L] - 0.25 * X[, 6L]
steps <- list(nirs4all_snv(ddof = 1L), nirs4all_msc())

check_recipe <- function(pipeline, X, y, tolerance) {
  reference <- nirs4all_fit(pipeline, X, y)
  for (format in c("json", "yaml")) {
    recipe <- nirs4all_export_pipeline(pipeline, format = format,
                                      scope = "r_native")
    imported <- nirs4all_r_pipeline_from_recipe(recipe)
    fitted <- nirs4all_fit(imported, X, y)
    stopifnot(identical(imported$learner$spec, pipeline$learner$spec))
    if (identical(reference$task, "classification")) {
      stopifnot(identical(predict(fitted, X), predict(reference, X)),
        max(abs(nirs4all_predict_proba(fitted, X) -
                  nirs4all_predict_proba(reference, X))) < tolerance)
    } else {
      stopifnot(max(abs(predict(fitted, X) - predict(reference, X))) < tolerance)
    }
    if (identical(format, "json")) {
      malformed <- jsonlite::fromJSON(recipe, simplifyVector = FALSE)
      last <- length(malformed$pipeline)
      malformed$pipeline[[last]]$model$class <- "r.system"
      stopifnot(inherits(try(nirs4all_r_pipeline_from_recipe(
        jsonlite::toJSON(malformed, auto_unbox = TRUE)), silent = TRUE),
        "try-error"))
      malformed <- jsonlite::fromJSON(recipe, simplifyVector = FALSE)
      malformed$pipeline[[last]]$model$params$command <- "bad"
      stopifnot(inherits(try(nirs4all_r_pipeline_from_recipe(
        jsonlite::toJSON(malformed, auto_unbox = TRUE)), silent = TRUE),
        "try-error"))
    }
  }
  stopifnot(inherits(try(nirs4all_export_pipeline(pipeline), silent = TRUE),
                     "try-error"))
}

if (have_ml) {
  forest <- nirs4all_pipeline(steps,
    nirs4all_ranger(num.trees = 35L, seed = 7L, mtry = 3L,
                    min.node.size = 2L))
  check_recipe(forest, X, y, 1e-12)
  branched <- nirs4all_pipeline(list(nirs4all_concat(list(
    snv = list(nirs4all_snv()), msc = list(nirs4all_msc())))),
    nirs4all_ranger(num.trees = 35L, seed = 7L, mtry = 3L))
  check_recipe(branched, X, y, 1e-12)
  elastic <- nirs4all_pipeline(steps,
    nirs4all_glmnet(lambda = 0.03, alpha = 0.4, standardize = FALSE))
  check_recipe(elastic, X, y, 1e-12)

  classes <- factor(rep(c("a", "b", "c"), each = 7L))
  classifier <- nirs4all_pipeline(list(nirs4all_snv()),
    nirs4all_ranger_classifier(num.trees = 35L, seed = 7L, mtry = 3L))
  check_recipe(classifier, X, classes, 1e-12)

  cli <- Sys.getenv("NIRS4ALL_DAGML_CLI")
  if (!nzchar(cli)) cli <- Sys.which("dag-ml-cli")
  if (strict && (!nzchar(cli) || !file.exists(cli) ||
                 !requireNamespace("dagml", quietly = TRUE)))
    stop("strict R-native recipe parity requires dagml and dag-ml-cli")
  if (nzchar(cli) && file.exists(cli) &&
      requireNamespace("dagml", quietly = TRUE)) {
    recipe <- nirs4all_export_pipeline(forest, scope = "r_native")
    imported <- nirs4all_r_pipeline_from_recipe(recipe)
    outcome <- nirs4all_dag_cv_refit_predict(imported, X, y, folds = 3L,
      split_steps = TRUE, cli = cli)
    stopifnot(max(abs(nirs4all_dag_predict(outcome, X[1:4, , drop = FALSE]) -
      predict(nirs4all_fit(forest, X, y), X[1:4, , drop = FALSE]))) < 1e-12)
  }
}

if (have_torch) {
  regression <- nirs4all_pipeline(list(nirs4all_snv()),
    nirs4all_torch_mlp(hidden = 6L, epochs = 8L,
                       learning_rate = 0.01, seed = 5L))
  check_recipe(regression, X, y, 1e-5)
  classes <- factor(rep(c("a", "b", "c"), each = 7L))
  classifier <- nirs4all_pipeline(list(nirs4all_snv()),
    nirs4all_torch_mlp_classifier(hidden = 6L, epochs = 8L,
                                 learning_rate = 0.01, seed = 5L))
  check_recipe(classifier, X, classes, 1e-5)
}
