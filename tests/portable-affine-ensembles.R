library(nirs4all)

# Held-out predictions from the independent Python n4m/Methods oracle.
# FusedSparsePLS is qualified against the corrected post-SIMPLS coefficient
# penalty: its previous value was the l1_lambda = 0 prediction, because the
# old native implementation thresholded discarded weights without changing
# predictive coefficients. See n4m-fused-sparse-penalty.R for both oracles.
X <- outer(seq_len(21L), seq_len(12L), function(s, b)
  sin(s * b / 9) + cos(s + b / 7) + s * b / 100)
colnames(X) <- paste0("wl", seq_len(ncol(X)))
y <- 1.3 + 0.7 * X[, 2L] - 0.4 * X[, 6L]
held <- X[c(2L, 8L, 17L), , drop = FALSE] + 0.031
cases <- list(
  fused_sparse_pls = list(params = list(), class = "n4m.FusedSparsePLS",
    expected = c(1.3373539467794613, 1.9377148952044154,
                 0.7544322586932719)),
  bagging_pls = list(params = list(n_estimators = 7L, seed = 13L),
    class = "n4m.BaggingPLS",
    expected = c(1.340366231116839, 2.072160066869946, 0.8905637569136029)),
  boosting_pls = list(params = list(n_estimators = 7L, learning_rate = 0.3),
    class = "n4m.BoostingPLS",
    expected = c(1.190830194241934, 2.206788158315018, 0.9303362422665358)),
  random_subspace_pls = list(params = list(n_estimators = 7L,
    features_per_subspace = 5L, seed = 13L),
    class = "n4m.RandomSubspacePLS",
    expected = c(1.393934040298960, 1.847198984652901, 0.8903822546307825)))

for (method in names(cases)) {
  case <- cases[[method]]
  pipeline <- nirs4all_pipeline(learner = nirs4all_n4m_method(
    method, 2L, case$params))
  fitted <- nirs4all_fit(pipeline, X, y)
  expected <- case$expected
  stopifnot(max(abs(predict(fitted, held) - expected)) < 1e-10,
    max(abs(as.numeric(n4m::n4m_predict(fitted$state$native_model,
      held)) - expected)) < 1e-10)
  bytes <- nirs4all_export_native_model(fitted)
  descriptor <- n4m::n4m_model_descriptor(bytes)
  stopifnot(identical(descriptor$algorithm, 11L),
    identical(descriptor$n_features, as.integer(ncol(X))),
    identical(descriptor$n_targets, 1L))
  imported <- nirs4all_import_native_model(bytes, pipeline, colnames(X))
  stopifnot(max(abs(predict(imported, held) - expected)) < 1e-10)
  file <- tempfile(fileext = ".rds")
  nirs4all_save(fitted, file)
  stopifnot(max(abs(predict(nirs4all_load(file), held) - expected)) < 1e-10)
  unlink(file)
  for (format in c("json", "yaml")) {
    recipe <- nirs4all_export_pipeline(pipeline, format)
    stopifnot(identical(tail(nirs4all_portable_class_names(
      nirs4all_load_pipeline(recipe)), 1L), case$class))
    parsed <- nirs4all_pipeline_from_portable(recipe)
    stopifnot(identical(parsed$learner$spec, pipeline$learner$spec),
      max(abs(predict(nirs4all_fit(parsed, X, y), held) - expected)) < 1e-10)
  }
}

# Native defaults remain the dispatcher defaults when optional fields are
# omitted from the recipe, including deterministic seed zero.
for (method in names(cases)) {
  pipeline <- nirs4all_pipeline(learner = nirs4all_n4m_method(method))
  recipe <- nirs4all_export_pipeline(pipeline)
  parsed <- nirs4all_pipeline_from_portable(recipe)
  stopifnot(identical(parsed$learner$spec, pipeline$learner$spec),
    max(abs(predict(nirs4all_fit(parsed, X, y), held) -
      predict(nirs4all_fit(pipeline, X, y), held))) < 1e-10)
}

bad <- list(
  list("fused_sparse_pls", list(l1_lambda = -0.1)),
  list("fused_sparse_pls", list(fusion_lambda = Inf)),
  list("bagging_pls", list(n_estimators = 0L)),
  list("bagging_pls", list(seed = -1L)),
  list("bagging_pls", list(seed = .Machine$integer.max + 1)),
  list("boosting_pls", list(learning_rate = 0)),
  list("boosting_pls", list(learning_rate = 1.2)),
  list("boosting_pls", list(learning_rate = NaN)),
  list("random_subspace_pls", list(features_per_subspace = 1.5)),
  list("random_subspace_pls", list(features_per_subspace = 0L)),
  list("random_subspace_pls", list(unknown = 1L)))
for (item in bad)
  stopifnot(inherits(try(nirs4all_n4m_method(item[[1L]], 2L,
    item[[2L]]), silent = TRUE), "try-error"))
stopifnot(inherits(try(nirs4all_fit(nirs4all_pipeline(
  learner = nirs4all_n4m_method("random_subspace_pls")),
  X[, 1:8, drop = FALSE], y), silent = TRUE), "try-error"),
  inherits(try(nirs4all_fit(nirs4all_pipeline(
    learner = nirs4all_n4m_method("random_subspace_pls", params = list(
      features_per_subspace = 13L))), X, y), silent = TRUE), "try-error"))

python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON")
python_root <- Sys.getenv("NIRS4ALL_PYTHON_FULL_ROOT")
if (nzchar(python) && nzchar(python_root)) {
  recipe_helper <- if (file.exists("helpers/portable_n4m_pipeline_peer.py"))
    "helpers/portable_n4m_pipeline_peer.py" else
    "tests/helpers/portable_n4m_pipeline_peer.py"
  native_helper <- if (file.exists("helpers/native_n4mm_peer.py"))
    "helpers/native_n4mm_peer.py" else "tests/helpers/native_n4mm_peer.py"
  stopifnot(file.exists(recipe_helper), file.exists(native_helper),
    file.exists(python), dir.exists(python_root))
  rows <- function(values) lapply(seq_len(nrow(values)), function(index)
    unname(as.numeric(values[index, ])))
  work <- tempfile("nirs4all-ensemble-peer-")
  dir.create(work)
  request <- file.path(work, "request.json")
  response <- file.path(work, "response.json")
  model_file <- file.path(work, "model.n4mm")
  prior_path <- Sys.getenv("PYTHONPATH", unset = "")
  Sys.setenv(PYTHONPATH = paste(c(python_root, prior_path),
    collapse = .Platform$path.sep))
  run_peer <- function(helper, arguments) {
    output <- suppressWarnings(system2(python,
      c(shQuote(helper), arguments), stdout = TRUE, stderr = TRUE))
    status <- attr(output, "status")
    if (!is.null(status) && status != 0L)
      stop("Python affine ensemble peer failed: ",
           paste(output, collapse = "\n"))
  }
  tryCatch({
    for (method in names(cases)) {
      case <- cases[[method]]
      pipeline <- nirs4all_pipeline(learner = nirs4all_n4m_method(
        method, 2L, case$params))
      fitted <- nirs4all_fit(pipeline, X, y)
      for (format in c("json", "yaml")) {
        exported <- nirs4all_export_pipeline(pipeline, format)
        recipe <- if (identical(format, "json"))
          jsonlite::fromJSON(exported, simplifyVector = FALSE) else
          yaml::yaml.load(exported)
        writeLines(as.character(jsonlite::toJSON(list(
          recipe = recipe, train = rows(X), validation = rows(held),
          y = unname(as.numeric(y))), auto_unbox = TRUE, digits = 17)),
          request)
        run_peer(recipe_helper, c(shQuote(request), shQuote(response)))
        actual <- as.numeric(jsonlite::fromJSON(response)$predictions)
        stopifnot(length(actual) == length(case$expected),
          max(abs(actual - case$expected)) < 1e-10)
      }
      writeLines(as.character(jsonlite::toJSON(list(
        X = rows(X), predict_X = rows(held),
        coefficients = lapply(seq_len(ncol(X)), function(index)
          as.numeric(fitted$state$coefficients[index, 1L])),
        intercept = unname(as.numeric(fitted$state$y_mean -
          drop(fitted$state$x_mean %*% fitted$state$coefficients)))),
        auto_unbox = FALSE, digits = NA)), request)
      writeBin(nirs4all_export_native_model(fitted), model_file)
      run_peer(native_helper, c("predict_affine", shQuote(model_file),
        shQuote(request), shQuote(response)))
      stopifnot(max(abs(as.numeric(jsonlite::fromJSON(response)$predictions) -
        case$expected)) < 1e-10)
      run_peer(native_helper, c("fit_affine", shQuote(model_file),
        shQuote(request), shQuote(response)))
      bytes <- readBin(model_file, "raw", n = file.info(model_file)$size)
      imported <- nirs4all_import_native_model(bytes, pipeline, colnames(X))
      stopifnot(max(abs(predict(imported, held) - case$expected)) < 1e-10)
    }
  }, finally = {
    if (nzchar(prior_path)) Sys.setenv(PYTHONPATH = prior_path)
    else Sys.unsetenv("PYTHONPATH")
    unlink(work, recursive = TRUE)
  })
}

cli <- Sys.getenv("NIRS4ALL_DAGML_CLI")
if (nzchar(cli) && file.exists(cli) && requireNamespace("dagml", quietly = TRUE)) {
  for (method in names(cases)) {
    pipeline <- nirs4all_pipeline(learner = nirs4all_n4m_method(
      method, 2L, cases[[method]]$params))
    outcome <- nirs4all_dag_cv_refit_predict(pipeline, X, y,
      folds = 3L, cli = cli)
    stopifnot(max(abs(nirs4all_dag_predict(outcome, held) -
      predict(nirs4all_fit(pipeline, X, y), held))) < 1e-10)
    expected_oof <- numeric(nrow(X))
    for (fold in 0:2) {
      validation <- seq_len(nrow(X))[(seq_len(nrow(X)) - 1L) %% 3L == fold]
      training <- setdiff(seq_len(nrow(X)), validation)
      expected_oof[validation] <- predict(nirs4all_fit(pipeline,
        X[training, , drop = FALSE], y[training]),
        X[validation, , drop = FALSE])
    }
    ids <- sprintf("sample:%08d", seq_len(nrow(X)))
    for (average in outcome$oof_average_results) {
      block <- average$aggregated_predictions[[1L]]
      block_ids <- vapply(block$unit_ids, `[[`, "", "id")
      values <- vapply(block$values,
        function(value) as.numeric(value[[1L]]), numeric(1))
      stopifnot(length(values) == nrow(X),
        max(abs(values - expected_oof[match(block_ids, ids)])) < 1e-10)
    }
  }
}
