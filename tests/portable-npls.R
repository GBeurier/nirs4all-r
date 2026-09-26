library(nirs4all)

X <- outer(seq_len(21L), seq_len(12L), function(s, b)
  sin(s * b / 9) + cos(s + b / 7) + s * b / 100)
colnames(X) <- paste0("wl", seq_len(ncol(X)))
y <- 1.3 + 0.7 * X[, 2L] - 0.4 * X[, 6L]
held <- X[c(2L, 8L, 17L), , drop = FALSE] + 0.031
expected <- c(1.242673188360166, 1.939312398512768,
              0.5941411153991755)
params <- list(mode_j = 3L, mode_k = 4L)
pipeline <- nirs4all_pipeline(learner = nirs4all_n4m_method("n_pls", 2L,
  params))
fitted <- nirs4all_fit(pipeline, X, y)
stopifnot(max(abs(predict(fitted, held) - expected)) < 1e-10,
  max(abs(as.numeric(n4m::n4m_predict(fitted$state$native_model, held)) -
    expected)) < 1e-10)
bytes <- nirs4all_export_native_model(fitted)
stopifnot(identical(n4m::n4m_model_descriptor(bytes)$algorithm, 11L),
  max(abs(predict(nirs4all_import_native_model(bytes, pipeline,
    colnames(X)), held) - expected)) < 1e-10)

# The native kernel also supports two targets. The high-level portable
# pipeline intentionally remains single-target until a separate wire contract.
y2 <- 0.3 - 0.2 * X[, 4L] + 0.5 * X[, 10L]
native <- n4m::n4m_method("n_pls", X, cbind(y, y2), 2L, params = params)
multi <- sweep(held, 2L, as.numeric(native$x_mean), "-") %*%
  native$coefficients
multi <- sweep(multi, 2L, as.numeric(native$y_mean), "+")
expected_multi <- matrix(c(1.227494353285351, 0.2894716259832100,
  2.011143179277550, 0.2706367639092839,
  0.5127342674702883, 1.178039833756177), ncol = 2L, byrow = TRUE)
stopifnot(identical(dim(native$coefficients), c(12L, 2L)),
  max(abs(multi - expected_multi)) < 1e-10)

for (format in c("json", "yaml")) {
  recipe <- nirs4all_export_pipeline(pipeline, format)
  stopifnot(identical(tail(nirs4all_portable_class_names(
    nirs4all_load_pipeline(recipe)), 1L), "n4m.NPLS"))
  parsed <- nirs4all_pipeline_from_portable(recipe)
  stopifnot(identical(parsed$learner$spec, pipeline$learner$spec),
    max(abs(predict(nirs4all_fit(parsed, X, y), held) - expected)) < 1e-10)
}

for (invalid in list(
  list(), list(mode_j = 3L), list(mode_j = 3L, mode_k = 0L),
  list(mode_j = 3.5, mode_k = 4L),
  list(mode_j = .Machine$integer.max + 1, mode_k = 4L),
  list(mode_j = 3L, mode_k = Inf),
  list(mode_j = 3L, mode_k = 4L, extra = 1L)))
  stopifnot(inherits(try(nirs4all_n4m_method("n_pls", 2L, invalid),
    silent = TRUE), "try-error"))
stopifnot(inherits(try(nirs4all_fit(nirs4all_pipeline(
  learner = nirs4all_n4m_method("n_pls", params = list(
    mode_j = 2L, mode_k = 5L))), X, y), silent = TRUE), "try-error"))

python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON")
python_root <- Sys.getenv("NIRS4ALL_PYTHON_FULL_ROOT")
if (nzchar(python) && nzchar(python_root)) {
  helper <- if (file.exists("helpers/portable_n4m_pipeline_peer.py"))
    "helpers/portable_n4m_pipeline_peer.py" else
    "tests/helpers/portable_n4m_pipeline_peer.py"
  stopifnot(file.exists(helper), file.exists(python), dir.exists(python_root))
  rows <- function(values) lapply(seq_len(nrow(values)), function(index)
    unname(as.numeric(values[index, ])))
  prior_path <- Sys.getenv("PYTHONPATH", unset = "")
  Sys.setenv(PYTHONPATH = paste(c(python_root, prior_path),
    collapse = .Platform$path.sep))
  tryCatch({
    for (format in c("json", "yaml")) {
      exported <- nirs4all_export_pipeline(pipeline, format)
      recipe <- if (identical(format, "json"))
        jsonlite::fromJSON(exported, simplifyVector = FALSE) else
        yaml::yaml.load(exported)
      request <- tempfile(fileext = ".json")
      response <- tempfile(fileext = ".json")
      writeLines(as.character(jsonlite::toJSON(list(
        recipe = recipe, train = rows(X), validation = rows(held),
        y = unname(as.numeric(y))), auto_unbox = TRUE, digits = 17)), request)
      output <- suppressWarnings(system2(python,
        c(shQuote(helper), shQuote(request), shQuote(response)),
        stdout = TRUE, stderr = TRUE))
      status <- attr(output, "status")
      if (!is.null(status) && status != 0L)
        stop("Python NPLS recipe peer failed: ", paste(output,
          collapse = "\n"))
      stopifnot(max(abs(as.numeric(jsonlite::fromJSON(response)$predictions) -
        expected)) < 1e-10)
      unlink(c(request, response))
    }
  }, finally = {
    if (nzchar(prior_path)) Sys.setenv(PYTHONPATH = prior_path)
    else Sys.unsetenv("PYTHONPATH")
  })
}

cli <- Sys.getenv("NIRS4ALL_DAGML_CLI")
if (nzchar(cli) && file.exists(cli) && requireNamespace("dagml", quietly = TRUE)) {
  outcome <- nirs4all_dag_cv_refit_predict(pipeline, X, y,
    folds = 3L, cli = cli)
  stopifnot(max(abs(nirs4all_dag_predict(outcome, held) - expected)) < 1e-10)
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
