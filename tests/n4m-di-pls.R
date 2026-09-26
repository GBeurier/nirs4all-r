# DI-PLS uses an explicit target-domain spectrum set at fit time. Its native
# coefficients are a predictor on fresh source/target-domain spectra, while
# the target-domain spectra themselves are part of the fit recipe.
library(nirs4all)

source <- outer(seq_len(28L), seq_len(8L), function(i, j)
  sin(i * j / 10) + cos(i / 3 + j / 8) + i * j / 110)
colnames(source) <- paste0("wl", seq_len(ncol(source)))
target <- outer(seq_len(17L), seq_len(8L), function(i, j)
  0.85 * sin((i + 2) * j / 10) + cos(i / 3 + j / 8) +
  i * j / 110 + j / 70)
held <- source[c(3L, 11L, 23L), , drop = FALSE] + 0.047
y <- 1.2 + 0.65 * source[, 2L] - 0.3 * source[, 6L]
parameter <- list(X_target = target, di_lambda = 0.7)
pipeline <- nirs4all_pipeline(learner = nirs4all_n4m_method(
  "di_pls", 2L, parameter))
fitted <- nirs4all_fit(pipeline, source, y)
native <- n4m::n4m_method("di_pls", source, y, 2L, params = parameter)
expected <- as.numeric((sweep(held, 2L, as.numeric(native$x_mean))) %*%
  native$coefficients + as.numeric(native$y_mean))
stopifnot(max(abs(predict(fitted, held) - expected)) < 1e-10,
  max(abs(fitted$state$coefficients - native$coefficients)) < 1e-10,
  max(abs(as.numeric(native$predictions) -
    as.numeric(sweep(source, 2L, as.numeric(native$x_mean)) %*%
      native$coefficients + as.numeric(native$y_mean)))) < 1e-10)
# The target domain and DI penalty must actually affect the fitted model.
alternative_target <- outer(seq_len(17L), seq_len(8L), function(i, j)
  sin((i + 2) * j / 5) + cos(i / 2 + j / 3) + i * j / 80)
alternative <- nirs4all_fit(nirs4all_pipeline(learner = nirs4all_n4m_method(
  "di_pls", 2L, list(X_target = alternative_target, di_lambda = 0.7))),
  source, y)
zero_penalty <- nirs4all_fit(nirs4all_pipeline(learner = nirs4all_n4m_method(
  "di_pls", 2L, list(X_target = target, di_lambda = 0))), source, y)
stopifnot(max(abs(predict(alternative, held) - expected)) > 1e-3,
  max(abs(predict(zero_penalty, held) - expected)) > 1e-4)

bytes <- nirs4all_export_native_model(fitted)
restored <- nirs4all_import_native_model(bytes, pipeline, colnames(source))
stopifnot(max(abs(predict(restored, held) - expected)) < 1e-10)
# A full cross-language trained envelope cannot represent the unsupervised
# target cohort yet; the standalone N4MM carries predictions only.
stopifnot(inherits(try(nirs4all_export_trained_pipeline(fitted),
  silent = TRUE), "try-error"))
path <- tempfile(fileext = ".rds")
nirs4all_save(fitted, path)
stopifnot(max(abs(predict(nirs4all_load(path), held) - expected)) < 1e-10,
  max(abs(predict(nirs4all_retrain(fitted, source, y), held) - expected)) < 1e-10)
unlink(path)

cli <- Sys.getenv("NIRS4ALL_DAGML_CLI", "")
if (nzchar(cli) && requireNamespace("dagml", quietly = TRUE)) {
  graph <- nirs4all_dag_cv_refit_predict(pipeline, source, y,
    folds = 3L, cli = cli)
  stopifnot(identical(as.integer(graph$fit_cv_result_count), 3L),
    identical(as.integer(graph$refit_result_count), 1L),
    max(abs(nirs4all_dag_predict(graph, held) - expected)) < 1e-10)
  expected_oof <- numeric(nrow(source))
  for (fold in 0:2) {
    valid <- seq_len(nrow(source))[(seq_len(nrow(source)) - 1L) %% 3L == fold]
    train <- setdiff(seq_len(nrow(source)), valid)
    expected_oof[valid] <- predict(nirs4all_fit(pipeline,
      source[train, , drop = FALSE], y[train]), source[valid, , drop = FALSE])
  }
  for (average in graph$oof_average_results) {
    block <- average$aggregated_predictions[[1L]]
    block_ids <- vapply(block$unit_ids, `[[`, "", "id")
    block_values <- vapply(block$values,
      function(value) as.numeric(value[[1L]]), numeric(1))
    ids <- sprintf("sample:%08d", seq_len(nrow(source)))
    stopifnot(max(abs(block_values - expected_oof[match(block_ids, ids)])) <
      1e-10)
  }
}

python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON", "")
if (nzchar(python)) {
  helper <- if (file.exists("helpers/dipls_peer.py"))
    "helpers/dipls_peer.py" else "tests/helpers/dipls_peer.py"
  rows <- function(matrix) lapply(seq_len(nrow(matrix)), function(i)
    unname(as.numeric(matrix[i, ])))
  request <- tempfile(fileext = ".json")
  response <- tempfile(fileext = ".json")
  writeLines(as.character(jsonlite::toJSON(list(
    source = rows(source), target = rows(target), held = rows(held),
    y = unname(as.numeric(y)), n_components = 2L, di_lambda = 0.7),
    auto_unbox = TRUE, digits = 17L)), request)
  output <- suppressWarnings(system2(python,
    c(shQuote(helper), shQuote(request), shQuote(response)),
    stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L)
    stop("Python DI-PLS n4m peer failed: ", paste(output, collapse = "\n"))
  peer <- jsonlite::fromJSON(response)
  stopifnot(max(abs(peer$predictions - expected)) < 1e-10,
    max(abs(peer$coefficients - as.numeric(native$coefficients))) < 1e-10,
    max(abs(peer$x_mean - as.numeric(native$x_mean))) < 1e-10,
    max(abs(peer$y_mean - as.numeric(native$y_mean))) < 1e-10)
  unlink(c(request, response))
}

for (invalid in list(list(), list(X_target = as.data.frame(target)),
  list(X_target = target[, 1L, drop = FALSE]),
  list(X_target = matrix(NA_real_, 3L, 8L)),
  list(X_target = target, di_lambda = -1),
  list(X_target = target, di_lambda = Inf),
  list(X_target = target, bad = 1)))
  stopifnot(inherits(try(nirs4all_n4m_method("di_pls", 2L, invalid),
    silent = TRUE), "try-error"))
bad_width <- nirs4all_pipeline(learner = nirs4all_n4m_method(
  "di_pls", 2L, list(X_target = target[, -1L, drop = FALSE])))
stopifnot(inherits(try(nirs4all_fit(bad_width, source, y),
  silent = TRUE), "try-error"))
named_target <- target
colnames(named_target) <- rev(colnames(source))
bad_order <- nirs4all_pipeline(learner = nirs4all_n4m_method(
  "di_pls", 2L, list(X_target = named_target)))
stopifnot(inherits(try(nirs4all_fit(bad_order, source, y),
  silent = TRUE), "try-error"))
