# Frozen independent Python pls4all.mb_pls_fit held-out oracle, libn4m 2.6.0.
# Python config: PLS_REGRESSION, NIPALS, REGRESSION deflation, center X/Y,
# scale_x=scale_y=FALSE. The sklearn MBPLSRegression wrapper defaults to
# scale_x=scale_y=TRUE and is deliberately not the reference for this alias.
library(nirs4all)

X <- outer(seq_len(21L), seq_len(12L), function(s, b)
  sin(s * b / 9) + cos(s + b / 7) + s * b / 100)
colnames(X) <- paste0("wl", seq_len(ncol(X)))
y <- 1.3 + 0.7 * X[, 2L] - 0.4 * X[, 6L]
held <- X[c(2L, 8L, 17L), , drop = FALSE] + 0.031
expected <- c(1.3614391588922699, 2.033212108151359,
              0.7661914180346159)
blocks <- c(4L, 4L, 4L)
pipeline <- nirs4all_pipeline(learner = nirs4all_n4m_method(
  "mb_pls", 2L, list(block_sizes = blocks)))
fitted <- nirs4all_fit(pipeline, X, y)
native <- n4m::n4m_method("mb_pls", X, y, 2L,
  params = list(block_sizes = blocks))
direct <- as.numeric(held %*% native$coefficients) +
  as.numeric(native$intercept)
stopifnot(is.null(fitted$state$x_mean), is.null(fitted$state$y_mean),
  identical(fitted$state$intercept, as.numeric(native$intercept)),
  max(abs(direct - expected)) < 1e-10,
  max(abs(predict(fitted, held) - direct)) < 1e-10,
  max(abs(as.numeric(n4m::n4m_predict(
    fitted$state$native_model, held)) - direct)) < 1e-10,
  max(abs(as.numeric(X %*% native$coefficients) +
    as.numeric(native$intercept) - as.numeric(native$predictions))) < 1e-10)

python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON")
if (nzchar(python)) {
  helper <- if (file.exists("helpers/mbpls_peer.py"))
    "helpers/mbpls_peer.py" else "tests/helpers/mbpls_peer.py"
  request <- tempfile(fileext = ".json")
  response <- tempfile(fileext = ".json")
  rows <- function(value) lapply(seq_len(nrow(value)), function(index)
    unname(as.numeric(value[index, ])))
  writeLines(as.character(jsonlite::toJSON(list(
    train = rows(X), validation = rows(held), y = unname(as.numeric(y)),
    block_sizes = unname(as.integer(blocks)), n_components = 2L),
    auto_unbox = TRUE, digits = 17L)), request)
  output <- suppressWarnings(system2(python,
    c(shQuote(helper), shQuote(request), shQuote(response)),
    stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L)
    stop("Python MB-PLS C-ABI peer failed: ", paste(output, collapse = "\n"))
  peer <- jsonlite::fromJSON(response)
  stopifnot(max(abs(peer$predictions - expected)) < 1e-10,
    abs(peer$intercept - fitted$state$intercept) < 1e-10)
  unlink(c(request, response))
}

for (format in c("json", "yaml")) {
  recipe <- nirs4all_export_pipeline(pipeline, format)
  stopifnot(identical(tail(nirs4all_portable_class_names(
    nirs4all_load_pipeline(recipe)), 1L), "n4m.MBPLS"))
  parsed <- nirs4all_pipeline_from_portable(recipe)
  stopifnot(identical(parsed$learner$spec, pipeline$learner$spec),
    max(abs(predict(nirs4all_fit(parsed, X, y), held) - expected)) < 1e-10)
  result <- nirs4all_run_portable_pipeline(recipe, list(X = X, y = y))
  stopifnot(length(result$variants) == 1L)
}

bundle <- nirs4all_export_trained_pipeline(fitted)
restored <- nirs4all_import_trained_pipeline(bundle)
document <- jsonlite::fromJSON(bundle, simplifyVector = FALSE)
manifest <- jsonlite::fromJSON(document$manifest_json, simplifyVector = FALSE)
stopifnot(identical(manifest$fit_recipe_assertion$block_sizes,
  unname(as.list(blocks))))
altered <- manifest
altered$fit_recipe_assertion$block_sizes <- as.list(c(3L, 5L, 4L))
document$manifest_json <- as.character(jsonlite::toJSON(altered,
  auto_unbox = TRUE, null = "null", digits = 17L))
document$manifest_sha256 <- digest::digest(document$manifest_json,
  algo = "sha256", serialize = FALSE)
stopifnot(inherits(try(nirs4all_import_trained_pipeline(
  as.character(jsonlite::toJSON(document, auto_unbox = TRUE,
    null = "null", digits = 17L))), silent = TRUE), "try-error"))
stopifnot(max(abs(predict(restored, held) - expected)) < 1e-10,
  max(abs(predict(nirs4all_retrain(restored, X, y), held) - expected)) < 1e-10)
bytes <- nirs4all_export_native_model(fitted)
stopifnot(identical(n4m::n4m_model_descriptor(bytes)$algorithm, 11L),
  max(abs(predict(nirs4all_import_native_model(bytes, pipeline,
    colnames(X)), held) - expected)) < 1e-10)

python_root <- Sys.getenv("NIRS4ALL_PYTHON_FULL_ROOT")
if (nzchar(python) && nzchar(python_root)) {
  helper <- if (file.exists("helpers/trained_n4m_peer.py"))
    "helpers/trained_n4m_peer.py" else "tests/helpers/trained_n4m_peer.py"
  request <- tempfile(fileext = ".json")
  response <- tempfile(fileext = ".json")
  r_bundle <- tempfile(fileext = ".json")
  python_bundle <- tempfile(fileext = ".json")
  writeLines(bundle, r_bundle)
  writeLines(as.character(jsonlite::toJSON(list(
    python_root = python_root, bundle = r_bundle,
    python_bundle = python_bundle, train = rows(X),
    validation = rows(held), y = unname(as.numeric(y)),
    feature_names = unname(as.list(colnames(X)))),
    auto_unbox = TRUE, digits = 17L)), request)
  output <- suppressWarnings(system2(python,
    c(shQuote(helper), shQuote(request), shQuote(response)),
    stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L)
    stop("Python MB-PLS trained peer failed: ", paste(output, collapse = "\n"))
  peer <- jsonlite::fromJSON(response)
  stopifnot(max(abs(peer$predictions - expected)) < 1e-10,
    max(abs(peer$retrained_predictions - expected)) < 1e-8,
    max(abs(peer$python_predictions - expected)) < 1e-8,
    isTRUE(peer$rejected_reordered), isTRUE(peer$rejected_unnamed))
  imported <- nirs4all_import_trained_pipeline(python_bundle)
  stopifnot(max(abs(predict(imported, held) - expected)) < 1e-8,
    max(abs(predict(nirs4all_retrain(imported, X, y), held) - expected)) < 1e-8)
  unlink(c(request, response, r_bundle, python_bundle))
}

workspace <- tempfile("nirs4all-mbpls-")
dir.create(workspace)
held_path <- file.path(workspace, "held.rds")
model_path <- file.path(workspace, "saved.rds")
bundle_path <- file.path(workspace, "trained.json")
saveRDS(held, held_path)
nirs4all_save(fitted, model_path)
writeLines(bundle, bundle_path)
helper <- if (file.exists("helpers/mbpls_replay.R"))
  "helpers/mbpls_replay.R" else "tests/helpers/mbpls_replay.R"
output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
  c(shQuote(helper), shQuote(held_path), shQuote(model_path),
    shQuote(bundle_path)), stdout = TRUE, stderr = TRUE))
status <- attr(output, "status")
if (!is.null(status) && status != 0L)
  stop("MB-PLS fresh-process replay failed: ", paste(output, collapse = "\n"))
replay <- jsonlite::fromJSON(paste(output, collapse = "\n"))
stopifnot(max(abs(replay$saved - expected)) < 1e-10,
  max(abs(replay$trained - expected)) < 1e-10)
unlink(workspace, recursive = TRUE)

for (invalid in list(list(), list(block_sizes = 12L),
  list(block_sizes = c(4L, 0L, 8L)),
  list(block_sizes = c(4.5, 3.5, 4)),
  list(block_sizes = c(4L, NA_integer_, 4L)),
  list(block_sizes = c(4L, Inf, 4L)),
  list(block_sizes = c(4L, 4L, .Machine$integer.max)),
  list(block_sizes = c(4L, 4L, 4L), extra = 1L)))
  stopifnot(inherits(try(nirs4all_n4m_method("mb_pls", 2L, invalid),
    silent = TRUE), "try-error"))
stopifnot(inherits(try(nirs4all_fit(nirs4all_pipeline(
  learner = nirs4all_n4m_method("mb_pls", params = list(
    block_sizes = c(4L, 4L, 3L)))), X, y), silent = TRUE), "try-error"))

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
