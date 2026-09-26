# GroupSparsePLS in n4m shrinks predictive coefficients *after* SIMPLS.
# It is not a reproduction of sgPLS::gPLS latent-direction optimization.
library(nirs4all)

train <- outer(seq_len(28L), seq_len(8L), function(i, j)
  sin(i * j / 10) + cos(i / 3 + j / 8) + i * j / 110)
colnames(train) <- paste0("wl", seq_len(ncol(train)))
held <- train[c(3L, 11L, 23L), , drop = FALSE] + 0.047
y <- 1.2 + 0.65 * train[, 2L] - 0.3 * train[, 6L]
groups <- stats::setNames(rep(c(2L, 9L), each = 4L), colnames(train))
learner <- nirs4all_group_sparse_pls(2L, groups, 0.2)
pipeline <- nirs4all_pipeline(learner = learner)
fitted <- nirs4all_fit(pipeline, train, y)
native <- n4m::n4m_method("group_sparse_pls", train, y, 2L,
  params = list(group_assignment = unname(groups), group_lambda = 0.2))
expected <- as.numeric(sweep(held, 2L, as.numeric(native$x_mean)) %*%
  native$coefficients + as.numeric(native$y_mean))
stopifnot(max(abs(predict(fitted, held) - expected)) < 1e-10,
  max(abs(fitted$state$coefficients - native$coefficients)) < 1e-10,
  identical(as.integer(native$n_groups), 2L))

# The coefficient penalty must affect held-out prediction, and a very large
# penalty must zero every group while preserving the response mean.
unpenalized <- nirs4all_fit(nirs4all_pipeline(learner =
  nirs4all_group_sparse_pls(2L, groups, 0)), train, y)
zeroed <- nirs4all_fit(nirs4all_pipeline(learner =
  nirs4all_group_sparse_pls(2L, groups, 1e6)), train, y)
stopifnot(max(abs(predict(unpenalized, held) - expected)) > 1e-5,
  identical(as.numeric(zeroed$state$coefficients), rep(0, ncol(train))),
  max(abs(predict(zeroed, held) - mean(y))) < 1e-10)

path <- tempfile(fileext = ".rds")
nirs4all_save(fitted, path)
stopifnot(max(abs(predict(nirs4all_load(path), held) - expected)) < 1e-10,
  max(abs(predict(nirs4all_retrain(fitted, train, y), held) - expected)) < 1e-10)
unlink(path)
bytes <- nirs4all_export_native_model(fitted)
restored <- nirs4all_import_native_model(bytes, pipeline, colnames(train))
stopifnot(max(abs(predict(restored, held) - expected)) < 1e-10)

reordered <- held[, rev(seq_len(ncol(held))), drop = FALSE]
stopifnot(inherits(try(predict(fitted, reordered), silent = TRUE), "try-error"),
  inherits(try(nirs4all_fit(nirs4all_pipeline(learner =
    nirs4all_group_sparse_pls(2L, rev(groups), 0.2)), train, y),
    silent = TRUE), "try-error"))
for (invalid in list(integer(), c(-1L, rep(0L, 7L)),
  c(NA_integer_, rep(0L, 7L)), c(0.5, rep(0, 7L)),
  stats::setNames(rep(0L, 8L), rep("x", 8L))))
  stopifnot(inherits(try(nirs4all_group_sparse_pls(2L, invalid),
    silent = TRUE), "try-error"))
for (invalid in c(-1, NA_real_, Inf))
  stopifnot(inherits(try(nirs4all_group_sparse_pls(2L, groups, invalid),
    silent = TRUE), "try-error"))
stopifnot(inherits(try(nirs4all_fit(nirs4all_pipeline(learner =
  nirs4all_group_sparse_pls(2L, groups[-1L])), train, y),
  silent = TRUE), "try-error"))

# Group assignment is defined on the learner's transformed feature axis.
# SNV preserves width/order but its native matrix omits column labels, so
# positional IDs are required for this preprocessing recipe.
preprocessed <- nirs4all_pipeline(list(nirs4all_snv()),
  nirs4all_group_sparse_pls(2L, unname(groups), 0.2))
transformed <- n4m::snv_transform(train)
native_preprocessed <- n4m::n4m_method("group_sparse_pls", transformed, y,
  2L, params = list(group_assignment = unname(groups), group_lambda = 0.2))
expected_preprocessed <- as.numeric(
  sweep(n4m::snv_transform(held), 2L,
    as.numeric(native_preprocessed$x_mean)) %*%
    native_preprocessed$coefficients +
    as.numeric(native_preprocessed$y_mean))
fitted_preprocessed <- nirs4all_fit(preprocessed, train, y)
stopifnot(max(abs(predict(fitted_preprocessed, held) -
    expected_preprocessed)) < 1e-10,
  inherits(try(nirs4all_export_native_model(fitted_preprocessed),
    silent = TRUE), "try-error"))

cli <- Sys.getenv("NIRS4ALL_DAGML_CLI", "")
if (nzchar(cli) && requireNamespace("dagml", quietly = TRUE)) {
  graph <- nirs4all_dag_cv_refit_predict(pipeline, train, y,
    folds = 3L, cli = cli)
  stopifnot(identical(as.integer(graph$fit_cv_result_count), 3L),
    identical(as.integer(graph$refit_result_count), 1L),
    max(abs(nirs4all_dag_predict(graph, held) - expected)) < 1e-10)
  expected_oof <- numeric(nrow(train))
  for (fold in 0:2) {
    validation <- seq_len(nrow(train))[(seq_len(nrow(train)) - 1L) %% 3L == fold]
    training <- setdiff(seq_len(nrow(train)), validation)
    expected_oof[validation] <- predict(nirs4all_fit(pipeline,
      train[training, , drop = FALSE], y[training]),
      train[validation, , drop = FALSE])
  }
  for (average in graph$oof_average_results) {
    block <- average$aggregated_predictions[[1L]]
    ids <- vapply(block$unit_ids, `[[`, "", "id")
    values <- vapply(block$values,
      function(value) as.numeric(value[[1L]]), numeric(1))
    expected_ids <- sprintf("sample:%08d", seq_len(nrow(train)))
    stopifnot(max(abs(values - expected_oof[match(ids, expected_ids)])) < 1e-10)
  }
}

python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON", "")
if (nzchar(python)) {
  helper <- if (file.exists("helpers/groupsparse_peer.py"))
    "helpers/groupsparse_peer.py" else "tests/helpers/groupsparse_peer.py"
  rows <- function(X) lapply(seq_len(nrow(X)), function(i)
    unname(as.numeric(X[i, ])))
  request <- tempfile(fileext = ".json")
  response <- tempfile(fileext = ".json")
  writeLines(as.character(jsonlite::toJSON(list(
    train = rows(train), held = rows(held), y = unname(as.numeric(y)),
    group_assignment = unname(as.integer(groups)),
    n_components = 2L, group_lambda = 0.2),
    auto_unbox = TRUE, digits = 17L)), request)
  output <- suppressWarnings(system2(python,
    c(shQuote(helper), shQuote(request), shQuote(response)),
    stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L)
    stop("Python GroupSparsePLS n4m peer failed: ",
      paste(output, collapse = "\n"))
  peer <- jsonlite::fromJSON(response)
  stopifnot(max(abs(peer$predictions - expected)) < 1e-10,
    max(abs(peer$coefficients - as.numeric(native$coefficients))) < 1e-10,
    max(abs(peer$x_mean - as.numeric(native$x_mean))) < 1e-10,
    max(abs(peer$y_mean - as.numeric(native$y_mean))) < 1e-10)
  unlink(c(request, response))
}
