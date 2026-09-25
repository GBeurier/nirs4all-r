library(nirs4all)

rows <- as.vector(rbind(1:12, 51:62, 101:112))
X <- as.matrix(iris[rows, 1:4])
y <- iris$Species[rows]
validation <- as.vector(rbind(13:15, 63:65, 113:115))
new_X <- as.matrix(iris[validation, 1:4])
pipeline <- nirs4all_pipeline(list(nirs4all_snv()),
                             nirs4all_sparse_pls_da(2L, 0.05))
for (format in c("json", "yaml")) {
  recipe <- nirs4all_export_pipeline(pipeline, format)
  stopifnot(identical(nirs4all_parse_execution_plan(recipe)$learner,
                      pipeline$learner$spec))
  roundtrip <- nirs4all_pipeline_from_portable(recipe)
  stopifnot(identical(roundtrip$learner$spec, pipeline$learner$spec),
            identical(predict(nirs4all_fit(roundtrip, X, y), new_X),
                      predict(nirs4all_fit(pipeline, X, y), new_X)))
}
portable <- list(name = "native_sparse_pls_da", pipeline = list(
  list(class = "n4m.SNV"),
  list(model = list(class = "n4m.SparsePLSDA",
                    params = list(sparsity_lambda = 0.05)),
       `_range_` = list(1L, 2L, 1L), param = "n_components")))
for (source in list(portable,
                    as.character(jsonlite::toJSON(portable, auto_unbox = TRUE)),
                    yaml::as.yaml(portable))) {
  for (with_holdout in c(FALSE, TRUE)) {
    recipe <- nirs4all_load_pipeline(source)
    if (with_holdout)
      recipe$pipeline <- c(list(list(class = "n4m.KennardStone",
        params = list(test_size = 0.25))), recipe$pipeline)
    result <- nirs4all_run_portable_pipeline(recipe, list(X = X, y = y))
    stopifnot(length(result$variants) == 2L,
              identical(result$targets, as.character(y[result$split$testIndices + 1L])),
              identical(result$selected$accuracy,
                        max(vapply(result$variants, `[[`, numeric(1), "accuracy"))))
    for (index in seq_along(result$variants)) {
      candidate <- nirs4all_pipeline(list(nirs4all_snv()),
        nirs4all_sparse_pls_da(index, 0.05))
      train <- result$split$trainIndices + 1L
      held <- result$split$testIndices + 1L
      reference <- as.character(predict(nirs4all_fit(candidate,
        X[train, , drop = FALSE], y[train]), X[held, , drop = FALSE]))
      stopifnot(identical(result$variants[[index]]$predictions, reference),
                identical(result$variants[[index]]$accuracy,
                          mean(reference == as.character(y[held]))))
    }
  }
}
numeric_classes <- as.integer(y) - 1L
numeric_result <- nirs4all_run_portable_pipeline(portable,
  list(X = X, y = numeric_classes))
stopifnot(is.integer(numeric_result$targets),
          identical(numeric_result$targets, numeric_classes),
          all(vapply(numeric_result$variants, function(variant)
            is.integer(variant$predictions) &&
              all(variant$predictions %in% unique(numeric_classes)), logical(1))))
for (index in seq_along(numeric_result$variants)) {
  manual <- nirs4all_fit(nirs4all_pipeline(list(nirs4all_snv()),
    nirs4all_sparse_pls_da(index, 0.05)), X,
    factor(numeric_classes, levels = 0:2))
  stopifnot(identical(numeric_result$variants[[index]]$predictions,
                      as.integer(predict(manual, X)) - 1L))
}
stopifnot(inherits(try(nirs4all_run_portable_pipeline(portable,
    list(X = X, y = y[-1L])), silent = TRUE), "try-error"))
if (requireNamespace("nirs4allformats", quietly = TRUE)) {
  source_path <- system.file("extdata", "formats_classification.csv",
                             package = "nirs4all", mustWork = TRUE)
  formats_data <- nirs4all_from_formats(source_path, target = "species")
  one_model <- list(pipeline = list(list(class = "n4m.SNV"),
    list(model = list(class = "n4m.SparsePLSDA",
                      params = list(n_components = 2L)))))
  formats_result <- nirs4all_run_portable_pipeline(one_model, formats_data)
  stopifnot(identical(formats_result$targets, unname(formats_data$y)),
            length(formats_result$selected$predictions) == nrow(formats_data$X),
            is.finite(formats_result$selected$accuracy))
}
fitted <- nirs4all_fit(pipeline, X, y)
predictions <- predict(fitted, new_X)
probabilities <- nirs4all_predict_proba(fitted, new_X)
native_scores <- n4m::n4m_predict(fitted$state$native_model,
  n4m::snv_transform(new_X))
manual_scores <- n4m::snv_transform(new_X)
manual_scores <- sweep(manual_scores, 2L, fitted$state$x_mean, "-") %*%
  fitted$state$coefficients
manual_scores <- sweep(manual_scores, 2L, fitted$state$y_mean, "+")
stopifnot(max(abs(native_scores - manual_scores)) < 1e-10)
stopifnot(is.factor(predictions), identical(levels(predictions), levels(y)),
          identical(dim(probabilities), c(nrow(new_X), nlevels(y))),
          identical(colnames(probabilities), levels(y)),
          max(abs(rowSums(probabilities) - 1)) < 1e-12,
          all(max.col(probabilities, ties.method = "first") == as.integer(predictions)))
bundle <- tempfile(fileext = ".rds")
nirs4all_save(fitted, bundle)
reloaded <- nirs4all_load(bundle)
stopifnot(identical(predict(reloaded, new_X), predictions),
          identical(nirs4all_predict_proba(reloaded, new_X), probabilities),
          identical(n4m::n4m_model_export(reloaded$state$native_model),
                    n4m::n4m_model_export(fitted$state$native_model)))
tampered <- readRDS(bundle)
tampered$fitted$state$classes <- rev(tampered$fitted$state$classes)
saveRDS(tampered, bundle)
stopifnot(inherits(try(nirs4all_load(bundle), silent = TRUE), "try-error"))
unlink(bundle)
stopifnot(inherits(try(nirs4all_sparse_pls_da(0L), silent = TRUE), "try-error"),
          inherits(try(nirs4all_sparse_pls_da(sparsity_lambda = -1),
                       silent = TRUE), "try-error"),
          inherits(try(nirs4all_fit(nirs4all_pipeline(
            learner = nirs4all_sparse_pls_da(5L)), X, y),
            silent = TRUE), "try-error"))

# Compare unseen-sample decisions and scores to the independent Python n4m
# binding. DAG tests below additionally establish fold-local model isolation.
python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON", "")
if (nzchar(python)) {
  helper <- if (file.exists("helpers/native_classifier_peer.py"))
    "helpers/native_classifier_peer.py" else
    "tests/helpers/native_classifier_peer.py"
  stopifnot(file.exists(python), file.exists(helper))
  plain <- nirs4all_fit(nirs4all_pipeline(
    learner = nirs4all_sparse_pls_da(2L, 0.05)), X, y)
  work <- tempfile("nirs4all-classifier-peer-")
  dir.create(work)
  request <- file.path(work, "request.json")
  response <- file.path(work, "response.json")
  matrix_rows <- function(value) lapply(seq_len(nrow(value)), function(i)
    unname(as.numeric(value[i, ])))
  writeLines(as.character(jsonlite::toJSON(list(
    train = matrix_rows(X), target = as.integer(y) - 1L,
    test = matrix_rows(new_X), n_components = 2L,
    sparsity_lambda = 0.05), auto_unbox = TRUE, digits = NA)), request)
  output <- suppressWarnings(system2(python,
    c(shQuote(helper), shQuote(request), shQuote(response)),
    stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L)
    stop("Python n4m sparse PLS-DA oracle failed: ", paste(output, collapse = "\n"))
  oracle <- jsonlite::fromJSON(response)
  scores <- sweep(new_X, 2L, plain$state$x_mean, "-") %*%
    plain$state$coefficients
  scores <- sweep(scores, 2L, plain$state$y_mean, "+")
  stopifnot(identical(oracle$classes, 0:2),
            max(abs(scores - oracle$scores)) < 1e-10,
            identical(as.integer(predict(plain, new_X)) - 1L,
                      as.integer(oracle$predictions)))
  writeLines(as.character(jsonlite::toJSON(list(
    train = matrix_rows(X), target = numeric_classes, test = matrix_rows(X),
    n_components = 2L, sparsity_lambda = 0.05,
    preprocessing = list("n4m.SNV")), auto_unbox = TRUE, digits = NA)), request)
  output <- suppressWarnings(system2(python,
    c(shQuote(helper), shQuote(request), shQuote(response)),
    stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L)
    stop("Python n4m SNV sparse PLS-DA recipe oracle failed: ",
         paste(output, collapse = "\n"))
  oracle <- jsonlite::fromJSON(response)
  stopifnot(identical(numeric_result$variants[[2L]]$predictions,
                      as.integer(oracle$predictions)))
  unlink(work, recursive = TRUE)
}

strict <- identical(Sys.getenv("NIRS4ALL_REQUIRE_DAG_PARITY"), "1")
cli <- Sys.getenv("NIRS4ALL_DAGML_CLI")
if (!nzchar(cli)) cli <- Sys.which("dag-ml-cli")
available <- nzchar(cli) && file.exists(cli) &&
  requireNamespace("dagml", quietly = TRUE)
if (!available && strict)
  stop("strict native sparse PLS-DA parity requires dagml and dag-ml-cli")
if (available) {
  outcome <- nirs4all_dag_cv_refit_predict(
    pipeline, X, y, folds = 4L, cli = cli, split_steps = TRUE)
  stopifnot(identical(as.integer(outcome$fit_cv_result_count), 8L),
            identical(as.integer(outcome$refit_result_count), 2L))
  dag_predictions <- nirs4all_dag_predict(outcome, new_X)
  stopifnot(identical(dag_predictions, predictions))
  expected_oof <- numeric(nrow(X))
  for (fold in 0:3) {
    held <- seq_len(nrow(X))[(seq_len(nrow(X)) - 1L) %% 4L == fold]
    train <- setdiff(seq_len(nrow(X)), held)
    train <- train[order(rownames(X)[train])]
    fold_fit <- nirs4all_fit(pipeline, X[train, , drop = FALSE], y[train])
    expected_oof[held] <- as.integer(predict(
      fold_fit, X[held, , drop = FALSE])) - 1L
  }
  for (average in outcome$oof_average_results) {
    block <- average$aggregated_predictions[[1L]]
    ids <- vapply(block$unit_ids, `[[`, "", "id")
    values <- vapply(block$values, function(value)
      as.numeric(value[[1L]]), numeric(1))
    stopifnot(length(values) == nrow(X),
              max(abs(values - expected_oof[match(ids, rownames(X))])) < 1e-10)
  }
}
