strict <- identical(Sys.getenv("NIRS4ALL_REQUIRE_DAG_PARITY"), "1")
available <- requireNamespace("torch", quietly = TRUE) &&
  torch::torch_is_installed()
if (!available && strict)
  stop("strict torch classification parity requires the R torch CPU runtime")

if (available) {
  library(nirs4all)
  rows <- as.vector(rbind(1:12, 51:62, 101:112))
  X <- as.matrix(iris[rows, 1:4])
  y <- iris$Species[rows]
  builder <- local({
    width <- 7L
    function(n_features, n_classes) torch::nn_sequential(
      torch::nn_linear(n_features, width), torch::nn_tanh(),
      torch::nn_linear(width, n_classes))
  })
  pipelines <- list(
    mlp = nirs4all_pipeline(list(nirs4all_snv()),
      nirs4all_torch_mlp_classifier(hidden = 8L, epochs = 20L,
        learning_rate = 0.01, seed = 13L)),
    module = nirs4all_pipeline(list(nirs4all_snv()),
      nirs4all_torch_module_classifier(builder, name = "iris_logits",
        epochs = 20L, learning_rate = 0.01, seed = 13L)))

  cli <- Sys.getenv("NIRS4ALL_DAGML_CLI")
  if (!nzchar(cli)) cli <- Sys.which("dag-ml-cli")
  dag_available <- nzchar(cli) && file.exists(cli) &&
    requireNamespace("dagml", quietly = TRUE)
  if (!dag_available && strict)
    stop("strict torch classification parity requires dagml and dag-ml-cli")

  for (name in names(pipelines)) {
    pipeline <- pipelines[[name]]
    fitted <- nirs4all_fit(pipeline, X, y)
    independent <- nirs4all_fit(pipeline, X, y)
    probabilities <- nirs4all_predict_proba(fitted, X)
    labels <- nirs4all_predict(fitted, X)
    stopifnot(identical(dim(probabilities), c(nrow(X), nlevels(y))),
              identical(colnames(probabilities), levels(y)),
              max(abs(rowSums(probabilities) - 1)) < 1e-6,
              identical(labels, factor(levels(y)[max.col(probabilities)],
                                       levels = levels(y))),
              max(abs(probabilities - nirs4all_predict_proba(independent, X))) < 1e-6,
              !identical(fitted$state$module, independent$state$module))
    path <- tempfile(fileext = ".rds")
    nirs4all_save(fitted, path)
    loaded <- nirs4all_load(path)
    stopifnot(max(abs(nirs4all_predict_proba(loaded, X) - probabilities)) < 1e-6,
              identical(nirs4all_predict(loaded, X), labels))
    if (identical(name, "module")) {
      matrix_path <- tempfile(fileext = ".rds")
      prediction_path <- tempfile(fileext = ".rds")
      saveRDS(X, matrix_path)
      expression <- paste(
        "args <- commandArgs(TRUE); library(nirs4all);",
        "fit <- nirs4all_load(args[[1]]); X <- readRDS(args[[2]]);",
        "saveRDS(nirs4all_predict_proba(fit, X), args[[3]])")
      output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
        c("-e", shQuote(expression), shQuote(path), shQuote(matrix_path),
          shQuote(prediction_path)), stdout = TRUE, stderr = TRUE))
      status <- attr(output, "status")
      if (is.null(status)) status <- 0L
      if (status != 0L)
        stop("torch classifier fresh-process replay failed: ",
             paste(output, collapse = "\n"))
      stopifnot(max(abs(readRDS(prediction_path) - probabilities)) < 1e-6)
      unlink(c(matrix_path, prediction_path))
    }
    unlink(path)

    if (dag_available) {
      outcome <- nirs4all_dag_cv_refit_predict(
        pipeline, X, y, folds = 4L, split_steps = TRUE, cli = cli)
      stopifnot(identical(as.integer(outcome$fit_cv_result_count), 8L),
                identical(as.integer(outcome$refit_result_count), 2L),
                identical(nirs4all_dag_predict(outcome, X[1:6, , drop = FALSE]),
                          labels[1:6]))
      expected_oof <- numeric(nrow(X))
      for (fold in 0:3) {
        validation <- seq_len(nrow(X))[(seq_len(nrow(X)) - 1L) %% 4L == fold]
        training <- setdiff(seq_len(nrow(X)), validation)
        training <- training[order(rownames(X)[training])]
        fold_fit <- nirs4all_fit(pipeline, X[training, , drop = FALSE],
                                y[training])
        expected_oof[validation] <- as.numeric(nirs4all_predict(
          fold_fit, X[validation, , drop = FALSE])) - 1L
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
  }

  invalid <- nirs4all_pipeline(learner = nirs4all_torch_module_classifier(
    function(n_features, n_classes) torch::nn_linear(n_features, 1L),
    epochs = 1L))
  stopifnot(inherits(try(nirs4all_fit(invalid, X, y), silent = TRUE), "try-error"),
            inherits(try(nirs4all_torch_module_classifier(builder,
              name = "invalid name"), silent = TRUE), "try-error"))
}
