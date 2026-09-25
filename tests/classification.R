if (requireNamespace("ranger", quietly = TRUE)) {
  library(nirs4all)
  rows <- as.vector(rbind(1:12, 51:62, 101:112))
  X <- as.matrix(iris[rows, 1:4])
  y <- iris$Species[rows]
  pipeline <- nirs4all_pipeline(
    list(nirs4all_snv()),
    nirs4all_ranger_classifier(num.trees = 20L, seed = 7L,
                               num.threads = 1L))
  fitted <- nirs4all_fit(pipeline, X, y)
  direct <- nirs4all_predict(fitted, X)
  probabilities <- nirs4all_predict_proba(fitted, X)
  saved_path <- tempfile(fileext = ".rds")
  nirs4all_save(fitted, saved_path)
  stopifnot(is.factor(direct), identical(levels(direct), levels(y)),
            identical(colnames(probabilities), levels(y)),
            identical(dim(probabilities), c(nrow(X), nlevels(y))),
            max(abs(rowSums(probabilities) - 1)) < 1e-10,
            identical(direct, nirs4all_predict(fitted, X)),
            identical(probabilities, nirs4all_predict_proba(fitted, X)),
            identical(direct, nirs4all_predict(nirs4all_load(saved_path), X)))
  stopifnot(inherits(try(nirs4all_fit(pipeline, X, as.numeric(y)),
                         silent = TRUE), "try-error"),
            inherits(try(nirs4all_predict_proba(nirs4all_fit(
              nirs4all_pipeline(learner = nirs4all_lm()),
              X, as.numeric(y)), X), silent = TRUE), "try-error"))
  misnamed_y <- y
  names(misnamed_y) <- rev(rownames(X))
  stopifnot(inherits(try(nirs4all_fit(pipeline, X, misnamed_y),
                         silent = TRUE), "try-error"))

  strict <- identical(Sys.getenv("NIRS4ALL_REQUIRE_DAG_PARITY"), "1")
  cli <- Sys.getenv("NIRS4ALL_DAGML_CLI")
  if (!nzchar(cli)) cli <- Sys.which("dag-ml-cli")
  available <- nzchar(cli) && file.exists(cli) &&
    requireNamespace("dagml", quietly = TRUE)
  if (!available && strict)
    stop("strict native classification parity requires dagml and dag-ml-cli")
  if (available) {
    stopifnot(inherits(try(nirs4all_dag_cv_refit_predict(
      pipeline, X, misnamed_y, folds = 4L, cli = cli), silent = TRUE),
      "try-error"))
    outcome <- nirs4all_dag_cv_refit_predict(
      pipeline, X, y, folds = 4L, cli = cli, split_steps = TRUE)
    stopifnot(identical(as.integer(outcome$fit_cv_result_count), 8L),
              identical(as.integer(outcome$refit_result_count), 2L),
              length(outcome$oof_average_results) >= 1L)
    external <- nirs4all_dag_predict(outcome, X[1:6, , drop = FALSE])
    stopifnot(identical(external, direct[1:6]))
    expected_oof <- numeric(nrow(X))
    for (fold in 0:3) {
      validation <- seq_len(nrow(X))[(seq_len(nrow(X)) - 1L) %% 4L == fold]
      training <- setdiff(seq_len(nrow(X)), validation)
      training <- training[order(rownames(X)[training])]
      fold_fitted <- nirs4all_fit(pipeline, X[training, , drop = FALSE],
                                 y[training])
      expected_oof[validation] <- as.numeric(nirs4all_predict(
        fold_fitted, X[validation, , drop = FALSE])) - 1L
      fold_probability <- nirs4all_predict_proba(
        fold_fitted, X[validation, , drop = FALSE])
      stopifnot(max(abs(rowSums(fold_probability) - 1)) < 1e-10)
    }
    for (average in outcome$oof_average_results) {
      block <- average$aggregated_predictions[[1L]]
      ids <- vapply(block$unit_ids, `[[`, "", "id")
      values <- vapply(block$values, function(value)
        as.numeric(value[[1L]]), numeric(1))
      expected_ids <- rownames(X)
      stopifnot(length(values) == nrow(X),
                max(abs(values - expected_oof[match(ids, expected_ids)])) < 1e-10)
    }
    bad_y <- factor(rep("setosa", nrow(X)), levels = levels(y))
    bad_y[2L] <- "versicolor"
    bad_y[3L] <- "virginica"
    stopifnot(inherits(try(nirs4all_dag_cv_refit_predict(
      pipeline, X, bad_y, folds = 4L, cli = cli), silent = TRUE), "try-error"))
  }
}
