if (requireNamespace("xgboost", quietly = TRUE)) {
  library(nirs4all)
  X <- as.matrix(iris[c(1:18, 51:68, 101:118), 1:4])
  y <- iris$Species[c(1:18, 51:68, 101:118)]
  colnames(X) <- paste0("feature", seq_len(ncol(X)))

  classifier <- nirs4all_pipeline(list(nirs4all_snv()),
    nirs4all_xgboost_classifier(nrounds = 12L, max_depth = 2L,
                                eta = 0.2, seed = 7L, nthread = 1L))
  fit <- nirs4all_fit(classifier, X, y)
  values <- nirs4all_predict_proba(fit, X)
  labels <- nirs4all_predict(fit, X)
  stopifnot(identical(dim(values), c(nrow(X), nlevels(y))),
            identical(colnames(values), levels(y)),
            max(abs(rowSums(values) - 1)) < 1e-5,
            identical(labels, factor(levels(y)[max.col(values)], levels = levels(y))))
  path <- tempfile(fileext = ".rds")
  nirs4all_save(fit, path)
  stopifnot(max(abs(nirs4all_predict_proba(nirs4all_load(path), X) - values)) < 1e-12)

  binary_y <- droplevels(y[y != "virginica"])
  binary_x <- X[y != "virginica", , drop = FALSE]
  binary_fit <- nirs4all_fit(nirs4all_pipeline(
    learner = nirs4all_xgboost_classifier(nrounds = 10L, nthread = 1L)),
    binary_x, binary_y)
  binary_prob <- nirs4all_predict_proba(binary_fit, binary_x)
  stopifnot(identical(colnames(binary_prob), levels(binary_y)),
            max(abs(rowSums(binary_prob) - 1)) < 1e-8)

  target <- as.numeric(iris$Sepal.Length[c(1:18, 51:68, 101:118)])
  regressor <- nirs4all_pipeline(list(nirs4all_snv()),
    nirs4all_xgboost(nrounds = 12L, max_depth = 2L, eta = 0.2,
                     seed = 7L, nthread = 1L))
  reg_fit <- nirs4all_fit(regressor, X, target)
  reg_pred <- nirs4all_predict(reg_fit, X)
  stopifnot(is.numeric(reg_pred), length(reg_pred) == nrow(X),
            all(is.finite(reg_pred)))
  reg_path <- tempfile(fileext = ".rds")
  nirs4all_save(reg_fit, reg_path)
  stopifnot(max(abs(nirs4all_predict(nirs4all_load(reg_path), X) - reg_pred)) < 1e-12)

  if (requireNamespace("nirs4allformats", quietly = TRUE)) {
    source <- system.file("extdata", "formats_classification.csv",
                          package = "nirs4all", mustWork = TRUE)
    dataset <- nirs4all_from_formats(source, target = "species")
    format_fit <- nirs4all_fit(classifier, dataset)
    format_prob <- nirs4all_predict_proba(format_fit, nirs4all_from_formats(source))
    stopifnot(identical(dim(format_prob), c(12L, 3L)),
              identical(colnames(format_prob), sort(unique(dataset$y))))
  }

  cli <- Sys.getenv("NIRS4ALL_DAGML_CLI")
  if (!nzchar(cli)) cli <- Sys.which("dag-ml-cli")
  if (nzchar(cli) && file.exists(cli) && requireNamespace("dagml", quietly = TRUE)) {
    outcome <- nirs4all_dag_cv_refit_predict(
      classifier, X, y, folds = 3L, cli = cli)
    stopifnot(as.integer(outcome$fit_cv_result_count) == 3L,
              identical(nirs4all_dag_predict(outcome, X[1:6, , drop = FALSE]),
                        labels[1:6]))
    expected_oof <- numeric(nrow(X))
    for (fold in 0:2) {
      validation <- seq_len(nrow(X))[(seq_len(nrow(X)) - 1L) %% 3L == fold]
      training <- setdiff(seq_len(nrow(X)), validation)
      fold_fit <- nirs4all_fit(classifier, X[training, , drop = FALSE], y[training])
      expected_oof[validation] <- as.numeric(nirs4all_predict(
        fold_fit, X[validation, , drop = FALSE])) - 1L
    }
    for (average in outcome$oof_average_results) {
      block <- average$aggregated_predictions[[1L]]
      ids <- vapply(block$unit_ids, `[[`, "", "id")
      oof <- vapply(block$values, function(value) as.numeric(value[[1L]]), numeric(1))
      expected_ids <- rownames(X)
      if (is.null(expected_ids))
        expected_ids <- sprintf("sample:%08d", seq_len(nrow(X)))
      stopifnot(!anyNA(match(ids, expected_ids)),
                max(abs(oof - expected_oof[match(ids, expected_ids)])) < 1e-8)
    }
    reg_outcome <- nirs4all_dag_cv_refit_predict(
      regressor, X, target, folds = 3L, cli = cli)
    stopifnot(as.integer(reg_outcome$fit_cv_result_count) == 3L,
              max(abs(nirs4all_dag_predict(reg_outcome, X[1:6, , drop = FALSE]) -
                      reg_pred[1:6])) < 1e-8)
  }
}
