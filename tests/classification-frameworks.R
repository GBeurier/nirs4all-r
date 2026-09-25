strict <- identical(Sys.getenv("NIRS4ALL_REQUIRE_DAG_PARITY"), "1")
available <- requireNamespace("parsnip", quietly = TRUE) &&
  requireNamespace("mlr3", quietly = TRUE) &&
  requireNamespace("rpart", quietly = TRUE)
if (!available && strict)
  stop("strict classification parity requires parsnip, mlr3 and rpart")

if (available) {
  library(nirs4all)
  rows <- as.vector(rbind(1:12, 51:62, 101:112))
  X <- as.matrix(iris[rows, 1:4])
  y <- iris$Species[rows]
  pipelines <- list(
    parsnip = nirs4all_pipeline(list(nirs4all_snv()),
      nirs4all_parsnip_classifier(parsnip::set_engine(
        parsnip::decision_tree(mode = "classification"), "rpart"))),
    mlr3 = nirs4all_pipeline(list(nirs4all_snv()),
      nirs4all_mlr3_classifier(mlr3::lrn("classif.rpart", cp = 0))))
  cli <- Sys.getenv("NIRS4ALL_DAGML_CLI")
  if (!nzchar(cli)) cli <- Sys.which("dag-ml-cli")
  dag_available <- nzchar(cli) && file.exists(cli) &&
    requireNamespace("dagml", quietly = TRUE)
  if (!dag_available && strict)
    stop("strict classification parity requires dagml and dag-ml-cli")

  for (name in names(pipelines)) {
    pipeline <- pipelines[[name]]
    fit <- nirs4all_fit(pipeline, X, y)
    labels <- nirs4all_predict(fit, X)
    proba <- nirs4all_predict_proba(fit, X)
    stopifnot(is.factor(labels), identical(levels(labels), levels(y)),
              identical(dim(proba), c(nrow(X), nlevels(y))),
              identical(colnames(proba), levels(y)),
              max(abs(rowSums(proba) - 1)) < 1e-10)
    path <- tempfile(fileext = ".rds")
    nirs4all_save(fit, path)
    stopifnot(identical(nirs4all_predict(nirs4all_load(path), X), labels))

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

  if (dag_available) {
    outcome <- nirs4all_dag_cv_refit_predict(
      pipelines, X, y, folds = 4L, cli = cli)
    stopifnot(!is.null(outcome$bundle$selected_variant_id),
              length(outcome$oof_average_results) >= 2L,
              is.factor(nirs4all_dag_predict(outcome, X[1:3, , drop = FALSE])))
  }
}
