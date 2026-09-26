strict <- identical(Sys.getenv("NIRS4ALL_REQUIRE_DAG_PARITY"), "1")
cli <- Sys.getenv("NIRS4ALL_DAGML_CLI")
if (!nzchar(cli)) cli <- Sys.which("dag-ml-cli")
available <- nzchar(cli) && file.exists(cli) &&
  requireNamespace("dagml", quietly = TRUE)
if (!available && strict) stop("native augmentation DAG test requires dagml and CLI")

if (available && strict) {
  library(nirs4all)
  X <- outer(seq_len(30L), seq_len(8L),
             function(i, j) sin(i * j / 11) + cos((i + j) / 7) + i * j / 100)
  sample_ids <- sprintf("sample:%04d", seq_len(nrow(X)))
  rownames(X) <- sample_ids
  y <- 1.2 + .7 * X[, 2L] - .3 * X[, 6L]
  names(y) <- sample_ids
  gaussian <- nirs4all_native_augmentation("gaussian_noise", .03, seed = 42)
  pipeline <- nirs4all_pipeline(learner = nirs4all_pls(2L),
                               augmentations = list(gaussian))
  splitter <- nirs4all_native_splitter("spxy_fold", n_splits = 3L)
  outcome <- nirs4all_dag_cv_refit_predict(
    pipeline, X, y, folds = 3L, sample_ids = sample_ids,
    splitter = splitter, cli = cli)
  dsl <- jsonlite::fromJSON(file.path(outcome$workdir, "dsl.json"),
                            simplifyVector = FALSE)
  stopifnot(identical(dsl$steps[[1L]]$params$augmentations[[1L]]$kind,
                      "gaussian_noise"),
            identical(as.integer(outcome$fit_cv_result_count), 3L))
  expected_oof <- stats::setNames(numeric(nrow(X)), sample_ids)
  for (fold in dsl$split_invocation$fold_set$folds) {
    train_ids <- as.character(unlist(fold$train_sample_ids, use.names = FALSE))
    valid_ids <- as.character(unlist(fold$validation_sample_ids,
                                     use.names = FALSE))
    train_X <- X[train_ids, , drop = FALSE]
    valid_X <- X[valid_ids, , drop = FALSE]
    augmented_train <- nirs4all:::nirs4all_augment_training(
      train_X, list(gaussian))
    stopifnot(identical(rownames(augmented_train), train_ids),
              identical(rownames(valid_X), valid_ids),
              identical(names(y[train_ids]), train_ids),
              identical(names(y[valid_ids]), valid_ids))
    manual <- nirs4all_fit(nirs4all_pipeline(learner = nirs4all_pls(2L)),
                          augmented_train, y[train_ids])
    expected_oof[valid_ids] <- predict(manual, valid_X)
    stopifnot(max(abs(predict(nirs4all_fit(pipeline, train_X,
                                         y[train_ids]), valid_X) -
                      expected_oof[valid_ids])) < 1e-12)
  }
  oof <- outcome$oof_average_results[[1L]]$aggregated_predictions[[1L]]
  oof_ids <- vapply(oof$unit_ids, `[[`, "", "id")
  oof_values <- vapply(oof$values,
                       function(value) as.numeric(value[[1L]]), numeric(1))
  stopifnot(length(oof_ids) == nrow(X),
            max(abs(oof_values - expected_oof[oof_ids])) < 1e-10)
  heldout <- X[c(2L, 12L, 25L), , drop = FALSE] + .031
  direct <- nirs4all_fit(pipeline, X, y)
  stopifnot(max(abs(nirs4all_dag_predict(outcome, heldout) -
                    predict(direct, heldout))) < 1e-10)
  stopifnot(inherits(try(nirs4all_dag_cv_refit_predict(
    pipeline, X, y, folds = 3L, sample_ids = sample_ids,
    splitter = splitter, split_steps = TRUE, cli = cli), silent = TRUE),
    "try-error"))
  message("native augmentation stays on X_train across DAG OOF/refit/predict")
}
