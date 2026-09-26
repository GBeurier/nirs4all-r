strict <- identical(Sys.getenv("NIRS4ALL_REQUIRE_DAG_PARITY"), "1")
cli <- Sys.getenv("NIRS4ALL_DAGML_CLI")
if (!nzchar(cli)) cli <- Sys.which("dag-ml-cli")
available <- nzchar(cli) && file.exists(cli) &&
  requireNamespace("dagml", quietly = TRUE)
if (!available && strict) stop("native splitter DAG test requires dagml and CLI")

{
  library(nirs4all)
  X <- outer(seq_len(30L), seq_len(8L),
             function(i, j) sin(i * j / 11) + cos((i + j) / 7) + i * j / 100)
  sample_ids <- sprintf("sample:%04d", seq_len(nrow(X)))
  rownames(X) <- sample_ids
  y <- 1.2 + 0.7 * X[, 2L] - 0.3 * X[, 6L]
  names(y) <- sample_ids
  groups <- rep(sprintf("batch:%02d", seq_len(15L)), each = 2L)
  names(groups) <- sample_ids
  pipeline <- nirs4all_pipeline(learner = nirs4all_pls(2L))
  specs <- list(
    spxy_fold = nirs4all_native_splitter("spxy_fold", n_splits = 3L),
    spxy_group_fold = nirs4all_native_splitter("spxy_group_fold", n_splits = 3L),
    binned_strat_group_fold = nirs4all_native_splitter(
      "binned_strat_group_fold", n_splits = 3L, n_bins = 2L, seed = 42L)
  )
  for (kind in names(specs)) {
    grouped <- kind != "spxy_fold"
    assigned <- nirs4all:::.nirs4all_native_cv_assignment(
      specs[[kind]], X, y, sample_ids, if (grouped) groups else NULL, 3L)
    stopifnot(identical(sort(unlist(lapply(assigned$splits, `[[`, "test"),
                                    use.names = FALSE)), seq_len(nrow(X))),
              identical(sort(unique(assigned$fold_number)), seq_len(3L)))
    for (index in seq_len(3L)) {
      split <- assigned$splits[[index]]
      stopifnot(identical(sample_ids[split$test], split$test_sample_ids),
                identical(which(assigned$fold_number == index), sort(split$test)))
      if (grouped)
        stopifnot(!length(intersect(groups[split$train], groups[split$test])))
    }
  }
  stopifnot(inherits(try(nirs4all_dag_cv_refit_predict(
    pipeline, X, y, folds = 3L, sample_ids = sample_ids,
    splitter = nirs4all_native_splitter("kennard_stone"),
    cli = "not-needed"), silent = TRUE), "try-error"),
    inherits(try(nirs4all_dag_cv_refit_predict(
      pipeline, X, y, folds = 3L, sample_ids = sample_ids,
      splitter = nirs4all_native_splitter("spxy_fold", n_splits = 4L),
      cli = "not-needed"), silent = TRUE), "try-error"),
    inherits(try(nirs4all_dag_cv_refit_predict(
      pipeline, X, y, folds = 3L, sample_ids = sample_ids,
      group_ids = groups, splitter = specs$spxy_fold,
      cli = "not-needed"), silent = TRUE), "try-error"))
  if (available && strict) {
  for (kind in names(specs)) {
    grouped <- kind != "spxy_fold"
    outcome <- nirs4all_dag_cv_refit_predict(
      pipeline, X, y, folds = 3L, sample_ids = sample_ids,
      group_ids = if (grouped) groups else NULL,
      splitter = specs[[kind]], cli = cli)
    dsl <- jsonlite::fromJSON(file.path(outcome$workdir, "dsl.json"),
                              simplifyVector = FALSE)
    stopifnot(identical(dsl$split_invocation$params$kind, "precomputed"),
              identical(dsl$split_invocation$fold_set$id,
                        paste0("folds:nirs4all-r:n4m:", kind)),
              identical(as.integer(outcome$fit_cv_result_count), 3L))
    expected_oof <- stats::setNames(numeric(nrow(X)), sample_ids)
    for (index in seq_len(3L)) {
      native <- nirs4all_native_split(specs[[kind]], X, y, sample_ids,
        if (grouped) groups else NULL, fold = index)
      fold <- dsl$split_invocation$fold_set$folds[[index]]
      train_ids <- as.character(unlist(fold$train_sample_ids, use.names = FALSE))
      valid_ids <- as.character(unlist(fold$validation_sample_ids,
                                       use.names = FALSE))
      stopifnot(identical(train_ids, sort(native$train_sample_ids)),
                identical(valid_ids, sort(native$test_sample_ids)))
      if (grouped)
        stopifnot(!length(intersect(groups[train_ids], groups[valid_ids])))
      fitted <- nirs4all_fit(pipeline, X[train_ids, , drop = FALSE], y[train_ids])
      expected_oof[valid_ids] <- predict(fitted, X[valid_ids, , drop = FALSE])
    }
    oof <- outcome$oof_average_results[[1L]]$aggregated_predictions[[1L]]
    oof_ids <- vapply(oof$unit_ids, `[[`, "", "id")
    oof_values <- vapply(oof$values,
                         function(value) as.numeric(value[[1L]]), numeric(1))
    stopifnot(length(oof_ids) == nrow(X),
              max(abs(oof_values - expected_oof[oof_ids])) < 1e-10)
    heldout <- X[c(2L, 11L, 25L), , drop = FALSE] + 0.03
    stopifnot(max(abs(nirs4all_dag_predict(outcome, heldout) -
                      predict(nirs4all_fit(pipeline, X, y), heldout))) < 1e-10)
  }
  stopifnot(inherits(try(nirs4all_dag_cv_refit_predict(
    pipeline, X, y, folds = 3L, sample_ids = sample_ids,
    splitter = nirs4all_native_splitter("kennard_stone"), cli = cli),
    silent = TRUE), "try-error"),
    inherits(try(nirs4all_dag_cv_refit_predict(
      pipeline, X, y, folds = 3L, sample_ids = sample_ids,
      splitter = nirs4all_native_splitter("spxy_fold", n_splits = 4L), cli = cli),
      silent = TRUE), "try-error"),
    inherits(try(nirs4all_dag_cv_refit_predict(
      pipeline, X, y, folds = 3L, sample_ids = sample_ids, group_ids = groups,
      splitter = specs$spxy_fold, cli = cli), silent = TRUE), "try-error"))
  message("three native fold kinds drive DAG CV/OOF/refit/replay with exact IDs")
  }
  message("three native fold kinds cover disjoint sample IDs and preserve groups")
}
