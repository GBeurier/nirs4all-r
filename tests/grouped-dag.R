strict <- identical(Sys.getenv("NIRS4ALL_REQUIRE_DAG_PARITY"), "1")
cli <- Sys.getenv("NIRS4ALL_DAGML_CLI")
if (!nzchar(cli)) cli <- Sys.which("dag-ml-cli")
available <- nzchar(cli) && file.exists(cli) &&
  requireNamespace("dagml", quietly = TRUE)
if (!available && strict) stop("strict grouped DAG test requires dagml and dag-ml-cli")

if (available) {
  library(nirs4all)
  X <- outer(seq_len(12L), seq_len(8L),
             function(i, j) sin(i * j / 7) + i * j / 50)
  sample_ids <- sprintf("sample:%04d", seq_len(nrow(X)))
  rownames(X) <- sample_ids
  groups <- c(rep("batch:A", 4L), rep("batch:B", 3L),
              rep("batch:C", 2L), rep("batch:D", 3L))
  names(groups) <- sample_ids
  y <- 2 + X[, 2L] - 0.3 * X[, 5L] +
    unname(c(A = 0, B = 0.2, C = -0.1, D = 0.4)[substring(groups, 7L)])
  names(y) <- sample_ids
  pipeline <- nirs4all_pipeline(learner = nirs4all_pls(2L))
  outcome <- nirs4all_dag_cv_refit_predict(
    pipeline, X, y, folds = 3L, sample_ids = sample_ids,
    group_ids = groups, cli = cli)
  dsl <- jsonlite::fromJSON(file.path(outcome$workdir, "dsl.json"),
                            simplifyVector = FALSE)
  stopifnot(identical(dsl$leakage_policy$split_unit, "group"),
            isTRUE(dsl$leakage_policy$require_group_ids),
            identical(as.integer(outcome$fit_cv_result_count), 3L))
  python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON", "")
  helper <- if (file.exists("helpers/native_n4mm_peer.py"))
    "helpers/native_n4mm_peer.py" else "tests/helpers/native_n4mm_peer.py"
  if (nzchar(python)) stopifnot(file.exists(python), file.exists(helper))
  expected_oof <- stats::setNames(numeric(nrow(X)), sample_ids)
  for (fold in dsl$split_invocation$fold_set$folds) {
    train_ids <- as.character(unlist(fold$train_sample_ids, use.names = FALSE))
    validation_ids <- as.character(unlist(fold$validation_sample_ids,
                                          use.names = FALSE))
    stopifnot(!length(intersect(unique(groups[train_ids]),
                                unique(groups[validation_ids]))))
    trained <- nirs4all_fit(pipeline, X[train_ids, , drop = FALSE], y[train_ids])
    expected_oof[validation_ids] <- predict(trained,
                                            X[validation_ids, , drop = FALSE])
    if (nzchar(python)) {
      request_path <- tempfile(fileext = ".json")
      model_path <- tempfile(fileext = ".n4mm")
      result_path <- tempfile(fileext = ".json")
      rows <- function(matrix) lapply(seq_len(nrow(matrix)), function(index)
        unname(as.numeric(matrix[index, ])))
      writeLines(as.character(jsonlite::toJSON(list(
        X = rows(X[train_ids, , drop = FALSE]),
        y = unname(as.numeric(y[train_ids])),
        predict_X = rows(X[validation_ids, , drop = FALSE])),
        auto_unbox = TRUE, digits = NA)), request_path)
      output <- suppressWarnings(system2(python, c(
        shQuote(helper), "fit_plain", shQuote(model_path),
        shQuote(request_path), shQuote(result_path)),
        stdout = TRUE, stderr = TRUE))
      status <- attr(output, "status")
      if (is.null(status)) status <- 0L
      if (status != 0L) stop("Python grouped fold fit failed: ",
                             paste(output, collapse = "\n"))
      python_predictions <- jsonlite::fromJSON(result_path)$predictions
      stopifnot(length(python_predictions) == length(validation_ids),
                max(abs(python_predictions -
                          expected_oof[validation_ids])) < 1e-10)
      unlink(c(request_path, model_path, result_path))
    }
  }
  oof <- outcome$oof_average_results[[1L]]$aggregated_predictions[[1L]]
  oof_ids <- vapply(oof$unit_ids, `[[`, "", "id")
  oof_values <- vapply(oof$values,
                       function(value) as.numeric(value[[1L]]), numeric(1))
  stopifnot(length(oof_ids) == nrow(X),
            max(abs(oof_values - expected_oof[oof_ids])) < 1e-10)
  external <- X[1:4, , drop = FALSE] + 0.01
  stopifnot(max(abs(nirs4all_dag_predict(outcome, external) -
                    predict(nirs4all_fit(pipeline, X, y), external))) < 1e-10)

  wrong_names <- groups
  names(wrong_names) <- rev(sample_ids)
  stopifnot(inherits(try(nirs4all_dag_cv_refit_predict(
    pipeline, X, y, folds = 3L, sample_ids = sample_ids,
    group_ids = wrong_names, cli = cli), silent = TRUE), "try-error"))
  stopifnot(inherits(try(nirs4all_dag_cv_refit_predict(
    pipeline, X, y, folds = 3L, sample_ids = sample_ids,
    group_ids = rep("one-group", nrow(X)), cli = cli), silent = TRUE),
    "try-error"))

  data_path <- file.path(outcome$workdir, "data.rds")
  saved <- readRDS(data_path)
  tampered <- saved
  tampered$group_ids[[1L]] <- "batch:tampered"
  saveRDS(tampered, data_path)
  adapter <- file.path(outcome$workdir, "run-r-adapter")
  variables <- c(NIRS4ALL_DAG_DATA_RDS = data_path,
                 NIRS4ALL_DAG_DSL = file.path(outcome$workdir, "dsl.json"),
                 NIRS4ALL_DAG_ENVELOPE = file.path(outcome$workdir,
                                                  "envelope.json"),
                 NIRS4ALL_DAG_ARTIFACT_DIR = file.path(outcome$workdir,
                                                       "artifacts"))
  verified <- suppressWarnings(system2(adapter, "--verify",
    env = paste0(names(variables), "=", variables), stdout = TRUE, stderr = TRUE))
  stopifnot(!is.null(attr(verified, "status")), attr(verified, "status") != 0L)
  saveRDS(saved, data_path)
  message("group-aware DAG CV/OOF/refit/replay and tamper refusal passed")
}
