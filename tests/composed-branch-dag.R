strict <- identical(Sys.getenv("NIRS4ALL_REQUIRE_DAG_PARITY"), "1")
cli <- Sys.getenv("NIRS4ALL_DAGML_CLI")
if (!nzchar(cli)) cli <- Sys.which("dag-ml-cli")
available <- nzchar(cli) && file.exists(cli) &&
  requireNamespace("dagml", quietly = TRUE)
if (!available && strict)
  stop("strict composed-branch DAG test requires dagml and dag-ml-cli")

if (available) {
  library(nirs4all)
  X <- outer(seq_len(18L), seq_len(9L), function(i, j)
    1 + sin(i * j / 11) + cos((i + 2 * j) / 7) + i * j / 120)
  colnames(X) <- sprintf("wavelength_%03d", seq_len(ncol(X)))
  ids <- sprintf("sample:%08d", seq_len(nrow(X)))
  rownames(X) <- ids
  y <- 2 + 0.7 * X[, 2L] - 0.4 * X[, 6L] + 0.25 * X[, 9L]
  names(y) <- ids
  pipeline <- nirs4all_pipeline(list(
    nirs4all_spa(top_k = 6L, n_components = 2L),
    nirs4all_concat(list(
      scatter = list(nirs4all_msc(), nirs4all_detrend(1L)),
      normalized = list(nirs4all_snv()))),
    nirs4all_snv()), nirs4all_pls(2L))
  graph <- nirs4all_dag_cv_refit_predict(pipeline, X, y, folds = 3L,
    split_steps = TRUE, cli = cli, process_workers = 2L)
  dsl <- jsonlite::fromJSON(file.path(graph$workdir, "dsl.json"),
                            simplifyVector = FALSE)
  kinds <- vapply(dsl$steps, `[[`, "", "kind")
  stopifnot(identical(kinds, c("transform", "concat_transform", "transform", "model")),
            length(graph$bundle$refit_artifacts) == 7L)
  records <- graph$bundle$refit_artifacts
  record_ids <- vapply(records, `[[`, "", "node_id")
  stopifnot(setequal(record_ids, c("transform:nirs4all-r:001",
    "transform:nirs4all-r:002", "transform:nirs4all-r:002:scatter:001",
    "transform:nirs4all-r:002:scatter:002",
    "transform:nirs4all-r:002:normalized:001",
    "transform:nirs4all-r:003", "model:nirs4all-r")))
  concat_record <- records[[match("transform:nirs4all-r:002", record_ids)]]
  stopifnot(identical(readRDS(concat_record$artifact$uri)$n_features, 6L))

  expected_oof <- stats::setNames(numeric(nrow(X)), ids)
  for (fold in dsl$split_invocation$fold_set$folds) {
    training <- as.character(unlist(fold$train_sample_ids, use.names = FALSE))
    validation <- as.character(unlist(fold$validation_sample_ids,
                                      use.names = FALSE))
    stopifnot(!length(intersect(training, validation)))
    expected_oof[validation] <- predict(nirs4all_fit(pipeline,
      X[training, , drop = FALSE], y[training]),
      X[validation, , drop = FALSE])
  }
  for (average in graph$oof_average_results) {
    block <- average$aggregated_predictions[[1L]]
    block_ids <- vapply(block$unit_ids, `[[`, "", "id")
    values <- vapply(block$values, function(value)
      as.numeric(value[[1L]]), numeric(1))
    stopifnot(max(abs(values - expected_oof[block_ids])) < 1e-9)
  }
  refit <- nirs4all_fit(pipeline, X, y)
  external <- X[1:5, , drop = FALSE] + 0.005
  stopifnot(max(abs(nirs4all_dag_predict(graph, external) -
                    predict(refit, external))) < 1e-9)
  wrong_columns <- external[, rev(seq_len(ncol(external))), drop = FALSE]
  stopifnot(inherits(try(nirs4all_dag_predict(graph, wrong_columns),
                         silent = TRUE), "try-error"))

  python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON", "")
  python_repo <- Sys.getenv("NIRS4ALL_PYTHON_NIRS4ALL_REPO", "")
  if (nzchar(python) && nzchar(python_repo)) {
    helper <- if (file.exists("helpers/python_topology_peer.py"))
      "helpers/python_topology_peer.py" else
      "tests/helpers/python_topology_peer.py"
    request <- tempfile(fileext = ".json")
    result <- tempfile(fileext = ".json")
    on.exit(unlink(c(request, result)), add = TRUE)
    writeLines(nirs4all_export_pipeline(pipeline), request)
    output <- suppressWarnings(system2(python, c(shQuote(helper),
      shQuote(request), shQuote(result), shQuote(python_repo)),
      stdout = TRUE, stderr = TRUE))
    status <- attr(output, "status")
    if (is.null(status)) status <- 0L
    if (status != 0L)
      stop("Python nirs4all composed topology oracle failed: ",
           paste(output, collapse = "\n"))
    topology <- jsonlite::fromJSON(result)
    stopifnot(isTRUE(topology$feature_merge),
              !isTRUE(topology$stacking),
              !isTRUE(topology$branches_without_merge),
              identical(as.integer(topology$models), 1L))
    numerical_helper <- if (file.exists("helpers/python_composed_branch_peer.py"))
      "helpers/python_composed_branch_peer.py" else
      "tests/helpers/python_composed_branch_peer.py"
    rows <- function(matrix) lapply(seq_len(nrow(matrix)), function(index)
      unname(as.numeric(matrix[index, ])))
    python_predict <- function(train_X, train_y, validation_X) {
      jsonlite::write_json(list(
        python_root = python_repo,
        recipe = jsonlite::fromJSON(nirs4all_export_pipeline(pipeline),
                                    simplifyVector = FALSE),
        train = rows(train_X), y = unname(as.numeric(train_y)),
        validation = rows(validation_X)), request,
        auto_unbox = TRUE, digits = 17)
      output <- suppressWarnings(system2(python, c(shQuote(numerical_helper),
        shQuote(request), shQuote(result)), stdout = TRUE, stderr = TRUE))
      status <- attr(output, "status")
      if (is.null(status)) status <- 0L
      if (status != 0L)
        stop("Python nirs4all composed numerical oracle failed: ",
             paste(output, collapse = "\n"))
      jsonlite::fromJSON(result)$predictions
    }
    python_full <- python_predict(X, y, external)
    stopifnot(length(python_full) == nrow(external),
              max(abs(python_full - predict(refit, external))) < 1e-8)
    fold <- dsl$split_invocation$fold_set$folds[[1L]]
    training <- as.character(unlist(fold$train_sample_ids, use.names = FALSE))
    validation <- as.character(unlist(fold$validation_sample_ids,
                                      use.names = FALSE))
    python_fold <- python_predict(X[training, , drop = FALSE], y[training],
                                  X[validation, , drop = FALSE])
    stopifnot(length(python_fold) == length(validation),
              max(abs(python_fold - expected_oof[validation])) < 1e-8)
  }
  child_record <- records[[match(
    "transform:nirs4all-r:002:scatter:001", record_ids)]]
  child_path <- child_record$artifact$uri
  child_state <- readRDS(child_path)
  child_state$n_features <- child_state$n_features + 1L
  saveRDS(child_state, child_path)
  stopifnot(inherits(try(nirs4all_dag_predict(graph, external),
                         silent = TRUE), "try-error"))
  message("composed SPA -> parallel n4m branches -> join -> SNV -> PLS DAG parity passed")
}
