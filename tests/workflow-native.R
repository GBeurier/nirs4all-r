strict <- identical(Sys.getenv("NIRS4ALL_REQUIRE_DAG_PARITY"), "1")
cli <- Sys.getenv("NIRS4ALL_DAGML_CLI")
if (!nzchar(cli)) cli <- Sys.which("dag-ml-cli")
available <- nzchar(cli) && file.exists(cli) &&
  requireNamespace("dagml", quietly = TRUE)
if (!available && strict) stop("native workflow test requires dagml and dag-ml-cli")

if (available) {
  library(nirs4all)
  nirs4all_run <- getFromNamespace("nirs4all_run", "nirs4all")
  nirs4all_workflow_predict <- getFromNamespace("nirs4all_workflow_predict", "nirs4all")
  nirs4all_workflow_retrain <- getFromNamespace("nirs4all_workflow_retrain", "nirs4all")
  nirs4all_workflow_export <- getFromNamespace("nirs4all_workflow_export", "nirs4all")
  nirs4all_workflow_load <- getFromNamespace("nirs4all_workflow_load", "nirs4all")
  X <- outer(seq_len(15L), seq_len(8L),
             function(i, j) sin(i * j / 7) + i * j / 50)
  y <- 2 + X[, 2L] - 0.3 * X[, 5L]
  ids <- sprintf("sample:%08d", seq_len(nrow(X)))
  run <- nirs4all_run(X, y, components = c(1L, 2L),
                      preprocessing = c("snv", "msc"), folds = 3L,
                      sample_ids = ids, cli = cli)
  message("workflow FIT_CV=", run$outcome$fit_cv_result_count,
          " REFIT=", run$outcome$refit_result_count)
  stopifnot(inherits(run, "nirs4all_workflow"),
            identical(as.integer(run$outcome$fit_cv_result_count), 9L),
            identical(as.integer(run$outcome$refit_result_count), 3L),
            identical(run$winner_variant_id,
                      run$outcome$bundle$selected_variant_id),
            length(run$outcome$bundle$scores$reports) >= 6L)
  external <- X[1:3, , drop = FALSE] + 0.02
  expected <- nirs4all_dag_predict(run$outcome, external)
  stopifnot(isTRUE(all.equal(nirs4all_workflow_predict(run, external),
                             expected, tolerance = 1e-10)))
  destination <- tempfile("nirs4all-workflow-export-")
  nirs4all_workflow_export(run, destination)
  moved <- tempfile("nirs4all-workflow-moved-")
  stopifnot(file.rename(destination, moved))
  unlink(run$workdir, recursive = TRUE)
  loaded <- nirs4all_workflow_load(moved)
  stopifnot(isTRUE(all.equal(nirs4all_workflow_predict(loaded, external),
                             expected, tolerance = 1e-10)))
  retrained <- nirs4all_workflow_retrain(
    run, X + 0.01, y + 0.1, sample_ids = ids, cli = cli)
  stopifnot(inherits(retrained, "nirs4all_workflow"),
            identical(as.integer(retrained$outcome$fit_cv_result_count), 9L))
  message("native workflow run/predict/export/load/retrain passed")
}
