library(nirs4all)
fixture <- Sys.getenv("NIRS4ALL_WORKSPACE_FIXTURE")
if (nzchar(fixture)) {
  workspace <- nirs4all_open_workspace(fixture)
  runs <- nirs4all_workspace_runs(workspace)
  run <- runs[[1L]]$native_run_id
  rows <- nirs4all_workspace_predictions(workspace, run)
  stopifnot(length(rows) == 5L, length(rows[[1L]]$result_metadata$sample_ids) > 0L)
  data <- jsonlite::fromJSON(Sys.getenv("NIRS4ALL_WORKSPACE_PREDICT_DATA"))
  oracle <- jsonlite::fromJSON(Sys.getenv("NIRS4ALL_WORKSPACE_PREDICT_ORACLE"))
  session <- nirs4all_workspace_session(workspace, run)
  predicted <- nirs4all_workspace_predict(session, data$x, data$sample_ids)
  stopifnot(identical(unlist(predicted$sample_ids), data$sample_ids),
            max(abs(unlist(predicted$y_pred) - as.numeric(oracle$y_pred))) < 1e-12)
  archive <- tempfile("workspace é snapshot ", fileext=".n4w")
  destination <- tempfile("workspace é imported ")
  nirs4all_workspace_export(workspace, archive)
  imported <- nirs4all_import_workspace(archive, destination)
  stopifnot(length(nirs4all_workspace_predictions(imported, run)) == length(rows))
  nirs4all_workspace_close(imported)
  nirs4all_workspace_close(workspace)
  stopifnot(isTRUE(session$closed), inherits(try(nirs4all_workspace_predictions(workspace, run), silent=TRUE), "try-error"),
            inherits(try(nirs4all_workspace_predict(session, data$x, data$sample_ids), silent=TRUE), "try-error"))
  unlink(c(archive,destination), recursive=TRUE)
  cat("PASS SDK workspace R query/session/native parity/export/import/Unicode/lifecycle\n")
}
