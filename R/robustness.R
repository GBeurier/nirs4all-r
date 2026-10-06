nirs4all_robustness <- function(model, X, y, sample_ids = NULL, scenarios = NULL,
                                methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
                                cli = Sys.getenv("NIRS4ALL_CORE_CLI"), source_id = NULL) {
  if (missing(cli) && is.list(model) && !is.null(model$cli)) cli <- model$cli
  archive <- if (is.character(model)) model else model$archive
  if (is.null(scenarios)) scenarios <- list(
    list(id = "observed", kind = "observed", severity = 0, seed = 0L),
    list(id = "gaussian", kind = "spectral_noise", severity = 0.01, seed = 1L))
  record <- .nirs4all_uncertainty_input(X, sample_ids, source_id, cli)
  record$truth <- .nirs4all_uncertainty_rows(y)
  record$scenarios <- scenarios
  .nirs4all_workflow_native_call("robustness", list(
    archive = normalizePath(archive, mustWork = TRUE),
    methods_library = normalizePath(methods_library, mustWork = TRUE),
    run_id = paste0("run:robustness:", basename(tempfile()))),
    record = record, cli = cli)
}
