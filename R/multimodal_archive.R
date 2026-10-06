#' Replay a native raw multimodal archive on an independent public dataset
#'
#' Native IO validates raw sources; DAG creates the request and independently
#' trusted controller profile. The persistent R adapter only hydrates and
#' predicts through installed Methods. No training workspace or Python is used.
#' @export
nirs4all_multimodal_archive_predict <- function(path, dataset,
    core_cli = Sys.which("nirs4all-core-archive"), dag_cli = Sys.which("dag-ml-cli"),
    workdir = tempfile("nirs4all-raw-archive-"),
    adapter_script = system.file("adapters", "nirs4all_core_multimodal_adapter.R", package = "nirs4all")) {
  if (missing(core_cli) && inherits(dataset, "nirs4all_dataset")) core_cli <- dataset$core_cli
  ds <- nirs4all_dataset(dataset, core_cli)
  archive <- nirs4all_core_archive(path, ds$core_cli)
  workdir <- .nirs4all_core_workdir(workdir)
  input <- .nirs4all_core_contract(ds$record, file.path(workdir, "dataset.json"), "public dataset")
  prepared_path <- file.path(workdir, "prepared.json")
  native_dir <- file.path(workdir, "native-inputs")
  .nirs4all_core_run(ds$core_cli, c("multimodal-replay-inputs", "--archive", archive$path,
    "--input", input, "--output", prepared_path, "--workdir", native_dir), workdir)
  prepared <- jsonlite::fromJSON(prepared_path, simplifyVector = FALSE)
  adapter_script <- .nirs4all_core_path(adapter_script, "Installed raw R adapter")
  rscript <- .nirs4all_core_path(file.path(R.home("bin"), "Rscript"), "Rscript", executable = TRUE)
  adapter <- file.path(workdir, "run-r-adapter")
  writeLines(c("#!/bin/sh", paste0("export DAGML_METHODS_MULTIMODAL_CONFIG=", shQuote(file.path(native_dir, "config.json"))),
    paste0("exec ", shQuote(rscript), " ", shQuote(adapter_script), " \"$@\"")), adapter)
  Sys.chmod(adapter, "0755")
  outcome <- nirs4all_core_archive_predict(archive, file.path(native_dir, "request.json"),
    file.path(native_dir, "envelopes.json"), adapter,
    file.path(native_dir, "trusted-controllers.json"), dag_cli, file.path(workdir, "replay"))
  if (length(outcome$outputs) != 1L || length(outcome$outputs[[1L]]$predictions) != 1L)
    stop("Raw archive replay requires one complete native sample prediction", call. = FALSE)
  block <- outcome$outputs[[1L]]$predictions[[1L]]
  expected_ids <- unlist(prepared$sample_ids, use.names = FALSE)
  actual_ids <- unlist(block$sample_ids, use.names = FALSE)
  if (anyDuplicated(actual_ids) || !setequal(expected_ids, actual_ids))
    stop("Native prediction identity coverage differs", call. = FALSE)
  positions <- match(expected_ids, actual_ids)
  values <- vapply(block$values, function(row) {
    value <- unlist(row); if (length(value) != 1L || !is.numeric(value) || !is.finite(value))
      stop("Finite scalar native predictions required", call. = FALSE)
    as.double(value)
  }, double(1))
  audit <- lapply(readLines(file.path(native_dir, "lifecycle.jsonl"), warn = FALSE), function(line)
    jsonlite::fromJSON(line, simplifyVector = FALSE))
  if (any(vapply(audit, function(event) event$operation %in% c("FIT_CV", "REFIT", "fit"), logical(1))))
    stop("Cold native replay unexpectedly performed training", call. = FALSE)
  list(sample_ids = expected_ids, target_names = unlist(block$target_names),
       values = matrix(values[positions], ncol = 1L), training_performed = FALSE,
       outcome = outcome, audit = audit, archive_sha256 = archive$archive$archive_sha256)
}
