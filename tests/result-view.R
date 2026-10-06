# A fixture must be produced by dag_ml.write_native_results_v2/Core native CV.
fixture <- Sys.getenv("NIRS4ALL_EXPERIMENT_FIXTURE")
if (nzchar(fixture)) {
  library(nirs4all)
  source_root <- Sys.getenv("NIRS4ALL_R_SOURCE_ROOT")
  if (nzchar(source_root)) source(file.path(source_root, "R", "result_view.R"))
  view <- nirs4all_open_experiment(fixture)
  stopifnot(identical(view$validation_level, "native_results_and_model_prediction_closure"), !is.null(view$model_archive))
  native <- jsonlite::fromJSON(file.path(fixture, "result_view.json"), simplifyVector = FALSE)
  rows <- nirs4all_result_predictions(view)
  stopifnot(identical(rows, native$predictions))
  scores <- nirs4all_result_compare(view)
  stopifnot(length(scores) == length(native$score_set$reports))
  stopifnot(identical(scores[[1L]]$metrics, native$score_set$reports[[1L]]$metrics))
  rows[[1L]]$sample_ids[[1L]] <- "changed"
  stopifnot(identical(nirs4all_result_predictions(view), native$predictions))
  stopifnot(inherits(try(nirs4all_result_predictions(view, variant_id = "absent"), silent = TRUE), "try-error"))
  stopifnot(inherits(try(nirs4all_result_compare(view, variant_id = "absent"), silent = TRUE), "try-error"))
  if (nzchar(source_root)) source(file.path(source_root, "R", "native_workflow.R"))
  original_archive <- Sys.getenv("NIRS4ALL_MODEL_FIXTURE")
  cli <- Sys.getenv("NIRS4ALL_CORE_CLI")
  native_call <- if (nzchar(source_root)) .nirs4all_workflow_native_call else
    getFromNamespace(".nirs4all_workflow_native_call", "nirs4all")
  initial_outcome <- native_call("workflow-load", list(archive = original_archive), cli = cli)
  object <- structure(list(archive = original_archive, outcome = initial_outcome,
                           cli = cli), class = "nirs4all_native_workflow")
  direct <- nirs4all_result_view(object)
  stopifnot(length(direct$predictions) == length(native$predictions),
            identical(direct$unattested_display_fields, c("dataset", "task_type")))
  for (i in seq_along(native$predictions)) {
    for (field in c("sample_ids", "sample_indices", "y_true", "y_pred", "y_pred_shape", "variant_id", "partition"))
      stopifnot(identical(direct$predictions[[i]][[field]], native$predictions[[i]][[field]]))
  }
  stopifnot(identical(direct$validation_level, "native_training_outcome_projection"))
  legacy <- structure(list(outcome = list(bundle = list(scores = native$score_set)),
      winner_variant_id = view$winner_variant_id, run_id = view$run_id), class = "nirs4all_workflow")
  bare <- list(bundle = list(scores = native$score_set, selected_variant_id = view$winner_variant_id),
               run_id = view$run_id)
  stopifnot(identical(nirs4all_result_view(legacy)$validation_level, "legacy_score_reports_unverified"),
            identical(nirs4all_result_view(bare)$validation_level, "unverified_score_reports"))
  # Substitute another fully valid native archive; invalid ZIP/hash rejection
  # would not prove binding to the original result object's identity.
  stopifnot(nzchar(Sys.getenv("NIRS4ALL_TUNING_DATASET")))
  if (nzchar(source_root)) source(file.path(source_root, "R", "tuning.R"))
  substitute_archive <- tempfile("nirs4all-result-substitute-", fileext = ".n4a")
  data <- jsonlite::fromJSON(Sys.getenv("NIRS4ALL_TUNING_DATASET"), simplifyVector = FALSE)
  substitute <- nirs4all_tune(data, trials = 1L, archive = substitute_archive)
  stopifnot(!identical(substitute$outcome$training_outcome$outcome_fingerprint,
                      initial_outcome$training_outcome$outcome_fingerprint))
  stopifnot(length(nirs4all_result_view(substitute)$predictions) > 0L)
  changed <- object
  changed$archive <- substitute_archive
  rejected <- try(nirs4all_result_view(changed), silent = TRUE)
  stopifnot(inherits(rejected, "try-error"), grepl("differs from the object's training outcome", as.character(rejected), fixed = TRUE))
  cat("Native R valid-archive substitution rejected:",
      initial_outcome$training_outcome$outcome_fingerprint, "!=",
      substitute$outcome$training_outcome$outcome_fingerprint, "\n")
  unlink(c(substitute_archive, paste0(substitute_archive, ".results")), recursive = TRUE)
  temporary <- tempfile("nirs4all-plain-result-")
  plain <- nirs4all_save_experiment(file.path(fixture, "results"), temporary)
  stopifnot(identical(plain$validation_level, "native_results"), is.null(plain$model_archive))
  unlink(temporary, recursive = TRUE)
  cat("Native R result view: PASS\n")
} else {
  cat("Native R result fixture not requested; skipped\n")
}
