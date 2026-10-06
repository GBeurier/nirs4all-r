# Query native score reports and prediction rows without recalculating metrics.

.nirs4all_result_canonical <- function(value) {
  if (!is.list(value)) return(value)
  if (!is.null(names(value))) value <- value[order(names(value))]
  lapply(value, .nirs4all_result_canonical)
}

#' View a native workflow result
#'
#' `nirs4all_run()` results retain the native DAG outcome and its score set.
#' An experiment loaded with [nirs4all_open_experiment()] also carries the
#' validated native-results V2 prediction projection.
nirs4all_result_view <- function(object) {
  if (inherits(object, "nirs4all_result_view")) return(object)
  if (inherits(object, "nirs4all_native_workflow") || inherits(object, "nirs4all_tuning_result")) {
    cli <- if (is.null(object$cli)) Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive") else object$cli
    native <- .nirs4all_workflow_native_call("training-result-view", list(archive = object$archive), cli = cli)
    expected <- object$outcome$training_outcome$outcome_fingerprint
    if (!is.character(expected) || length(expected) != 1L || is.na(expected) ||
        !grepl("^[0-9a-f]{64}$", expected) ||
        !identical(native$training_outcome_fingerprint, expected))
      stop("native result archive differs from the object's training outcome", call. = FALSE)
    scores <- native$score_set
    winner <- native$winner_variant_id
    run_id <- native$run_id
    rows <- native$predictions
    validation_level <- "native_training_outcome_projection"
  } else if (inherits(object, "nirs4all_workflow")) {
    outcome <- object$outcome
    scores <- outcome$bundle$scores
    winner <- object$winner_variant_id
    run_id <- object$run_id
    rows <- list()
    validation_level <- "legacy_score_reports_unverified"
  } else if (is.list(object) && is.list(object$bundle)) {
    scores <- object$bundle$scores
    winner <- object$bundle$selected_variant_id
    run_id <- object$run_id
    rows <- list()
    validation_level <- "unverified_score_reports"
  } else {
    stop("object must be a native workflow or experiment result", call. = FALSE)
  }
  if (!is.list(scores) || !is.list(scores$reports) || !is.character(winner) ||
      length(winner) != 1L || is.na(winner) || !nzchar(winner))
    stop("native outcome lacks score reports or selected variant", call. = FALSE)
  structure(list(run_id = run_id, winner_variant_id = winner,
                 score_set = scores, predictions = rows,
                 validation_level = validation_level,
                 unattested_display_fields = c("dataset", "task_type")),
            class = "nirs4all_result_view")
}

#' Compare stored native candidate score reports
nirs4all_result_compare <- function(object, variant_id = NULL,
                                    partition = NULL) {
  view <- nirs4all_result_view(object)
  reports <- view$score_set$reports
  if (!is.null(variant_id) && !any(vapply(reports, function(report)
      identical(report$variant_id, variant_id), logical(1))))
    stop("unknown variant_id", call. = FALSE)
  reports <- Filter(function(report)
    (is.null(variant_id) || identical(report$variant_id, variant_id)) &&
    (is.null(partition) || identical(report$partition, partition)), reports)
  lapply(reports, function(report) {
    report$is_winner <- identical(report$variant_id, view$winner_variant_id)
    report
  })
}

#' Extract native prediction rows, preserving stable sample identity
nirs4all_result_predictions <- function(object, variant_id = NULL,
                                        partition = NULL, fold_id = NULL) {
  view <- nirs4all_result_view(object)
  variants <- unique(c(vapply(view$score_set$reports, function(report)
    if (is.null(report$variant_id)) "" else report$variant_id, character(1)),
    vapply(view$predictions, function(row) row$variant_id, character(1))))
  if (!is.null(variant_id) && !(variant_id %in% variants))
    stop("unknown variant_id", call. = FALSE)
  Filter(function(row)
    (is.null(variant_id) || identical(row$variant_id, variant_id)) &&
    (is.null(partition) || identical(row$partition, partition)) &&
    (is.null(fold_id) || identical(row$fold_id, fold_id)), view$predictions)
}

#' Reopen a cross-language experiment directory
#'
#' The source native result triple and JSON projection are verified by SHA-256.
#' R never reads a model archive as an RDS sidecar; use the Core Archive replay
#' surface for a portable Methods model.
nirs4all_open_experiment <- function(directory,
                                    cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive")) {
  if (!requireNamespace("jsonlite", quietly = TRUE) ||
      !requireNamespace("digest", quietly = TRUE))
    stop("opening experiments requires jsonlite and digest", call. = FALSE)
  directory <- normalizePath(directory, mustWork = TRUE)
  index_path <- file.path(directory, "experiment.json")
  if (nzchar(Sys.readlink(index_path)) || !file.exists(index_path))
    stop("experiment index must be a regular file", call. = FALSE)
  index <- jsonlite::fromJSON(index_path, simplifyVector = FALSE)
  if (!identical(index$schema, "nirs4all.experiment.v1") ||
      !is.character(index$run_id) || !is.character(index$winner_variant_id) ||
      !setequal(names(index$results),
                c("manifest.json", "score_set.json", "predictions.parquet")))
    stop("invalid experiment index", call. = FALSE)
  check_hash <- function(relative, expected) {
    path <- file.path(directory, relative)
    if (nzchar(Sys.readlink(path)) || !file.exists(path) || dir.exists(path) ||
        !identical(digest::digest(file = path, algo = "sha256"), expected))
      stop(paste("experiment member hash mismatch:", relative), call. = FALSE)
    path
  }
  for (name in names(index$results))
    check_hash(file.path("results", name), index$results[[name]])
  projected <- check_hash("result_view.json", index$result_view_sha256)
  view <- jsonlite::fromJSON(projected, simplifyVector = FALSE)
  manifest <- jsonlite::fromJSON(file.path(directory, "results", "manifest.json"),
                                 simplifyVector = FALSE)
  scores <- jsonlite::fromJSON(file.path(directory, "results", "score_set.json"),
                               simplifyVector = FALSE)
  if (!identical(manifest$schema_version, 2L) ||
      !identical(manifest$engine, "dag-ml") ||
      !identical(manifest$score_set_hash,
                 digest::digest(file = file.path(directory, "results", "score_set.json"),
                                algo = "sha256")) ||
      !identical(manifest$run_id, index$run_id) ||
      !identical(manifest$selected_variant_id, index$winner_variant_id) ||
      !identical(view$manifest, manifest) || !identical(view$score_set, scores) ||
      !is.list(view$predictions))
    stop("experiment native result projection is inconsistent", call. = FALSE)
  variants <- unique(c(vapply(scores$reports, function(report)
    if (is.null(report$variant_id)) "" else report$variant_id, character(1)),
    vapply(view$predictions, function(row) row$variant_id, character(1))))
  if (!(index$winner_variant_id %in% variants))
    stop("winner is absent from native results", call. = FALSE)
  for (row in view$predictions) {
    ids <- unlist(row$sample_ids, use.names = FALSE)
    positions <- unlist(row$sample_indices, use.names = FALSE)
    if (length(ids) && (length(ids) != length(positions) || anyDuplicated(ids)))
      stop("invalid native prediction sample identities", call. = FALSE)
  }
  archive <- index$model_archive
  if (!is.null(archive)) {
    if (!identical(archive$path, "model.n4a"))
      stop("invalid Core model archive reference", call. = FALSE)
    check_hash("model.n4a", archive$sha256)
  }
  # Only Core's native reader can prove that this projection is exactly the
  # Parquet result and that an archive closes over its training outcome.
  native_output <- tempfile("nirs4all-native-result-", fileext = ".json")
  native_log <- tempfile("nirs4all-native-result-", fileext = ".log")
  on.exit(unlink(c(native_output, native_log)), add = TRUE)
  status <- system2(cli, c("experiment-open", "--input", shQuote(directory),
                          "--output", shQuote(native_output)),
                    stdout = native_log, stderr = native_log)
  if (!identical(status, 0L) || !file.exists(native_output))
    stop(paste("native experiment validation failed:",
               paste(readLines(native_log, warn = FALSE), collapse = "\n")), call. = FALSE)
  native <- jsonlite::fromJSON(native_output, simplifyVector = FALSE)
  if (!identical(view$manifest, native$manifest) ||
      !identical(view$score_set, native$score_set) ||
      !identical(.nirs4all_result_canonical(view$predictions),
                 .nirs4all_result_canonical(native$predictions)))
    stop("experiment projection disagrees with native Parquet results", call. = FALSE)
  structure(list(run_id = index$run_id,
                 winner_variant_id = index$winner_variant_id,
                 score_set = scores, predictions = view$predictions,
                 validation_level = if (is.null(archive)) "native_results" else "native_results_and_model_prediction_closure",
                 unattested_display_fields = c("dataset", "task_type"),
                 model_archive = if (is.null(archive)) NULL else
                   file.path(directory, "model.n4a")),
            class = "nirs4all_result_view")
}
