nirs4all_dag_step_spec <- function(step) {
  spec <- unclass(step)
  if (identical(spec$kind, "concat"))
    spec$branches <- lapply(spec$branches, function(branch)
      lapply(branch, nirs4all_dag_step_spec))
  spec
}

#' Run native DAG-ML CV, refit and prediction from an R matrix
#'
#' Builds a DAG-ML campaign with explicit sample IDs and folds.
#' DAG-ML owns the FIT_CV, OOF, REFIT and PREDICT schedule; the R process
#' adapter fits the selected nirs4all learner on the requested rows. The
#' returned replay predictions are on the refit cohort, not an independent
#' external test set. A named list of pipelines activates native parameter
#' variant generation and OOF-based selection before one full-data refit. This
#' bridge does not yet expose arbitrary DAG branches, nested CV, or adaptive
#' host HPO from the high-level R API.
#'
#' @param pipeline A [nirs4all_pipeline()] using a built-in learner, or a named
#'   list of at least two such pipelines to compare by native OOF RMSE for
#'   regression or accuracy for classification.
#' @param X Finite numeric samples-by-features matrix.
#' @param y Finite numeric regression target, or factor/character class labels.
#' @param folds Number of deterministic, non-shuffled CV folds.
#' @param sample_ids Optional unique sample IDs. Defaults to matrix row names
#'   when present, otherwise zero-padded IDs in matrix row order.
#' @param root_seed Non-negative integer DAG-ML root seed.
#' @param cli Path to an installed `dag-ml-cli` executable.
#' @param workdir New or empty directory for the native contracts and model
#'   artifact. Keep it to replay the outcome later.
#' @param process_workers Number of process adapter workers.
#' @param group_ids Optional group ID per sample, in the same order as `X`.
#'   Named vectors must have names identical to `sample_ids`. When supplied,
#'   whole groups, not individual samples, are assigned to validation folds.
#' @param split_steps If `TRUE`, run each n4m preprocessing step as a separate
#'   DAG transform node. Named variants must have the same number of steps;
#'   their step methods and parameters may differ. A single
#'   [nirs4all_concat()] step is lowered to parallel branch transforms and a
#'   feature-join node; it cannot yet be combined with variants or other steps.
#' @return Native DAG-ML outcome with an additional `workdir` path.
#' @export
nirs4all_dag_cv_refit_predict <- function(
    pipeline, X, y = NULL, folds = 5L, sample_ids = NULL, root_seed = 1L,
    cli = Sys.which("dag-ml-cli"), workdir = tempfile("nirs4all-dag-"),
    process_workers = 1L, group_ids = NULL, split_steps = FALSE) {
  if (!requireNamespace("dagml", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE) ||
      !requireNamespace("digest", quietly = TRUE))
    stop("Native DAG execution requires dagml, jsonlite and digest", call. = FALSE)
  variants <- NULL
  if (inherits(pipeline, "nirs4all_pipeline")) {
    pipelines <- list(pipeline)
  } else if (is.list(pipeline) && length(pipeline) >= 2L &&
             !is.null(names(pipeline)) && !anyNA(names(pipeline)) &&
             !anyDuplicated(names(pipeline)) &&
             all(grepl("^[A-Za-z][A-Za-z0-9_.-]*$", names(pipeline)))) {
    pipelines <- pipeline
    variants <- names(pipeline)
  } else {
    stop("pipeline must be one built-in pipeline or a named list of variants",
         call. = FALSE)
  }
  if (!all(vapply(pipelines, function(value)
      inherits(value, "nirs4all_pipeline") && !is.null(value$learner$spec),
      logical(1))))
    stop("native DAG execution requires built-in learners in every pipeline",
         call. = FALSE)
  tasks <- vapply(pipelines, function(value) {
    task <- value$learner$task
    if (is.null(task)) "regression" else task
  }, character(1))
  if (length(unique(tasks)) != 1L)
    stop("all native DAG variants must have the same task", call. = FALSE)
  classification <- identical(tasks[[1L]], "classification")
  if (!is.logical(split_steps) || length(split_steps) != 1L || is.na(split_steps))
    stop("split_steps must be TRUE or FALSE", call. = FALSE)
  if (split_steps && !is.null(variants) &&
      length(unique(vapply(pipelines, function(value) length(value$steps),
                           integer(1)))) != 1L)
    stop("split_steps variants must have the same number of preprocessing steps",
         call. = FALSE)
  has_concat <- any(vapply(pipelines, function(value)
    any(vapply(value$steps, function(step) identical(step$kind, "concat"), logical(1))),
    logical(1)))
  if (split_steps && has_concat &&
      (!is.null(variants) || length(pipelines[[1L]]$steps) != 1L))
    stop("split_steps concat currently requires one pipeline with concat as its sole step",
         call. = FALSE)
  if (split_steps && has_concat &&
      !all(grepl("^[A-Za-z][A-Za-z0-9_]*$",
                 names(pipelines[[1L]]$steps[[1L]]$branches))))
    stop("split_steps concat branch names must use letters, numbers and underscores",
         call. = FALSE)
  if (inherits(X, "nirs4all_dataset")) {
    if (!is.null(y)) stop("y must come from the nirs4all_dataset", call. = FALSE)
    if (!is.null(sample_ids) && !identical(sample_ids, X$sample_ids))
      stop("sample_ids and formats sample IDs differ", call. = FALSE)
    sample_ids <- X$sample_ids
    y <- X$y
    X <- X$X
  }
  X <- nirs4all_matrix(X)
  y_names <- names(y)
  class_levels <- NULL
  if (classification) {
    if (is.character(y)) y <- factor(y)
    if (!is.factor(y) || is.ordered(y) || is.matrix(y) ||
        length(y) != nrow(X) || anyNA(y) || nlevels(y) < 2L ||
        any(tabulate(as.integer(y), nbins = nlevels(y)) == 0L))
      stop("classification y must have at least two observed classes", call. = FALSE)
    class_levels <- levels(y)
    y <- as.numeric(y) - 1
  } else if (!is.numeric(y) || is.matrix(y) || length(y) != nrow(X) ||
             anyNA(y) || any(!is.finite(y))) {
    stop("y must be one finite numeric value per row of X", call. = FALSE)
  }
  if (!is.numeric(folds) || length(folds) != 1L || !is.finite(folds) ||
      folds < 2L || folds > nrow(X) || folds != floor(folds))
    stop("folds must be an integer from two to the sample count", call. = FALSE)
  if (!is.numeric(root_seed) || length(root_seed) != 1L ||
      !is.finite(root_seed) || root_seed < 0 ||
      root_seed > .Machine$integer.max || root_seed != floor(root_seed))
    stop("root_seed must be a non-negative integer", call. = FALSE)
  if (!is.numeric(process_workers) || length(process_workers) != 1L ||
      !is.finite(process_workers) || process_workers < 1L ||
      process_workers > .Machine$integer.max || process_workers != floor(process_workers))
    stop("process_workers must be a positive integer", call. = FALSE)
  if (is.null(sample_ids)) sample_ids <- rownames(X)
  if (is.null(sample_ids)) sample_ids <- sprintf("sample:%08d", seq_len(nrow(X)))
  if (!is.character(sample_ids) || length(sample_ids) != nrow(X) ||
      anyNA(sample_ids) || any(!nzchar(trimws(sample_ids))) ||
      anyDuplicated(sample_ids))
    stop("sample_ids must be unique, non-empty strings aligned to X", call. = FALSE)
  if (!is.null(rownames(X)) && !identical(rownames(X), sample_ids))
    stop("sample_ids and X row names differ", call. = FALSE)
  if (!is.null(y_names) && !identical(y_names, sample_ids))
    stop("sample_ids and y names differ", call. = FALSE)
  if (is.factor(group_ids)) group_ids <- as.character(group_ids)
  if (!is.null(group_ids)) {
    if (!is.character(group_ids) || length(group_ids) != nrow(X) ||
        anyNA(group_ids) || any(!nzchar(trimws(group_ids))) ||
        (!is.null(names(group_ids)) &&
         !identical(names(group_ids), sample_ids)))
      stop("group_ids must be non-empty strings aligned to sample_ids",
           call. = FALSE)
    if (length(unique(group_ids)) < folds)
      stop("grouped CV needs at least one distinct group per fold",
           call. = FALSE)
    group_ids <- unname(group_ids)
  }
  if (!is.character(cli) || length(cli) != 1L || is.na(cli) || !nzchar(cli))
    stop("dag-ml-cli is required for native DAG execution", call. = FALSE)
  if (!is.character(workdir) || length(workdir) != 1L || is.na(workdir) ||
      !nzchar(workdir)) stop("workdir must be a directory path", call. = FALSE)
  if (dir.exists(workdir) && length(list.files(workdir, all.files = TRUE,
                                               no.. = TRUE)))
    stop("workdir must be empty", call. = FALSE)
  if (!dir.exists(workdir)) dir.create(workdir, recursive = TRUE)
  workdir <- normalizePath(workdir, mustWork = TRUE)

  empty <- structure(list(), names = character())
  leakage <- list(split_unit = if (is.null(group_ids)) "sample" else "group",
                  forbid_origin_cross_fold = TRUE,
                  allow_observation_split_with_shared_target = FALSE,
                  require_group_ids = !is.null(group_ids), unsafe_flags = list())
  indices <- seq_len(nrow(X))
  fold_number <- if (is.null(group_ids)) {
    (indices - 1L) %% as.integer(folds) + 1L
  } else {
    group_sizes <- table(group_ids)
    ordered_groups <- names(group_sizes)[order(-as.integer(group_sizes),
                                                names(group_sizes))]
    assigned <- stats::setNames(integer(length(ordered_groups)), ordered_groups)
    fold_load <- integer(as.integer(folds))
    for (group in ordered_groups) {
      fold <- which.min(fold_load)
      assigned[[group]] <- fold
      fold_load[[fold]] <- fold_load[[fold]] + as.integer(group_sizes[[group]])
    }
    unname(as.integer(assigned[group_ids]))
  }
  if (classification && any(vapply(seq_len(as.integer(folds)), function(index)
      length(unique(y[fold_number != index])) != length(class_levels), logical(1))))
    stop("each classification training fold must contain every class", call. = FALSE)
  fold_set <- list(
    id = "folds:nirs4all-r", sample_ids = as.list(sort(sample_ids)),
    sample_groups = if (is.null(group_ids)) empty else
      as.list(stats::setNames(group_ids, sample_ids)),
    folds = lapply(seq_len(as.integer(folds)), function(index) {
      validation_rows <- indices[fold_number == index]
      train_rows <- indices[!indices %in% validation_rows]
      list(fold_id = paste0("fold:", index - 1L),
           train_sample_ids = as.list(sort(sample_ids[train_rows])),
           validation_sample_ids = as.list(sort(sample_ids[validation_rows])),
           metadata = empty)
    }))
  fingerprints <- nirs4all_dag_fingerprints(X, y, sample_ids, group_ids,
                                            class_levels)
  data_content_fingerprint <- fingerprints$data_content_fingerprint
  relations <- list(records = lapply(seq_along(sample_ids), function(index) {
    record <- list(observation_id = paste0("observation:", index),
         sample_id = sample_ids[[index]],
         target_id = paste0("target:", index), source_id = "r_matrix")
    if (!is.null(group_ids)) record$group_id <- group_ids[[index]]
    record
  }))
  envelope <- list(schema_version = 1L,
                   schema_fingerprint = fingerprints$schema_fingerprint,
                   plan_fingerprint = fingerprints$plan_fingerprint,
                   data_content_fingerprint = fingerprints$data_content_fingerprint,
                   target_content_fingerprint = fingerprints$target_content_fingerprint,
                   coordinator_relations = relations)
  model_specs <- list()
  model_params <- lapply(pipelines, function(value) {
    params <- value$learner$spec
    if (is.character(params$learner) && length(params$learner) == 1L &&
        params$learner %in% c("parsnip", "parsnip_classifier", "mlr3",
                             "mlr3_classifier", "torch_module")) {
      spec_bytes <- serialize(value$learner$model_spec, NULL, version = 3L)
      key <- digest::digest(spec_bytes, algo = "sha256", serialize = FALSE)
      model_specs[[key]] <<- spec_bytes
      params$spec_key <- key
    }
    params$preprocessing <- if (split_steps) list() else
      lapply(value$steps, nirs4all_dag_step_spec)
    params
  })
  transform_steps <- if (split_steps) lapply(seq_along(pipelines[[1L]]$steps),
    function(index) {
      step <- pipelines[[1L]]$steps[[index]]
      node_id <- sprintf("transform:nirs4all-r:%03d", index)
      if (identical(step$kind, "concat"))
        return(list(kind = "concat_transform", id = node_id,
          branches = lapply(names(step$branches), function(name) list(
            id = name,
            steps = lapply(seq_along(step$branches[[name]]), function(position)
              list(id = sprintf("%s:%s:%03d", node_id, name, position),
                   operator = list(type = "Nirs4allRPreprocess"),
                   params = list(preprocessing = list(nirs4all_dag_step_spec(
                     step$branches[[name]][[position]]))))))),
          metadata = list(merge_mode = "concat")))
      list(kind = "transform", id = node_id,
        operator = list(type = "Nirs4allRPreprocess"),
        params = list(preprocessing = list(nirs4all_dag_step_spec(step))))
    })
    else list()
  dsl <- list(id = "dsl:nirs4all-r", campaign_id = "campaign:nirs4all-r",
              root_seed = as.integer(root_seed), leakage_policy = leakage,
              split_invocation = list(id = "split:outer", controller_id = NULL,
                                      leakage_policy = leakage,
                                      params = list(kind = "kfold",
                                                    n_splits = as.integer(folds),
                                                    shuffle = FALSE),
                                      fold_set = fold_set),
              steps = c(transform_steps, list(list(kind = "model", id = "model:nirs4all-r",
                                operator = list(type = "Nirs4allR"),
                                params = model_params[[1L]]))))
  if (!is.null(variants)) {
    dsl$max_variants <- length(variants)
    dsl$generation_dimensions <- list(list(
      name = "nirs4all_r_pipeline",
      choices = lapply(seq_along(variants), function(index) {
        overrides <- if (split_steps) lapply(seq_along(pipelines[[index]]$steps),
          function(step_index) list(
            node_id = sprintf("transform:nirs4all-r:%03d", step_index),
            params = list(preprocessing = list(
              nirs4all_dag_step_spec(
                pipelines[[index]]$steps[[step_index]]))))) else list()
        overrides[[length(overrides) + 1L]] <- list(
          node_id = "model:nirs4all-r", params = model_params[[index]])
        list(label = variants[[index]], param_overrides = overrides)
      })))
  }
  port <- list(name = "oof", kind = "prediction", representation = NULL,
               cardinality = "one", description = "")
  controller <- list(
    controller_id = "controller:nirs4all-r",
    controller_version = as.character(utils::packageVersion("nirs4all")),
    operator_kind = "model", priority = 0L,
    supported_phases = list("FIT_CV", "REFIT", "PREDICT"),
    input_ports = list(), output_ports = list(port), data_requirements = NULL,
    capabilities = list("deterministic", "process_safe", "emits_predictions",
                        "emits_artifacts", "stateful"),
    fit_scope = "fold_train", rng_policy = "externally_deterministic",
    artifact_policy = "serializable")
  controllers <- list(controller)
  if (length(transform_steps)) {
    data_port <- function(name) list(name = name, kind = "data",
      representation = "tabular_numeric", cardinality = "one", description = "")
    controllers[[2L]] <- list(
      controller_id = "controller:nirs4all-r-transform",
      controller_version = as.character(utils::packageVersion("nirs4all")),
      operator_kind = "transform", priority = 0L,
      supported_phases = list("FIT_CV", "REFIT", "PREDICT"),
      input_ports = list(data_port("x")),
      output_ports = list(data_port("x_out")), data_requirements = NULL,
      capabilities = list("deterministic", "process_safe", "emits_artifacts", "stateful"),
      fit_scope = "fold_train", rng_policy = "externally_deterministic",
      artifact_policy = "serializable")
    controllers[[1L]]$input_ports <- list(data_port("x"))
    if (has_concat) controllers[[3L]] <- list(
      controller_id = "controller:nirs4all-r-concat",
      controller_version = as.character(utils::packageVersion("nirs4all")),
      operator_kind = "feature_join", priority = 0L,
      supported_phases = list("FIT_CV", "REFIT", "PREDICT"),
      input_ports = list(), output_ports = list(data_port("x_out")),
      data_requirements = NULL,
      capabilities = list("deterministic", "process_safe", "emits_artifacts", "stateful"),
      fit_scope = "fold_train", rng_policy = "externally_deterministic",
      artifact_policy = "serializable")
  }
  write_json <- function(filename, value) {
    path <- file.path(workdir, filename)
    jsonlite::write_json(value, path, auto_unbox = TRUE, null = "null",
                         digits = 17, pretty = TRUE)
    path
  }
  data_path <- file.path(workdir, "data.rds")
  saveRDS(list(X = X, y = as.numeric(y), class_levels = class_levels,
               sample_ids = sample_ids,
               group_ids = group_ids,
               model_specs = model_specs), data_path)
  dsl_path <- write_json("dsl.json", dsl)
  controllers_path <- write_json("controllers.json", controllers)
  envelope_path <- write_json("envelope.json", envelope)
  artifact_dir <- file.path(workdir, "artifacts")
  dir.create(artifact_dir)
  adapter_source <- system.file("adapters", "nirs4all_dag_adapter.R",
                                package = "nirs4all", mustWork = TRUE)
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows")
    "Rscript.exe" else "Rscript")
  if (!file.exists(rscript)) stop("Rscript executable is unavailable", call. = FALSE)
  adapter <- file.path(workdir, if (.Platform$OS.type == "windows")
    "run-r-adapter.cmd" else "run-r-adapter")
  if (.Platform$OS.type == "windows") {
    writeLines(c("@echo off",
                 paste0(shQuote(rscript, type = "cmd"), " ",
                        shQuote(adapter_source, type = "cmd"), " %*")), adapter)
  } else {
    writeLines(c("#!/bin/sh",
                 paste0("exec ", shQuote(rscript), " ",
                        shQuote(adapter_source), " \"$@\"")), adapter)
    Sys.chmod(adapter, "0755")
  }
  description <- suppressWarnings(system2(adapter, "--describe",
                                           stdout = TRUE, stderr = TRUE))
  description_status <- attr(description, "status")
  if (!is.null(description_status) && description_status != 0L)
    stop("R DAG adapter handshake failed: ", paste(description, collapse = "\n"),
         call. = FALSE)
  variables <- c("NIRS4ALL_DAG_DATA_RDS", "NIRS4ALL_DAG_DSL",
                 "NIRS4ALL_DAG_ENVELOPE", "NIRS4ALL_DAG_ARTIFACT_DIR")
  previous <- Sys.getenv(variables, unset = NA_character_)
  on.exit(for (index in seq_along(variables)) {
    if (is.na(previous[[index]])) Sys.unsetenv(variables[[index]])
    else do.call(Sys.setenv,
                 stats::setNames(list(previous[[index]]), variables[[index]]))
  }, add = TRUE)
  do.call(Sys.setenv, stats::setNames(
    as.list(c(data_path, dsl_path, envelope_path, artifact_dir)), variables))
  verification <- suppressWarnings(system2(adapter, "--verify",
                                            stdout = TRUE, stderr = TRUE))
  verification_status <- attr(verification, "status")
  if (!is.null(verification_status) && verification_status != 0L)
    stop("R DAG data attestation failed: ", paste(verification, collapse = "\n"),
         call. = FALSE)
  run_key <- substr(data_content_fingerprint, 1L, 16L)
  outcome <- dagml::dagml_cv_refit_predict(
    dsl = dsl_path, controllers = controllers_path,
    envelope = envelope_path, adapter = adapter, cli = cli,
    output = file.path(workdir, "outcome.json"),
    process_workers = as.integer(process_workers),
    process_timeout_ms = 120000L,
    bundle_id = paste0("bundle:nirs4all-r:", run_key),
    plan_id = paste0("plan:nirs4all-r:", run_key),
    run_id = paste0("run:nirs4all-r:", run_key),
    root_seed = as.integer(root_seed),
    selection_metric = if (classification) "accuracy" else "rmse")
  outcome$workdir <- workdir
  outcome
}

#' Predict new samples with a native DAG-ML refit artifact
#'
#' Loads the winning R model and any preceding transform sidecars recorded by
#' a completed native campaign. Each sidecar is checked against its SHA-256 fingerprint
#' before loading. This is local R inference from a DAG-ML-selected model;
#' it does not run a new DAG-ML PREDICT phase or score the new cohort.
#' Only load outcomes and RDS artifacts from trusted sources.
#'
#' @param outcome Result of [nirs4all_dag_cv_refit_predict()]. Its persistent
#'   `workdir` and refit artifact must still exist.
#' @param X Finite numeric matrix or a [nirs4all_from_formats()] dataset.
#' @return Numeric regression predictions or factor class predictions,
#'   in input row order.
#' @export
nirs4all_dag_predict <- function(outcome, X) {
  if (!requireNamespace("digest", quietly = TRUE))
    stop("Native DAG artifact verification requires digest", call. = FALSE)
  if (!is.list(outcome) || !is.list(outcome$bundle) ||
      !is.character(outcome$workdir) || length(outcome$workdir) != 1L ||
      is.na(outcome$workdir) || !dir.exists(outcome$workdir))
    stop("outcome must be a persisted native nirs4all DAG result", call. = FALSE)
  records <- outcome$bundle$refit_artifacts
  if (!is.list(records) || !length(records) ||
      !all(vapply(records, function(record) is.list(record$artifact), logical(1))))
    stop("DAG bundle has no valid refit artifacts", call. = FALSE)
  artifact_root <- normalizePath(file.path(outcome$workdir, "artifacts"),
                                 mustWork = FALSE)
  checked_path <- function(record, kind, controller) {
    artifact <- record$artifact
    path <- artifact$uri
    fingerprint <- artifact$content_fingerprint
    resolved <- if (is.character(path) && length(path) == 1L &&
                    !is.na(path) && file.exists(path))
      normalizePath(path, mustWork = TRUE) else ""
    if (!identical(artifact$kind, kind) ||
        !identical(artifact$backend, "rds") ||
        !identical(artifact$controller_id, controller) ||
        !identical(record$controller_id, controller) ||
        !startsWith(resolved, paste0(artifact_root, .Platform$file.sep)) ||
        !is.character(fingerprint) || length(fingerprint) != 1L ||
        is.na(fingerprint) || !grepl("^[a-f0-9]{64}$", fingerprint) ||
        !identical(digest::digest(resolved, algo = "sha256", file = TRUE),
                   fingerprint))
      stop("DAG refit artifact identity or content mismatch", call. = FALSE)
    resolved
  }
  model_records <- Filter(function(record)
    identical(record$node_id, "model:nirs4all-r"), records)
  if (length(model_records) != 1L)
    stop("DAG bundle must contain exactly one model refit artifact", call. = FALSE)
  model_path <- checked_path(model_records[[1L]], "nirs4all_r_model",
                             "controller:nirs4all-r")
  concat_records <- Filter(function(record)
    identical(record$node_id, "transform:nirs4all-r:001") &&
      identical(record$controller_id, "controller:nirs4all-r-concat"), records)
  if (length(concat_records)) {
    if (length(concat_records) != 1L)
      stop("DAG bundle has duplicate concat artifacts", call. = FALSE)
    path <- checked_path(concat_records[[1L]], "nirs4all_r_concat",
                         "controller:nirs4all-r-concat")
    state <- readRDS(path)
    if (!is.list(state) || length(state$steps) != 1L ||
        !identical(state$steps[[1L]]$kind, "concat") ||
        length(state$states) != 1L ||
        !identical(names(state$states[[1L]]),
                   names(state$steps[[1L]]$branches)))
      stop("DAG concat artifact state is invalid", call. = FALSE)
    child_ids <- unlist(lapply(names(state$steps[[1L]]$branches), function(name)
      sprintf("transform:nirs4all-r:001:%s:%03d", name,
              seq_along(state$steps[[1L]]$branches[[name]]))),
      use.names = FALSE)
    record_ids <- vapply(records, `[[`, "", "node_id")
    if (anyDuplicated(record_ids) ||
        !setequal(record_ids, c("model:nirs4all-r",
                                "transform:nirs4all-r:001", child_ids)))
      stop("DAG concat branch artifacts are incomplete", call. = FALSE)
    for (name in names(state$steps[[1L]]$branches)) {
      for (position in seq_along(state$steps[[1L]]$branches[[name]])) {
        child_id <- sprintf("transform:nirs4all-r:001:%s:%03d", name, position)
        record <- records[[match(child_id, record_ids)]]
        child_path <- checked_path(record, "nirs4all_r_transform",
                                   "controller:nirs4all-r-transform")
        child <- readRDS(child_path)
        if (!identical(child$steps,
                       list(state$steps[[1L]]$branches[[name]][[position]])) ||
            !identical(child$states[[1L]],
                       state$states[[1L]][[name]][[position]]))
          stop("DAG concat branch state differs from its refit artifact",
               call. = FALSE)
      }
    }
    if (inherits(X, "nirs4all_dataset")) X <- X$X
    X <- nirs4all_matrix(X, state$n_features)
    transformed <- nirs4all_transform(X, state$steps, state$states)
    return(nirs4all_predict(nirs4all_load(model_path), transformed))
  }
  transform_records <- Filter(function(record)
    startsWith(record$node_id, "transform:nirs4all-r:"), records)
  if (length(records) != length(transform_records) + 1L ||
      anyDuplicated(vapply(transform_records, `[[`, "", "node_id")))
    stop("DAG bundle has unexpected or duplicate refit artifacts", call. = FALSE)
  if (length(transform_records)) {
    expected_ids <- sprintf("transform:nirs4all-r:%03d",
                            seq_along(transform_records))
    ids <- vapply(transform_records, `[[`, "", "node_id")
    if (!setequal(ids, expected_ids))
      stop("DAG transform artifacts are incomplete", call. = FALSE)
    transform_records <- transform_records[match(expected_ids, ids)]
    if (inherits(X, "nirs4all_dataset")) X <- X$X
    X <- nirs4all_matrix(X)
    for (record in transform_records) {
      path <- checked_path(record, "nirs4all_r_transform",
                           "controller:nirs4all-r-transform")
      state <- readRDS(path)
      if (!is.list(state) || length(state$steps) != 1L ||
          length(state$states) != 1L || !identical(state$n_features, ncol(X)))
        stop("DAG transform state does not match prediction features", call. = FALSE)
      X <- nirs4all_transform(X, state$steps, state$states)
    }
  }
  nirs4all_predict(nirs4all_load(model_path), X)
}

nirs4all_dag_fingerprints <- function(X, y, sample_ids, group_ids = NULL,
                                     class_levels = NULL) {
  fingerprint <- function(value) digest::digest(
    jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", digits = 17),
    algo = "sha256", serialize = FALSE)
  feature_names <- colnames(X)
  if (is.null(feature_names)) feature_names <- sprintf("feature:%08d", seq_len(ncol(X)))
  data_fields <- list(sample_ids = as.list(sample_ids), rows = nrow(X),
                      cols = ncol(X), values_row_major = as.list(as.numeric(t(X))))
  if (!is.null(group_ids)) data_fields$group_ids <- as.list(group_ids)
  schema_fields <- list(
      schema = "nirs4all-r.matrix-schema.v1", representation = "tabular_numeric",
      features = as.list(feature_names), target = "y")
  if (!is.null(class_levels)) schema_fields$class_levels <- as.list(class_levels)
  list(
    schema_fingerprint = fingerprint(schema_fields),
    plan_fingerprint = fingerprint(list(
      plan = "nirs4all-r.direct-matrix.v1", source = "r_matrix",
      output = "tabular_numeric")),
    data_content_fingerprint = fingerprint(data_fields),
    target_content_fingerprint = fingerprint(list(
      sample_ids = as.list(sample_ids), targets = as.list(as.numeric(y)))))
}
