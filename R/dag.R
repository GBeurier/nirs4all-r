#' Run native DAG-ML CV, refit and prediction from an R matrix
#'
#' Builds a single-node DAG-ML campaign with explicit sample IDs and folds.
#' DAG-ML owns the FIT_CV, OOF, REFIT and PREDICT schedule; the R process
#' adapter fits the selected nirs4all learner on the requested rows. The
#' returned replay predictions are on the refit cohort, not an independent
#' external test set. A named list of pipelines activates native parameter
#' variant generation and OOF-based selection before one full-data refit. This
#' bridge does not yet expose arbitrary DAG branches, nested CV, or adaptive
#' host HPO from the high-level R API.
#'
#' @param pipeline A [nirs4all_pipeline()] using a built-in learner, or a named
#'   list of at least two such pipelines to compare by native OOF RMSE.
#' @param X Finite numeric samples-by-features matrix.
#' @param y Finite numeric target vector.
#' @param folds Number of deterministic, non-shuffled CV folds.
#' @param sample_ids Optional unique sample IDs. Defaults to matrix row names
#'   when present, otherwise zero-padded IDs in matrix row order.
#' @param root_seed Non-negative integer DAG-ML root seed.
#' @param cli Path to an installed `dag-ml-cli` executable.
#' @param workdir New or empty directory for the native contracts and model
#'   artifact. Keep it to replay the outcome later.
#' @param process_workers Number of process adapter workers.
#' @return Native DAG-ML outcome with an additional `workdir` path.
#' @export
nirs4all_dag_cv_refit_predict <- function(
    pipeline, X, y = NULL, folds = 5L, sample_ids = NULL, root_seed = 1L,
    cli = Sys.which("dag-ml-cli"), workdir = tempfile("nirs4all-dag-"),
    process_workers = 1L) {
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
  if (inherits(X, "nirs4all_dataset")) {
    if (!is.null(y)) stop("y must come from the nirs4all_dataset", call. = FALSE)
    if (!is.null(sample_ids) && !identical(sample_ids, X$sample_ids))
      stop("sample_ids and formats sample IDs differ", call. = FALSE)
    sample_ids <- X$sample_ids
    y <- X$y
    X <- X$X
  }
  X <- nirs4all_matrix(X)
  if (!is.numeric(y) || is.matrix(y) || length(y) != nrow(X) ||
      anyNA(y) || any(!is.finite(y)))
    stop("y must be one finite numeric value per row of X", call. = FALSE)
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
  if (!is.null(names(y)) && !identical(names(y), sample_ids))
    stop("sample_ids and y names differ", call. = FALSE)
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
  leakage <- list(split_unit = "sample", forbid_origin_cross_fold = TRUE,
                  allow_observation_split_with_shared_target = FALSE,
                  require_group_ids = FALSE, unsafe_flags = list())
  indices <- seq_len(nrow(X))
  fold_set <- list(
    id = "folds:nirs4all-r", sample_ids = as.list(sort(sample_ids)),
    sample_groups = empty,
    folds = lapply(seq_len(as.integer(folds)), function(index) {
      validation_rows <- indices[(indices - 1L) %% as.integer(folds) == index - 1L]
      train_rows <- indices[!indices %in% validation_rows]
      list(fold_id = paste0("fold:", index - 1L),
           train_sample_ids = as.list(sort(sample_ids[train_rows])),
           validation_sample_ids = as.list(sort(sample_ids[validation_rows])),
           metadata = empty)
    }))
  fingerprints <- nirs4all_dag_fingerprints(X, y, sample_ids)
  data_content_fingerprint <- fingerprints$data_content_fingerprint
  relations <- list(records = lapply(seq_along(sample_ids), function(index) {
    list(observation_id = paste0("observation:", index),
         sample_id = sample_ids[[index]],
         target_id = paste0("target:", index), source_id = "r_matrix")
  }))
  envelope <- list(schema_version = 1L,
                   schema_fingerprint = fingerprints$schema_fingerprint,
                   plan_fingerprint = fingerprints$plan_fingerprint,
                   data_content_fingerprint = fingerprints$data_content_fingerprint,
                   target_content_fingerprint = fingerprints$target_content_fingerprint,
                   coordinator_relations = relations)
  model_params <- lapply(pipelines, function(value) {
    params <- value$learner$spec
    params$preprocessing <- lapply(value$steps, unclass)
    params
  })
  dsl <- list(id = "dsl:nirs4all-r", campaign_id = "campaign:nirs4all-r",
              root_seed = as.integer(root_seed), leakage_policy = leakage,
              split_invocation = list(id = "split:outer", controller_id = NULL,
                                      leakage_policy = leakage,
                                      params = list(kind = "kfold",
                                                    n_splits = as.integer(folds),
                                                    shuffle = FALSE),
                                      fold_set = fold_set),
              steps = list(list(kind = "model", id = "model:nirs4all-r",
                                operator = list(type = "Nirs4allR"),
                                params = model_params[[1L]])))
  if (!is.null(variants)) {
    dsl$max_variants <- length(variants)
    dsl$generation_dimensions <- list(list(
      name = "nirs4all_r_pipeline",
      choices = lapply(seq_along(variants), function(index) list(
        label = variants[[index]],
        param_overrides = list(list(node_id = "model:nirs4all-r",
                                    params = model_params[[index]]))))))
  }
  port <- list(name = "oof", kind = "prediction", representation = NULL,
               cardinality = "one", description = "")
  controller <- list(
    controller_id = "controller:nirs4all-r", controller_version = "0.4.0",
    operator_kind = "model", priority = 0L,
    supported_phases = list("FIT_CV", "REFIT", "PREDICT"),
    input_ports = list(), output_ports = list(port), data_requirements = NULL,
    capabilities = list("deterministic", "process_safe", "emits_predictions",
                        "emits_artifacts", "stateful"),
    fit_scope = "fold_train", rng_policy = "externally_deterministic",
    artifact_policy = "serializable")
  write_json <- function(filename, value) {
    path <- file.path(workdir, filename)
    jsonlite::write_json(value, path, auto_unbox = TRUE, null = "null",
                         digits = 17, pretty = TRUE)
    path
  }
  data_path <- file.path(workdir, "data.rds")
  saveRDS(list(X = X, y = as.numeric(y), sample_ids = sample_ids), data_path)
  dsl_path <- write_json("dsl.json", dsl)
  controllers_path <- write_json("controllers.json", list(controller))
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
    root_seed = as.integer(root_seed))
  outcome$workdir <- workdir
  outcome
}

nirs4all_dag_fingerprints <- function(X, y, sample_ids) {
  fingerprint <- function(value) digest::digest(
    jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", digits = 17),
    algo = "sha256", serialize = FALSE)
  feature_names <- colnames(X)
  if (is.null(feature_names)) feature_names <- sprintf("feature:%08d", seq_len(ncol(X)))
  list(
    schema_fingerprint = fingerprint(list(
      schema = "nirs4all-r.matrix-schema.v1", representation = "tabular_numeric",
      features = as.list(feature_names), target = "y")),
    plan_fingerprint = fingerprint(list(
      plan = "nirs4all-r.direct-matrix.v1", source = "r_matrix",
      output = "tabular_numeric")),
    data_content_fingerprint = fingerprint(list(
      sample_ids = as.list(sample_ids), rows = nrow(X), cols = ncol(X),
      values_row_major = as.list(as.numeric(t(X))))),
    target_content_fingerprint = fingerprint(list(
      sample_ids = as.list(sample_ids), targets = as.list(as.numeric(y)))))
}
