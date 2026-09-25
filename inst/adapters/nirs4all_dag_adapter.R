#!/usr/bin/env Rscript
# Process adapter for real R matrices. DAG-ML owns fold and phase scheduling;
# this script owns only the selected learner's fit/predict and its sidecar.

empty_object <- structure(list(), names = character())
args <- commandArgs(trailingOnly = TRUE)
if (identical(args, "--describe")) {
  cat(jsonlite::toJSON(list(
    schema_version = 1L, protocol = "dag-ml-process-adapter",
    adapter_id = "nirs4all-r-model-adapter",
    supported_modes = list("one_shot", "jsonl"),
    capabilities = list("control_frames_v1", "node_task_json_v1",
                        "node_result_json_v1", "parallel_invocation_v1",
                        "persistent_workers", "worker_env",
                        "stateful_refit_artifacts")), auto_unbox = TRUE), "\n", sep = "")
  quit(save = "no", status = 0L)
}
suppressPackageStartupMessages(library(nirs4all))

data_path <- Sys.getenv("NIRS4ALL_DAG_DATA_RDS")
dsl_path <- Sys.getenv("NIRS4ALL_DAG_DSL")
envelope_path <- Sys.getenv("NIRS4ALL_DAG_ENVELOPE")
artifact_dir <- Sys.getenv("NIRS4ALL_DAG_ARTIFACT_DIR")
if (!nzchar(data_path) || !nzchar(dsl_path) || !nzchar(envelope_path) ||
    !nzchar(artifact_dir))
  stop("NIRS4ALL_DAG_DATA_RDS, NIRS4ALL_DAG_DSL, NIRS4ALL_DAG_ENVELOPE and NIRS4ALL_DAG_ARTIFACT_DIR are required")
data <- readRDS(data_path)
dsl <- jsonlite::fromJSON(dsl_path, simplifyVector = FALSE)
envelope <- jsonlite::fromJSON(envelope_path, simplifyVector = FALSE)
if (!is.list(data) || !is.matrix(data$X) || !is.numeric(data$X) ||
    !is.numeric(data$y) || length(data$y) != nrow(data$X) ||
    !is.character(data$sample_ids) || length(data$sample_ids) != nrow(data$X) ||
    anyNA(data$X) || any(!is.finite(data$X)) || anyNA(data$y) ||
    any(!is.finite(data$y)) || anyNA(data$sample_ids) ||
    anyDuplicated(data$sample_ids))
  stop("invalid R matrix dataset for DAG-ML adapter")
if (!is.list(dsl$split_invocation$fold_set$folds))
  stop("DAG-ML DSL has no explicit fold set")
fold_universe <- as.character(unlist(dsl$split_invocation$fold_set$sample_ids,
                                     use.names = FALSE))
relation_universe <- vapply(envelope$coordinator_relations$records,
                            function(record) record$sample_id, character(1))
if (anyDuplicated(fold_universe) || anyDuplicated(relation_universe) ||
    !setequal(fold_universe, data$sample_ids) ||
    !setequal(relation_universe, data$sample_ids))
  stop("DAG-ML fold, relation and matrix sample IDs disagree")
fingerprints <- nirs4all:::nirs4all_dag_fingerprints(
  data$X, data$y, data$sample_ids)
for (key in names(fingerprints)) {
  if (!identical(envelope[[key]], fingerprints[[key]]))
    stop(paste("DAG-ML envelope does not attest R data:", key))
}
if (identical(args, "--verify")) quit(save = "no", status = 0L)

as_ids <- function(value) as.character(unlist(value, use.names = FALSE))
sample_rows <- function(ids) {
  if (!length(ids) || anyDuplicated(ids)) stop("empty or duplicate sample IDs")
  rows <- match(ids, data$sample_ids)
  if (anyNA(rows)) stop("DAG-ML requested unknown sample IDs")
  rows
}
safe_handle <- function(value) {
  acc <- 17
  for (byte in as.integer(charToRaw(value))) acc <- (acc * 31 + byte) %% 2147483647
  as.integer(if (acc == 0) 1 else acc)
}
learner_from_task <- function(task) {
  params <- task$node_plan$params
  kind <- params$learner
  if (is.null(kind)) stop("model node requires params.learner")
  switch(kind,
    pls = nirs4all_pls(n_components = as.integer(params$n_components),
                       algo = if (is.null(params$algo)) "pls_simpls" else params$algo,
                       center_x = if (is.null(params$center_x)) TRUE else params$center_x,
                       scale_x = if (is.null(params$scale_x)) TRUE else params$scale_x,
                       center_y = if (is.null(params$center_y)) TRUE else params$center_y,
                       scale_y = if (is.null(params$scale_y)) TRUE else params$scale_y),
    n4m_method = nirs4all_n4m_method(
      method = params$method, n_components = as.integer(params$n_components),
      params = if (is.null(params$params)) list() else params$params),
    lm = nirs4all_lm(),
    ranger = do.call(nirs4all_ranger, c(
      list(num.trees = as.integer(params$num_trees),
           seed = if (is.null(params$seed)) 1L else as.integer(params$seed)),
      if (is.null(params$extra)) list() else params$extra)),
    glmnet = nirs4all_glmnet(lambda = as.numeric(params$lambda),
                            alpha = if (is.null(params$alpha)) 1 else as.numeric(params$alpha),
                            standardize = if (is.null(params$standardize)) TRUE else params$standardize),
    torch_mlp = nirs4all_torch_mlp(
      hidden = if (is.null(params$hidden)) 32L else as.integer(params$hidden),
      epochs = if (is.null(params$epochs)) 100L else as.integer(params$epochs),
      learning_rate = if (is.null(params$learning_rate)) 0.001 else as.numeric(params$learning_rate),
      seed = if (is.null(params$seed)) 1L else as.integer(params$seed)),
    stop(paste("unknown R learner:", kind)))
}
steps_from_task <- function(task) {
  specs <- task$node_plan$params$preprocessing
  if (is.null(specs)) return(list())
  lapply(specs, function(spec) {
    switch(spec$kind,
      snv = nirs4all_snv(ddof = as.integer(spec$ddof),
                        with_mean = if (is.null(spec$with_mean)) TRUE else spec$with_mean,
                        with_std = if (is.null(spec$with_std)) TRUE else spec$with_std),
      local_snv = nirs4all_local_snv(
        window = as.integer(spec$window), pad_mode = spec$pad_mode,
        constant_value = as.numeric(spec$constant_value)),
      robust_snv = nirs4all_robust_snv(
        with_center = spec$with_center, with_scale = spec$with_scale,
        k = as.numeric(spec$k)),
      area_normalization = nirs4all_area_normalization(method = spec$method),
      detrend = nirs4all_detrend(polyorder = as.integer(spec$polyorder)),
      msc = nirs4all_msc(),
      emsc = nirs4all_emsc(degree = as.integer(spec$degree)),
      savgol = nirs4all_savgol(
        window_length = as.integer(spec$window_length),
        polyorder = as.integer(spec$polyorder), deriv = as.integer(spec$deriv),
        delta = as.numeric(spec$delta), mode = spec$mode,
        cval = as.numeric(spec$cval)),
      stop(paste("unsupported preprocessing step:", spec$kind)))
  })
}
fold_ids <- function(fold_id) {
  matches <- Filter(function(fold) identical(fold$fold_id, fold_id),
                    dsl$split_invocation$fold_set$folds)
  if (length(matches) != 1L) stop("unknown DAG-ML fold ID")
  list(train = as_ids(matches[[1L]]$train_sample_ids),
       validation = as_ids(matches[[1L]]$validation_sample_ids))
}
view_ids <- function(task, partition) {
  views <- Filter(function(view) identical(view$partition, partition),
                  task$data_views)
  if (length(views) == 1L) as_ids(views[[1L]]$sample_ids) else NULL
}
artifact_location <- function(node) {
  dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(normalizePath(artifact_dir, mustWork = TRUE),
            paste0("model-", safe_handle(node), ".rds"))
}
prediction_block <- function(node, partition, fold_id, ids, values) {
  list(producer_node = node, partition = partition, fold_id = fold_id,
       sample_ids = as.list(ids),
       values = lapply(values, function(value) list(as.numeric(value))),
       target_names = list("y"))
}
target_block <- function(ids) {
  rows <- sample_rows(ids)
  list(level = "sample",
       unit_ids = lapply(ids, function(id) list(level = "sample", id = id)),
       values = lapply(data$y[rows], function(value) list(as.numeric(value))),
       target_names = list("y"))
}
record_result <- function(task, raw_line) {
  seed_tokens <- regmatches(raw_line,
    gregexpr('"seed"[[:space:]]*:[[:space:]]*[0-9]+', raw_line))[[1L]]
  if (!length(seed_tokens)) stop("NodeTask lacks an exact numeric seed")
  seed_decimal <- sub('.*:', '', tail(seed_tokens, 1L))
  phase <- task$phase
  if (!(phase %in% c("FIT_CV", "REFIT", "PREDICT")))
    stop("unsupported DAG-ML phase")
  node <- task$node_plan$node_id
  controller <- task$node_plan$controller_id
  artifact_id <- paste0("artifact:", node, ":nirs4all-r:refit")
  artifact_path <- artifact_location(node)
  all_ids <- as_ids(dsl$split_invocation$fold_set$sample_ids)
  if (identical(phase, "FIT_CV")) {
    fold <- fold_ids(task$fold_id)
    train_ids <- fold$train
    prediction_ids <- fold$validation
    partition <- "validation"
  } else if (identical(phase, "REFIT")) {
    train_ids <- view_ids(task, "full_train")
    if (is.null(train_ids)) train_ids <- all_ids
    prediction_ids <- train_ids
    partition <- "final"
  } else {
    train_ids <- NULL
    prediction_ids <- view_ids(task, "predict")
    if (is.null(prediction_ids)) prediction_ids <- all_ids
    partition <- "final"
  }
  sample_rows(prediction_ids)
  artifacts <- list()
  artifact_handles <- empty_object
  if (!identical(phase, "PREDICT")) {
    train_rows <- sample_rows(train_ids)
    learner <- learner_from_task(task)
    fitted <- nirs4all_fit(nirs4all_pipeline(steps = steps_from_task(task),
                                            learner = learner),
                          data$X[train_rows, , drop = FALSE], data$y[train_rows])
    if (identical(phase, "REFIT")) {
      nirs4all_save(fitted, artifact_path)
      artifact <- list(id = artifact_id, kind = "nirs4all_r_model",
                       controller_id = controller, backend = "rds",
                       uri = artifact_path,
                       content_fingerprint = digest::digest(artifact_path,
                                                           algo = "sha256", file = TRUE),
                       size_bytes = as.integer(file.info(artifact_path)$size),
                       plugin = "nirs4all-r", plugin_version = "0.4.0.9000")
      artifacts <- list(artifact)
      artifact_handles <- setNames(list(list(
        handle = safe_handle(artifact_id), kind = "model",
        owner_controller = controller)), artifact_id)
    }
  } else {
    inputs <- task$artifact_inputs
    expected <- Filter(function(input) identical(input$artifact$id, artifact_id), inputs)
    if (length(expected) != 1L ||
        !identical(expected[[1L]]$artifact$uri, artifact_path) ||
        !identical(expected[[1L]]$artifact$content_fingerprint,
                   digest::digest(artifact_path, algo = "sha256", file = TRUE)))
      stop("PREDICT artifact identity or content mismatch")
    fitted <- nirs4all_load(artifact_path)
  }
  predictions <- nirs4all_predict(fitted, data$X[sample_rows(prediction_ids), , drop = FALSE])
  result <- list(
    node_id = node,
    outputs = list(oof = list(handle = safe_handle(paste(node, phase, sep = ":")),
                              kind = "prediction", owner_controller = controller)),
    predictions = list(prediction_block(node, partition,
      if (identical(phase, "FIT_CV")) task$fold_id else NULL,
      prediction_ids, predictions)),
    artifacts = artifacts, artifact_handles = artifact_handles,
    lineage = list(
      record_id = paste("lineage", node, phase,
                        if (is.null(task$variant_id)) "base" else task$variant_id,
                        if (is.null(task$fold_id)) "none" else task$fold_id, sep = ":"),
      run_id = task$run_id, node_id = node, phase = phase,
      controller_id = controller,
      controller_version = task$node_plan$controller_version,
      variant_id = task$variant_id, fold_id = task$fold_id,
      branch_path = if (is.null(task$branch_path)) list() else task$branch_path,
      input_lineage = list(), artifact_refs = artifacts,
      params_fingerprint = task$node_plan$params_fingerprint,
      data_model_shape_fingerprint = NULL,
      aggregation_policy_fingerprint = NULL,
      seed = "__DAGML_U64_SEED__", unsafe_flags = list(), metrics = empty_object,
      loss_attestations = list(), early_stopping_records = list()))
  if (!identical(phase, "PREDICT"))
    result$regression_targets <- list(target_block(prediction_ids))
  encoded <- jsonlite::toJSON(result, auto_unbox = TRUE, null = "null", digits = 17)
  sub('"seed":"__DAGML_U64_SEED__"', paste0('"seed":', seed_decimal),
      encoded, fixed = TRUE)
}
emit <- function(value) {
  cat(value, "\n", sep = "")
  flush(stdout())
}
emit_ack <- function(status) emit(jsonlite::toJSON(
  list(type = "ack", schema_version = 1L, status = status), auto_unbox = TRUE))
emit_error <- function(message) emit(jsonlite::toJSON(
  list(type = "error", schema_version = 1L,
       error = list(code = "nirs4all_r_adapter_error", message = message,
                    retryable = FALSE)), auto_unbox = TRUE))
handle_line <- function(line) {
  payload <- jsonlite::fromJSON(line, simplifyVector = FALSE)
  if (is.list(payload) && is.character(payload$type) && length(payload$type) == 1L) {
    if (identical(payload$type, "init")) { emit_ack("initialized"); return(TRUE) }
    if (identical(payload$type, "close")) { emit_ack("closed"); return(FALSE) }
    if (!identical(payload$type, "task")) stop("unsupported process frame")
    result <- record_result(payload$task, line)
    emit(paste0('{"type":"result","schema_version":1,"result":', result, '}'))
  } else {
    emit(record_result(payload, line))
  }
  TRUE
}
input <- file("stdin", open = "r")
repeat {
  line <- readLines(input, n = 1L, warn = FALSE)
  if (!length(line)) break
  continued <- tryCatch(handle_line(line), error = function(error) {
    emit_error(conditionMessage(error))
    TRUE
  })
  if (!continued || !identical(args, "--jsonl")) break
}
close(input)
