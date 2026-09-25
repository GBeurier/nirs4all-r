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
if (!is.null(data$class_levels) &&
    (!is.character(data$class_levels) || length(data$class_levels) < 2L ||
     anyNA(data$class_levels) || anyDuplicated(data$class_levels) ||
     any(!nzchar(data$class_levels)) ||
     any(!(data$y %in% (seq_along(data$class_levels) - 1L)))))
  stop("invalid encoded classification labels for DAG-ML adapter")
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
if (!is.null(data$group_ids)) {
  if (!is.character(data$group_ids) ||
      length(data$group_ids) != length(data$sample_ids) ||
      anyNA(data$group_ids) || any(!nzchar(trimws(data$group_ids))))
    stop("invalid R group IDs for DAG-ML adapter")
  expected_groups <- stats::setNames(data$group_ids, data$sample_ids)
  fold_groups <- dsl$split_invocation$fold_set$sample_groups
  relation_groups <- stats::setNames(vapply(
    envelope$coordinator_relations$records,
    function(record) record$group_id, character(1)), relation_universe)
  if (!is.list(fold_groups) ||
      !setequal(names(fold_groups), data$sample_ids) ||
      !identical(unname(vapply(fold_groups[data$sample_ids], `[[`, character(1), 1L)),
                 unname(expected_groups)) ||
      !identical(unname(relation_groups[data$sample_ids]),
                 unname(expected_groups)) ||
      !identical(dsl$leakage_policy$split_unit, "group"))
    stop("DAG-ML fold, relation and matrix group IDs disagree")
}
fingerprints <- nirs4all:::nirs4all_dag_fingerprints(
  data$X, data$y, data$sample_ids, data$group_ids, data$class_levels)
for (key in names(fingerprints)) {
  if (!identical(envelope[[key]], fingerprints[[key]]))
    stop(paste("DAG-ML envelope does not attest R data:", key))
}
if (!is.list(data$model_specs) ||
    (length(data$model_specs) &&
     (is.null(names(data$model_specs)) || anyDuplicated(names(data$model_specs)))))
  stop("invalid R model specification store")
for (key in names(data$model_specs)) {
  bytes <- data$model_specs[[key]]
  if (!grepl("^[0-9a-f]{64}$", key) || !is.raw(bytes) ||
      !identical(digest::digest(bytes, algo = "sha256", serialize = FALSE), key))
    stop("R model specification fingerprint mismatch")
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
model_spec_from_task <- function(params) {
  key <- params$spec_key
  if (!is.character(key) || length(key) != 1L ||
      !grepl("^[0-9a-f]{64}$", key))
    stop("invalid R model specification key")
  spec_bytes <- data$model_specs[[key]]
  if (!is.raw(spec_bytes) ||
      !identical(digest::digest(spec_bytes, algo = "sha256", serialize = FALSE), key))
    stop("R model specification fingerprint mismatch")
  unserialize(spec_bytes)
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
    ranger_classifier = do.call(nirs4all_ranger_classifier, c(
      list(num.trees = as.integer(params$num_trees),
           seed = if (is.null(params$seed)) 1L else as.integer(params$seed)),
      if (is.null(params$extra)) list() else params$extra)),
    glmnet = nirs4all_glmnet(lambda = as.numeric(params$lambda),
                            alpha = if (is.null(params$alpha)) 1 else as.numeric(params$alpha),
                            standardize = if (is.null(params$standardize)) TRUE else params$standardize),
    parsnip = {
      spec <- model_spec_from_task(params)
      if (!identical(spec$engine, params$engine))
        stop("parsnip model specification engine mismatch")
      nirs4all_parsnip(spec)
    },
    mlr3 = {
      learner <- model_spec_from_task(params)
      if (!inherits(learner, "LearnerRegr") ||
          !identical(learner$id, params$engine))
        stop("mlr3 learner specification identity mismatch")
      nirs4all_mlr3(learner)
    },
    torch_mlp = nirs4all_torch_mlp(
      hidden = if (is.null(params$hidden)) 32L else as.integer(params$hidden),
      epochs = if (is.null(params$epochs)) 100L else as.integer(params$epochs),
      learning_rate = if (is.null(params$learning_rate)) 0.001 else as.numeric(params$learning_rate),
      seed = if (is.null(params$seed)) 1L else as.integer(params$seed)),
    torch_module = {
      builder <- model_spec_from_task(params)
      if (!is.function(builder))
        stop("torch module specification is not a builder function")
      nirs4all_torch_module(builder, name = params$name,
        epochs = as.integer(params$epochs),
        learning_rate = as.numeric(params$learning_rate),
        seed = as.integer(params$seed))
    },
    stop(paste("unknown R learner:", kind)))
}
steps_from_task <- function(task) {
  specs <- task$node_plan$params$preprocessing
  if (is.null(specs)) return(list())
  step_from_spec <- function(spec) {
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
      concat = nirs4all_concat(lapply(spec$branches, function(branch)
        lapply(branch, step_from_spec))),
      stop(paste("unsupported preprocessing step:", spec$kind)))
  }
  lapply(specs, step_from_spec)
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
transform_data_location <- function(handle) {
  file.path(normalizePath(artifact_dir, mustWork = TRUE),
            paste0("data-", handle, ".rds"))
}
input_matrices <- function(task, train_ids, prediction_ids, input_name = "x") {
  raw <- function(ids) if (is.null(ids)) NULL else
    data$X[sample_rows(ids), , drop = FALSE]
  upstream <- task$input_handles[[paste0("data:", input_name)]]
  if (is.null(upstream) ||
      !(upstream$owner_controller %in% c("controller:nirs4all-r-transform",
                                        "controller:nirs4all-r-concat")))
    return(list(train = raw(train_ids), prediction = raw(prediction_ids)))
  if (!identical(upstream$kind, "data") ||
      !is.numeric(upstream$handle) || length(upstream$handle) != 1L)
    stop("invalid upstream transform data handle")
  path <- transform_data_location(as.integer(upstream$handle))
  if (!file.exists(path)) stop("missing upstream transform matrix sidecar")
  payload <- readRDS(path)
  expected_key <- paste0(payload$node_id, ".x_out")
  if (!identical(payload$handle, as.integer(upstream$handle)) ||
      !identical(task$input_handles[[expected_key]]$handle, upstream$handle) ||
      !identical(payload$phase, task$phase) ||
      !identical(payload$fold_id, task$fold_id) ||
      !identical(payload$variant_id, task$variant_id) ||
      !identical(payload$train_ids, train_ids) ||
      !identical(payload$prediction_ids, prediction_ids) ||
      (!is.null(train_ids) && (!is.matrix(payload$train) ||
        nrow(payload$train) != length(train_ids))) ||
      !is.matrix(payload$prediction) ||
      nrow(payload$prediction) != length(prediction_ids))
    stop("upstream transform sidecar does not match DAG task identity")
  list(train = payload$train, prediction = payload$prediction)
}
concat_dsl_step <- function(node) {
  matches <- Filter(function(step) identical(step$id, node) &&
    identical(step$kind, "concat_transform"), dsl$steps)
  if (length(matches) != 1L) stop("unknown DAG concat node")
  matches[[1L]]
}
concat_step_state <- function(spec) {
  branches <- lapply(spec$branches, function(branch)
    lapply(branch$steps, function(child) {
      task <- list(node_plan = list(params = child$params))
      steps_from_task(task)[[1L]]
    }))
  names(branches) <- vapply(spec$branches, `[[`, "", "id")
  step <- nirs4all_concat(branches)
  states <- lapply(spec$branches, function(branch)
    lapply(branch$steps, function(child) {
      artifact <- readRDS(artifact_location(child$id))
      expected <- steps_from_task(list(node_plan = list(params = child$params)))
      if (!identical(artifact$steps, expected) ||
          length(artifact$states) != 1L)
        stop("concat branch fitted state does not match its DAG step")
      artifact$states[[1L]]
    }))
  names(states) <- names(branches)
  list(step = step, states = states)
}
prediction_block <- function(node, partition, fold_id, ids, values) {
  list(producer_node = node, partition = partition, fold_id = fold_id,
       sample_ids = as.list(ids),
       values = lapply(values, function(value) list(as.numeric(value))),
       target_names = list("y"))
}
probability_block <- function(node, fold_id, ids, values) {
  list(producer_node = node, partition = "validation", fold_id = fold_id,
       sample_ids = as.list(ids),
       class_labels = as.list(as.numeric(seq_along(data$class_levels) - 1L)),
       values = lapply(seq_len(nrow(values)), function(index)
         as.list(as.numeric(values[index, ]))))
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
  is_concat <- identical(controller, "controller:nirs4all-r-concat")
  if (is_concat) {
    spec <- concat_dsl_step(node)
    branch_names <- vapply(spec$branches, `[[`, "", "id")
    branch_pairs <- lapply(branch_names, function(name) {
      upstream <- task$input_handles[[paste0("data:", name, "_x")]]
      if (is.null(upstream) ||
          !identical(upstream$owner_controller,
                     "controller:nirs4all-r-transform"))
        stop("concat branch lacks an upstream data handle")
      input_matrices(task, train_ids, prediction_ids, paste0(name, "_x"))
    })
    names(branch_pairs) <- branch_names
    matrices <- list(
      train = if (is.null(train_ids)) NULL else
        nirs4all:::nirs4all_concat_matrices(lapply(branch_pairs, `[[`, "train")),
      prediction = nirs4all:::nirs4all_concat_matrices(
        lapply(branch_pairs, `[[`, "prediction")))
  } else matrices <- input_matrices(task, train_ids, prediction_ids)
  if (controller %in% c("controller:nirs4all-r-transform",
                        "controller:nirs4all-r-concat")) {
    if (is_concat) {
      train_matrix <- matrices$train
      prediction_matrix <- matrices$prediction
      if (identical(phase, "REFIT")) {
        state <- concat_step_state(spec)
        saveRDS(list(steps = list(state$step), states = list(state$states),
                     n_features = ncol(data$X)), artifact_path)
      }
    } else {
      steps <- steps_from_task(task)
      if (length(steps) != 1L) stop("transform node needs one n4m step")
      if (!identical(phase, "PREDICT")) {
        transformed <- nirs4all:::nirs4all_fit_transform(matrices$train, steps)
        prediction_matrix <- nirs4all:::nirs4all_transform(
          matrices$prediction, steps, transformed$states)
        if (identical(phase, "REFIT"))
          saveRDS(list(steps = steps, states = transformed$states,
                       n_features = ncol(matrices$train)), artifact_path)
        train_matrix <- transformed$X
      } else {
        train_matrix <- NULL
        prediction_matrix <- NULL
      }
    }
    if (identical(phase, "PREDICT")) {
      inputs <- task$artifact_inputs
      expected <- Filter(function(input) identical(input$artifact$id, artifact_id), inputs)
      if (length(expected) != 1L ||
          !identical(expected[[1L]]$artifact$uri, artifact_path) ||
          !identical(expected[[1L]]$artifact$content_fingerprint,
                     digest::digest(artifact_path, algo = "sha256", file = TRUE)))
        stop("PREDICT transform artifact identity or content mismatch")
      if (!is_concat) {
        state <- readRDS(artifact_path)
        if (!identical(state$steps, steps) ||
            !identical(state$n_features, ncol(matrices$prediction)))
          stop("PREDICT transform state does not match its node")
        prediction_matrix <- nirs4all:::nirs4all_transform(
          matrices$prediction, steps, state$states)
      }
    }
    handle <- safe_handle(paste(node, phase, task$variant_id,
                                task$fold_id, sep = ":"))
    saveRDS(list(handle = handle, node_id = node, phase = phase, fold_id = task$fold_id,
                 variant_id = task$variant_id, train_ids = train_ids,
                 prediction_ids = prediction_ids, train = train_matrix,
                 prediction = prediction_matrix), transform_data_location(handle))
    artifacts <- list()
    artifact_handles <- empty_object
    if (identical(phase, "REFIT")) {
      artifacts <- list(list(id = artifact_id,
        kind = if (is_concat) "nirs4all_r_concat" else "nirs4all_r_transform",
        controller_id = controller, backend = "rds", uri = artifact_path,
        content_fingerprint = digest::digest(artifact_path,
          algo = "sha256", file = TRUE),
        size_bytes = as.integer(file.info(artifact_path)$size),
        plugin = "nirs4all-r",
        plugin_version = as.character(utils::packageVersion("nirs4all"))))
      artifact_handles <- setNames(list(list(
        handle = safe_handle(artifact_id), kind = "model",
        owner_controller = controller)), artifact_id)
    }
    result <- list(node_id = node,
      outputs = list(x_out = list(handle = handle, kind = "data",
        owner_controller = controller)),
      predictions = list(), artifacts = artifacts,
      artifact_handles = artifact_handles,
      lineage = list(record_id = paste("lineage", node, phase,
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
        seed = "__DAGML_U64_SEED__", unsafe_flags = list(),
        metrics = empty_object, loss_attestations = list(),
        early_stopping_records = list()))
    encoded <- jsonlite::toJSON(result, auto_unbox = TRUE, null = "null", digits = 17)
    return(sub('"seed":"__DAGML_U64_SEED__"',
      paste0('"seed":', seed_decimal), encoded, fixed = TRUE))
  }
  artifacts <- list()
  artifact_handles <- empty_object
  if (!identical(phase, "PREDICT")) {
    train_rows <- sample_rows(train_ids)
    learner <- learner_from_task(task)
    if (!identical(identical(learner$task, "classification"),
                   !is.null(data$class_levels)))
      stop("DAG learner task differs from attested target kind")
    fitted <- nirs4all_fit(nirs4all_pipeline(steps = steps_from_task(task),
                                            learner = learner),
                          matrices$train,
                          if (identical(learner$task, "classification"))
                            factor(data$class_levels[data$y[train_rows] + 1L],
                                   levels = data$class_levels)
                          else data$y[train_rows])
    if (identical(phase, "REFIT")) {
      nirs4all_save(fitted, artifact_path)
      artifact <- list(id = artifact_id, kind = "nirs4all_r_model",
                       controller_id = controller, backend = "rds",
                       uri = artifact_path,
                       content_fingerprint = digest::digest(artifact_path,
                                                           algo = "sha256", file = TRUE),
                       size_bytes = as.integer(file.info(artifact_path)$size),
                       plugin = "nirs4all-r",
                       plugin_version = as.character(utils::packageVersion("nirs4all")))
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
  predictions <- nirs4all_predict(fitted, matrices$prediction)
  classification <- identical(fitted$task, "classification")
  if (classification) {
    if (!identical(fitted$classes, data$class_levels))
      stop("fitted class levels differ from attested DAG data")
    probabilities <- if (identical(phase, "FIT_CV"))
      nirs4all_predict_proba(fitted, matrices$prediction) else NULL
    predictions <- as.numeric(predictions) - 1L
  }
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
  if (classification && identical(phase, "FIT_CV"))
    result$classification_probabilities <- list(probability_block(
      node, task$fold_id, prediction_ids, probabilities))
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
