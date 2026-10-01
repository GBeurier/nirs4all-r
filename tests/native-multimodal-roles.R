# Genuine Methods numerics and published process frames. Native DAG scheduling,
# signatures and cross-language five-model qualification live in DAG-ML's gate.
library(nirs4all)
required <- identical(Sys.getenv("NIRS4ALL_REQUIRE_R_ROLE_ADAPTER"), "1")
if (!all(c("n4m_role_pipeline", "n4m_role_pipeline_import") %in% getNamespaceExports("n4m"))) {
  if (required) stop("The mandatory R role adapter gate requires the native Methods role binding")
} else {
  role <- function(name) get(name, envir = asNamespace("nirs4all"))
  decode <- function(value) jsonlite::fromJSON(value, simplifyVector = FALSE)
  encode <- function(value) as.character(jsonlite::toJSON(value, auto_unbox = TRUE,
    null = "null", digits = I(17L)))
  wire <- function(value) decode(encode(value))
  object <- function() structure(list(), names = character())
  refused <- function(expr, text) {
    error <- tryCatch({ force(expr); NULL }, error = identity)
    stopifnot(inherits(error, "error"), grepl(text, conditionMessage(error), fixed = TRUE))
  }
  manifest <- list(controller_id = "controller:methods.r.regression", controller_version = "1.0.0",
    operator_kind = "model", capabilities = list("consumes_oof_predictions"))
  ids <- paste0("sample:", seq_len(10L))
  train_ids <- ids[seq_len(8L)]
  heldout_ids <- ids[9:10]
  sources <- lapply(seq_len(4L), function(s) {
    X <- outer(seq_along(ids), seq_len(s + 2L), function(i, j) sin(i * j / 13 + s) + i / 9)
    permutation <- c(seq.int(2L, 10L, 2L), seq.int(1L, 9L, 2L))
    permutation <- if (s %% 2L) rev(permutation) else permutation
    list(X = X[permutation, , drop = FALSE], sample_ids = ids[permutation])
  })
  names(sources) <- c("nir", "image", "series", "metadata")
  source_X <- function(name, selected) {
    source <- sources[[name]]
    source$X[match(selected, source$sample_ids), , drop = FALSE]
  }
  y <- source_X("nir", ids)[, 1L] + 0.3 * source_X("image", ids)[, 2L]
  operators <- stats::setNames(rep(list(list(type = "n4m:models.regularized.ridge")), 5L),
    paste0("model:", c(names(sources), "meta")))
  prepare <- function(fit = TRUE, current_sources = sources, current_operators = operators) {
    nirs4all_dag_role_adapter(current_sources, current_operators, manifest, manifest,
      targets = if (fit) list(values = y, sample_ids = ids) else NULL, allow_fit = fit)
  }
  prepared <- prepare()
  config <- readRDS(file.path(prepared$workdir, "config.rds"))
  controller <- role(".nirs4all_role_controller")(config)
  view <- function(partition, selected, source) list(partition = partition,
    sample_ids = as.list(selected), source_ids = list(source),
    include_augmented = FALSE, include_excluded = partition %in% c("fold_validation", "predict"))
  task <- function(node, phase, selected = train_ids, params = list(alpha = 0.2)) {
    wire(list(run_id = "run:r-role-test", phase = phase, variant_id = "variant:test",
      fold_id = if (phase == "FIT_CV") "fold:outer" else NULL, seed = 19L,
      branch_path = list(), node_plan = list(node_id = node, kind = "model",
        controller_id = manifest$controller_id, controller_version = manifest$controller_version,
        params = params, params_fingerprint = paste(rep("a", 64L), collapse = "")),
      input_handles = object(), data_views = object(), prediction_inputs = object(),
      artifact_inputs = object()))
  }
  source_task <- function(source, phase, selected = train_ids) {
    out <- task(paste0("model:", source), phase)
    out$data_views <- list(x = view(if (phase == "FIT_CV") "fold_train" else
      if (phase == "REFIT") "full_train" else "predict", selected, source))
    if (phase == "FIT_CV") out$data_views[["x:validation"]] <- view("fold_validation", heldout_ids, source)
    wire(out)
  }
  payloads <- refs <- list()
  source_predictions <- list()
  for (source in names(sources)) {
    cv <- controller$invoke(source_task(source, "FIT_CV"))
    X <- source_X(source, train_ids)
    colnames(X) <- paste0("x/", seq_len(ncol(X)) - 1L)
    native <- n4m::n4m_estimator_fit(n4m::n4m_role_pipeline(list(
      list(method_id = "models.regularized.ridge", params = list(alpha = 0.2)))), X, y[1:8])
    test_X <- source_X(source, heldout_ids)
    colnames(test_X) <- colnames(X)
    expected <- as.numeric(predict(native, test_X))
    stopifnot(max(abs(unlist(cv$predictions[[1L]]$values) - expected)) < 1e-12)
    role(".nirs4all_role_dispose")(native)
    refit <- controller$invoke(source_task(source, "REFIT"))
    ref <- refit$artifacts[[1L]]
    exported <- controller$invoke(wire(list(operation = "export_artifact_payload",
      schema_version = 1L, artifact_id = ref$id)))
    refs[[source]] <- ref
    payloads[[source]] <- exported$payload
    source_predictions[[source]] <- refit$predictions[[1L]]$values
    saved <- decode(rawToChar(as.raw(unlist(exported$payload))))
    stopifnot(is.list(saved$steps), length(saved$steps) == 1L,
      is.list(saved$states), length(saved$states) == 1L,
      is.list(saved$target_names), length(saved$target_names) == 1L,
      identical(ref$plugin, "dagml.methods.r.regression"))
  }
  # Current source IDs, row alignment and off-fold separation are mandatory.
  bad <- source_task("nir", "FIT_CV")
  bad$data_views$x$source_ids <- list("unknown")
  refused(controller$invoke(bad), "explicitly named current source")
  bad <- source_task("nir", "FIT_CV")
  bad$data_views$x$include_excluded <- TRUE
  refused(controller$invoke(bad), "excluded fitting views")
  bad <- source_task("nir", "FIT_CV")
  bad$data_views[["x:validation"]]$include_augmented <- TRUE
  refused(controller$invoke(bad), "specialized controller")
  bad <- source_task("nir", "FIT_CV")
  bad$data_views[["x:validation"]]$sample_ids <- as.list(train_ids[1:2])
  refused(controller$invoke(bad), "overlap")

  prediction_input <- function(values, selected, partition, fold_ids = list("fold:inner"))
    list(prediction_level = "sample", sample_ids = as.list(selected), values = values,
      partition = partition, fold_ids = fold_ids, prediction_width = 1L)
  meta <- task("model:meta", "REFIT")
  # This is controller-level routing evidence, not an OOF scheduler oracle:
  # the separate native DAG gate produces the real inner OOF values.
  for (source in names(sources)) {
    order <- rev(seq_along(train_ids))
    meta$prediction_inputs[[paste0("model:", source, ".oof")]] <- prediction_input(
      source_predictions[[source]][order], train_ids[order], "validation")
  }
  meta_result <- controller$invoke(wire(meta))
  refs$meta <- meta_result$artifacts[[1L]]
  payloads$meta <- controller$invoke(wire(list(operation = "export_artifact_payload",
    schema_version = 1L, artifact_id = refs$meta$id)))$payload
  bad <- meta
  bad$prediction_inputs[[1L]]$partition <- "train"
  refused(controller$invoke(wire(bad)), "validation OOF")
  controller$close()

  replay_prepared <- prepare(FALSE)
  replay_config <- readRDS(file.path(replay_prepared$workdir, "config.rds"))
  replay <- role(".nirs4all_role_controller")(replay_config)
  hydrate <- function(name, bytes = payloads[[name]], ref = refs[[name]]) {
    wire(list(operation = "hydrate_artifact_payload", schema_version = 1L,
      request = list(controller_id = manifest$controller_id, node_id = paste0("model:", name),
        params_fingerprint = paste(rep("a", 64L), collapse = ""), artifact = ref), payload = bytes))
  }
  handles <- lapply(names(refs), function(name) replay$invoke(hydrate(name))$handle)
  names(handles) <- names(refs)
  predict_task <- function(name) {
    out <- if (name == "meta") task("model:meta", "PREDICT") else source_task(name, "PREDICT", heldout_ids)
    key <- paste0("artifact:", name)
    out$input_handles[[key]] <- handles[[name]]
    out$artifact_inputs[[key]] <- list(node_id = paste0("model:", name),
      controller_id = manifest$controller_id, artifact = refs[[name]],
      params_fingerprint = out$node_plan$params_fingerprint)
    wire(out)
  }
  outputs <- list()
  for (source in names(sources)) outputs[[source]] <- replay$invoke(predict_task(source))$predictions[[1L]]
  meta_predict <- predict_task("meta")
  for (source in names(sources)) meta_predict$prediction_inputs[[paste0("model:", source, ".oof:predict")]] <-
    prediction_input(outputs[[source]]$values, heldout_ids, "final")
  predicted <- replay$invoke(wire(meta_predict))$predictions[[1L]]
  stopifnot(identical(unlist(predicted$sample_ids), heldout_ids))
  ordered_sources <- sort(names(sources), method = "radix")
  meta_X <- do.call(cbind, lapply(ordered_sources, function(source)
    unlist(source_predictions[[source]], use.names = FALSE)))
  colnames(meta_X) <- paste0("model:", ordered_sources, ".oof/0")
  expected_model <- n4m::n4m_estimator_fit(n4m::n4m_role_pipeline(list(
    list(method_id = "models.regularized.ridge", params = list(alpha = 0.2)))), meta_X, y[1:8])
  meta_test_X <- do.call(cbind, lapply(ordered_sources, function(source)
    unlist(outputs[[source]]$values, use.names = FALSE)))
  colnames(meta_test_X) <- colnames(meta_X)
  stopifnot(max(abs(unlist(predicted$values) - as.numeric(predict(expected_model, meta_test_X)))) < 1e-10)
  role(".nirs4all_role_dispose")(expected_model)
  refused(replay$invoke(source_task("nir", "REFIT")), "Fitting is disabled")
  bad <- predict_task("nir")
  bad$node_plan$params$alpha <- 5
  refused(replay$invoke(wire(bad)), "current recipe mismatch")
  bad <- meta_predict
  bad$prediction_inputs[[1L]]$sample_ids <- list("foreign", heldout_ids[[2L]])
  refused(replay$invoke(wire(bad)), "sample coverage mismatch")
  bad <- predict_task("nir")
  bad$artifact_inputs[[1L]]$artifact$uri <- "../escape.json"
  refused(replay$invoke(wire(bad)), "model binding")
  bad_hydrate <- hydrate("nir")
  bad_hydrate$request$artifact$content_fingerprint <- paste(rep("b", 64L), collapse = "")
  refused(replay$invoke(bad_hydrate), "hash, size or URI")
  malformed <- decode(rawToChar(as.raw(unlist(payloads$nir))))
  malformed$states <- list(list(1L))
  raw <- charToRaw(encode(malformed))
  malformed_ref <- refs$nir
  malformed_ref$content_fingerprint <- digest::digest(raw, algo = "sha256", serialize = FALSE)
  malformed_ref$uri <- paste0("artifacts/", malformed_ref$content_fingerprint, ".json")
  malformed_ref$size_bytes <- length(raw)
  refused(replay$invoke(hydrate("nir", as.list(as.integer(raw)), malformed_ref)), "n4m_role_pipeline_import")
  for (name in names(handles)) replay$invoke(wire(list(operation = "release_hydrated_artifact_payload",
    schema_version = 1L, handle = handles[[name]])))
  replay$close()
  events <- lapply(readLines(replay_prepared$audit_path), decode)
  operations <- vapply(events, `[[`, "", "operation")
  stopifnot(sum(operations == "hydrate") == 5L, sum(operations == "PREDICT") == 5L,
    sum(operations == "release") == 5L, !any(operations == "fit"))

  # Same-width column permutation is rejected independently of native state.
  changed <- sources
  changed$nir$X <- changed$nir$X[, rev(seq_len(ncol(changed$nir$X))), drop = FALSE]
  changed$nir$feature_names <- rev(as.character(seq_len(ncol(changed$nir$X)) - 1L))
  changed_prepared <- prepare(FALSE, changed)
  changed_controller <- role(".nirs4all_role_controller")(readRDS(file.path(changed_prepared$workdir, "config.rds")))
  changed_handle <- changed_controller$invoke(hydrate("nir"))$handle
  bad <- predict_task("nir")
  bad$input_handles[[1L]] <- changed_handle
  refused(changed_controller$invoke(wire(bad)), "feature order")
  changed_controller$close()
  # Coordinator release remains acknowledged after error cleanup; native
  # external pointer/checkpoint references have already been dropped.
  changed_controller$invoke(wire(list(operation = "release_hydrated_artifact_payload",
    schema_version = 1L, handle = changed_handle)))
  trusted <- manifest
  trusted$controller_version <- "2.0.0"
  refused(nirs4all_dag_role_adapter(sources, operators, manifest, trusted), "independently trusted")
  refused(nirs4all_dag_role_adapter(sources, operators, manifest, manifest,
    targets = list(values = y, sample_ids = ids)), "must not receive target values")

  # Actual new R process: persistent framing, opaque hydration, target-free
  # prediction, exact u64 lineage seed and native release on failure.
  process_prepared <- prepare(FALSE)
  frames <- list(list(type = "init", schema_version = 1L, controller_id = manifest$controller_id,
    worker_index = 0L, worker_count = 1L),
    list(type = "portable_artifact", schema_version = 1L, task = hydrate("nir")))
  process_task <- predict_task("nir")
  process_task$input_handles[[1L]]$handle <- 1L
  frames[[3L]] <- list(type = "task", schema_version = 1L, task = process_task)
  bad <- process_task
  bad$node_plan$params$alpha <- 9
  frames[[4L]] <- list(type = "task", schema_version = 1L, task = bad)
  frames[[5L]] <- list(type = "portable_artifact", schema_version = 1L,
    task = list(operation = "release_hydrated_artifact_payload", schema_version = 1L,
      handle = list(handle = 1L, kind = "model", owner_controller = manifest$controller_id)))
  frames[[6L]] <- list(type = "close", schema_version = 1L)
  lines <- vapply(frames, encode, "")
  lines[[3L]] <- sub('"seed":19', '"seed":18446744073709551615', lines[[3L]], fixed = TRUE)
  input <- file.path(process_prepared$workdir, "frames.jsonl")
  writeLines(lines, input)
  output <- suppressWarnings(system2(process_prepared$adapter, "--jsonl", stdin = input,
    stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  stopifnot(is.null(status) || status == 0L, length(output) == 6L,
    grepl('"seed":18446744073709551615', output[[3L]], fixed = TRUE))
  replies <- lapply(output, decode)
  stopifnot(identical(replies[[3L]]$type, "result"), identical(replies[[4L]]$type, "error"),
    identical(replies[[5L]]$result$operation, "released_hydrated_artifact_payload"))
  unlink(c(prepared$workdir, replay_prepared$workdir, changed_prepared$workdir,
    process_prepared$workdir), recursive = TRUE)
}
