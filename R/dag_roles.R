# Methods-backed process controller for already materialized numeric sources.
# DAG-ML owns all plans, folds, OOF construction, signing and replay validation.
.nirs4all_role_id <- "controller:methods.r.regression"
.nirs4all_role_version <- "1.0.0"
.nirs4all_role_plugin <- "dagml.methods.r.regression"
.nirs4all_role_empty <- function() structure(list(), names = character())
.nirs4all_role_require <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}
.nirs4all_role_ids <- function(value, label) {
  if (is.list(value)) value <- unlist(value, use.names = FALSE)
  .nirs4all_role_require(is.character(value) && length(value) > 0L &&
    !anyNA(value) && all(nzchar(trimws(value))) && !anyDuplicated(value),
    paste(label, "needs unique nonempty IDs or names"))
  unname(value)
}
.nirs4all_role_map <- function(value, label, nonempty = FALSE) {
  .nirs4all_role_require(is.list(value) && (!nonempty || length(value) > 0L) &&
    (!length(value) || (!is.null(names(value)) && !anyNA(names(value)) &&
      all(nzchar(names(value))) && !anyDuplicated(names(value)))),
    paste(label, "must be a named object"))
  value
}
.nirs4all_role_matrix <- function(value, ids, label) {
  .nirs4all_role_require(is.matrix(value) && is.numeric(value) &&
    nrow(value) == length(ids) && ncol(value) > 0L && !anyNA(value) &&
    all(is.finite(value)), paste(label, "must be a finite numeric matrix aligned to IDs"))
  storage.mode(value) <- "double"
  value
}
.nirs4all_role_equal <- function(a, b) {
  # Structural comparison only. Native DAG-ML remains the fingerprint authority.
  if (is.list(a) && is.list(b)) {
    if (length(a) != length(b) || !setequal(names(a), names(b))) return(FALSE)
    if (!is.null(names(a))) b <- b[names(a)]
    return(all(vapply(seq_along(a), function(i)
      .nirs4all_role_equal(a[[i]], b[[i]]), logical(1))))
  }
  if (is.numeric(a) && is.numeric(b)) return(identical(as.double(a), as.double(b)))
  identical(a, b)
}
.nirs4all_role_namespace <- function(key)
  sub(":(validation|outer|refit|predict|test)$", "", key)
.nirs4all_role_keys <- function(object)
  names(object)[order(.nirs4all_role_namespace(names(object)), names(object), method = "radix")]
.nirs4all_role_steps <- function(node, operator) {
  .nirs4all_role_require(is.list(operator) && is.character(operator$type) &&
    length(operator$type) == 1L, "A current compiled Methods operator is required")
  if (identical(operator$type, "N4mRolePipeline")) {
    steps <- operator$steps
  } else {
    .nirs4all_role_require(startsWith(operator$type, "n4m:"),
      "Operator must be n4m:<method-id> or N4mRolePipeline")
    steps <- list(list(class = operator$type, params = .nirs4all_role_empty()))
  }
  .nirs4all_role_require(is.list(steps) && length(steps) > 0L && length(steps) <= 128L,
    "Methods recipe must contain 1 to 128 steps")
  for (i in seq_along(steps)) {
    step <- steps[[i]]
    .nirs4all_role_require(is.list(step) && !is.null(names(step)) &&
      !anyDuplicated(names(step)) && all(names(step) %in% c("class", "methodId", "params")) &&
      xor(!is.null(step$class), !is.null(step$methodId)), "Invalid native Methods recipe step")
    token <- if (is.null(step$class)) step$methodId else step$class
    .nirs4all_role_ids(token, "Methods step")
    .nirs4all_role_require(length(token) == 1L &&
      (is.null(step$class) || startsWith(token, "n4m:")), "Invalid native Methods method ID")
    steps[[i]]$params <- .nirs4all_role_map(if (is.null(step$params))
      .nirs4all_role_empty() else step$params, "Methods parameters")
  }
  params <- .nirs4all_role_map(if (is.null(node$params))
    .nirs4all_role_empty() else node$params, "Planned parameters")
  last <- length(steps)
  for (key in names(params)) steps[[last]]$params[key] <- params[key]
  steps
}
.nirs4all_role_native_steps <- function(steps) lapply(steps, function(step)
  list(method_id = if (is.null(step$class)) step$methodId else
    substring(step$class, 5L), params = step$params))
.nirs4all_role_dispose <- function(model) {
  # n4m's public R role API uses native external-pointer finalizers, rather
  # than a dispose method. Drop the pointer and checkpoint references before
  # collecting, so release completes the upstream C finalizer synchronously.
  if (!is.null(model) && is.environment(model$state)) {
    model$state$pointer <- NULL
    model$state$n4me <- NULL
  }
  invisible(gc())
}

#' Prepare a native Methods multimodal DAG process adapter
#'
#' Prepares a persistent JSONL controller for an existing native DAG-ML plan.
#' Sources are named numeric projections, not raw N-D encoders. DAG-ML must
#' validate and sign training/replay contracts and compare signed manifests
#' with independently trusted installed manifests before invoking this adapter.
#' This function does not build folds, search spaces, plans or prediction caches.
#' @param sources Named list of lists with finite numeric `X`, explicit unique
#'   `sample_ids`, and optional ordered `feature_names` (otherwise column names
#'   or zero-based positional names). Each source may have its own row order.
#' @param operators Named map of node IDs to their current compiled native
#'   operators, using `n4m:<method-id>` or `N4mRolePipeline` recipes.
#' @param controller_manifest Native DAG-ML model controller manifest.
#' @param trusted_manifest Independently trusted current installed manifest;
#'   must equal `controller_manifest`. Both use controller:methods.r.regression
#'   version 1.0.0. Trust cannot be inferred from a package or its hashes.
#' @param targets Optional list with finite numeric `values` and unique
#'   `sample_ids`; values is a vector or samples-by-targets matrix.
#' @param target_names Unique ordered response names, default `"y"`.
#' @param allow_fit Explicitly allow FIT_CV/REFIT. Defaults to FALSE for replay.
#' @param workdir New or empty local directory for config and launcher.
#' @return List with `adapter` executable path, `manifest`, `workdir`, and
#'   `audit_path` for lifecycle metadata. Run via native DAG-ML process wrappers.
#' @export
nirs4all_dag_role_adapter <- function(sources, operators, controller_manifest,
    trusted_manifest, targets = NULL, target_names = "y", allow_fit = FALSE,
    workdir = tempfile("nirs4all-role-adapter-")) {
  .nirs4all_role_require(.Platform$OS.type != "windows",
    "The native Methods DAG process adapter currently requires a POSIX shell")
  .nirs4all_role_require(is.logical(allow_fit) && length(allow_fit) == 1L && !is.na(allow_fit),
    "allow_fit must be TRUE or FALSE")
  target_names <- .nirs4all_role_ids(target_names, "Target names")
  sources <- .nirs4all_role_map(sources, "Sources", TRUE)
  sources <- lapply(sources, function(source) {
    source <- .nirs4all_role_map(source, "Numeric source", TRUE)
    .nirs4all_role_require(
      all(names(source) %in% c("X", "sample_ids", "feature_names")), "Invalid named numeric source")
    ids <- .nirs4all_role_ids(source$sample_ids, "Source sample IDs")
    X <- .nirs4all_role_matrix(source$X, ids, "Source X")
    if (!is.null(rownames(X))) .nirs4all_role_require(identical(rownames(X), ids),
      "Source row names disagree with sample IDs")
    features <- source$feature_names
    if (is.null(features)) features <- colnames(X)
    if (is.null(features)) features <- as.character(seq_len(ncol(X)) - 1L)
    features <- .nirs4all_role_ids(features, "Source feature names")
    .nirs4all_role_require(length(features) == ncol(X) &&
      (is.null(colnames(X)) || identical(colnames(X), features)),
      "Source feature names disagree with its ordered columns")
    list(X = X, sample_ids = ids, feature_names = features)
  })
  if (!is.null(targets)) {
    targets <- .nirs4all_role_map(targets, "Target table", TRUE)
    .nirs4all_role_require(
      all(names(targets) %in% c("values", "sample_ids")), "Invalid target table")
    ids <- .nirs4all_role_ids(targets$sample_ids, "Target sample IDs")
    values <- targets$values
    if (is.numeric(values) && is.null(dim(values))) values <- matrix(values, ncol = 1L)
    values <- .nirs4all_role_matrix(values, ids, "Targets")
    .nirs4all_role_require(ncol(values) == length(target_names) &&
      (is.null(rownames(values)) || identical(rownames(values), ids)) &&
      (is.null(colnames(values)) || identical(colnames(values), target_names)),
      "Target width or ordered names disagree with target IDs/names")
    targets <- list(values = values, sample_ids = ids)
  }
  .nirs4all_role_require(!allow_fit || !is.null(targets), "FIT_CV/REFIT requires explicit targets")
  .nirs4all_role_require(allow_fit || is.null(targets), "A replay adapter must not receive target values")
  operators <- .nirs4all_role_map(operators, "Current operators", TRUE)
  for (operator in operators) {
    steps <- .nirs4all_role_native_steps(.nirs4all_role_steps(list(), operator))
    n4m::n4m_role_pipeline(steps) # Native recipe validation; no fit.
  }
  .nirs4all_role_require(is.list(controller_manifest) && is.list(trusted_manifest) &&
    .nirs4all_role_equal(controller_manifest, trusted_manifest),
    "The signed controller manifest differs from the independently trusted current manifest")
  .nirs4all_role_require(identical(controller_manifest$controller_id, .nirs4all_role_id) &&
    identical(controller_manifest$controller_version, .nirs4all_role_version) &&
    identical(controller_manifest$operator_kind, "model") &&
    "consumes_oof_predictions" %in% unlist(controller_manifest$capabilities),
    "The current Methods R model manifest identity or OOF capability is invalid")
  .nirs4all_role_require(is.character(workdir) && length(workdir) == 1L &&
    !is.na(workdir) && nzchar(workdir), "workdir must be a nonempty directory path")
  .nirs4all_role_require(!dir.exists(workdir) ||
    !length(list.files(workdir, all.files = TRUE, no.. = TRUE)), "workdir must be empty")
  if (!dir.exists(workdir)) dir.create(workdir, recursive = TRUE)
  workdir <- normalizePath(workdir, mustWork = TRUE)
  config <- list(sources = sources, targets = targets, target_names = target_names,
    operators = operators, manifest = controller_manifest, trusted_manifest = trusted_manifest,
    allow_fit = allow_fit, audit_path = file.path(workdir, "lifecycle.jsonl"))
  config_path <- file.path(workdir, "config.rds")
  saveRDS(config, config_path)
  source <- system.file("adapters", "nirs4all_role_adapter.R", package = "nirs4all", mustWork = TRUE)
  rscript <- file.path(R.home("bin"), "Rscript")
  .nirs4all_role_require(file.exists(rscript), "Rscript executable is unavailable")
  adapter <- file.path(workdir, "run-r-role-adapter")
  writeLines(c("#!/bin/sh", paste("export NIRS4ALL_DAG_ROLE_CONFIG=", shQuote(config_path), sep = ""),
    paste0("exec ", shQuote(rscript), " ", shQuote(source), " \"$@\"")), adapter)
  Sys.chmod(adapter, "0755")
  list(adapter = adapter, manifest = controller_manifest, workdir = workdir,
    audit_path = config$audit_path)
}

.nirs4all_role_controller <- function(config) {
  .nirs4all_role_require(.nirs4all_role_equal(config$manifest, config$trusted_manifest),
    "Methods R current trust mismatch")
  models <- new.env(parent = emptyenv())
  released <- new.env(parent = emptyenv())
  artifacts <- new.env(parent = emptyenv())
  next_handle <- 0L
  closed <- FALSE
  audit <- function(operation, node = NULL, ids = NULL) {
    json <- jsonlite::toJSON(list(operation = operation, node_id = node,
      sample_ids = as.list(ids)), auto_unbox = TRUE, null = "null")
    cat(json, "\n", file = config$audit_path, append = TRUE, sep = "")
  }
  dispose <- function(model, node = NULL) {
    .nirs4all_role_dispose(model)
    audit("dispose", node)
  }
  close <- function() {
    for (key in ls(models)) {
      entry <- get(key, envir = models, inherits = FALSE)
      dispose(entry$model, entry$node_id)
      rm(list = key, envir = models)
      assign(key, TRUE, envir = released)
      audit("release", entry$node_id)
    }
    rm(list = ls(artifacts), envir = artifacts)
    closed <<- TRUE
    invisible(NULL)
  }
  join <- function(blocks) {
    .nirs4all_role_require(length(blocks) > 0L, "No current feature blocks were supplied")
    ids <- blocks[[1L]]$sample_ids
    X <- do.call(cbind, lapply(blocks, function(block) {
      .nirs4all_role_require(setequal(block$sample_ids, ids), "Feature block sample coverage mismatch")
      block$X[match(ids, block$sample_ids), , drop = FALSE]
    }))
    features <- .nirs4all_role_ids(unlist(lapply(blocks, `[[`, "feature_names"),
      use.names = FALSE), "Current ordered feature names")
    colnames(X) <- features
    list(sample_ids = ids, X = X, feature_names = features)
  }
  features <- function(task, partition) {
    views <- .nirs4all_role_map(task$data_views, "Native data views")
    blocks <- lapply(.nirs4all_role_keys(views), function(key) {
      view <- views[[key]]
      if (!identical(view$partition, partition)) return(NULL)
      # Native NonFit views retain excluded rows for validation/prediction
      # coverage. Their explicit sample IDs are authoritative. Fitting still
      # refuses excluded rows; no host may turn a NonFit view into a fit.
      nonfit <- partition %in% c("fold_validation", "predict")
      .nirs4all_role_require(identical(view$include_augmented, FALSE) &&
        is.logical(view$include_excluded) && length(view$include_excluded) == 1L &&
        !is.na(view$include_excluded) && (nonfit || identical(view$include_excluded, FALSE)),
        "Augmented or excluded fitting views require a specialized controller")
      ids <- .nirs4all_role_ids(view$sample_ids, "Native view sample IDs")
      source_ids <- .nirs4all_role_ids(view$source_ids, "Native view source IDs")
      .nirs4all_role_require(length(source_ids) == 1L && source_ids %in% names(config$sources),
        "Native view must identify one explicitly named current source")
      source <- config$sources[[source_ids]]
      rows <- match(ids, source$sample_ids)
      .nirs4all_role_require(!anyNA(rows), "Native view requested unknown source sample IDs")
      columns <- seq_len(ncol(source$X))
      if (!is.null(view$columns)) {
        names <- .nirs4all_role_ids(view$columns, "Native view columns")
        columns <- match(names, source$feature_names)
        .nirs4all_role_require(!anyNA(columns), "Native view requested unknown source columns")
      }
      list(sample_ids = ids, X = source$X[rows, columns, drop = FALSE],
        feature_names = paste0(.nirs4all_role_namespace(key), "/", source$feature_names[columns]))
    })
    join(Filter(Negate(is.null), blocks))
  }
  predictions <- function(task, outer) {
    inputs <- .nirs4all_role_map(task$prediction_inputs, "Native prediction inputs")
    suffix <- switch(task$phase, FIT_CV = ":outer", REFIT = ":refit", PREDICT = ":predict")
    keys <- .nirs4all_role_keys(inputs)
    keys <- keys[if (outer) endsWith(keys, suffix) else !grepl(":(outer|refit|predict|test)$", keys)]
    join(lapply(keys, function(key) {
      input <- inputs[[key]]
      ids <- .nirs4all_role_ids(input$sample_ids, "OOF sample IDs")
      .nirs4all_role_require(identical(input$prediction_level, "sample"),
        "This controller requires sample-level prediction inputs")
      if (identical(task$phase, "FIT_CV") || (identical(task$phase, "REFIT") && !outer)) {
        .nirs4all_role_require(identical(input$partition, "validation"),
          "Meta training must consume native validation OOF")
        if (identical(task$phase, "FIT_CV") && !outer)
          .nirs4all_role_require(!(task$fold_id %in% unlist(input$fold_ids)),
            "Outer-fold predictions cannot fit their meta-model")
      }
      if (outer && !identical(task$phase, "FIT_CV"))
        .nirs4all_role_require(input$partition %in% c("test", "final"),
          "Meta prediction needs native off-fold rows")
      width <- input$prediction_width
      .nirs4all_role_require(is.numeric(width) && length(width) == 1L &&
        is.finite(width) && width >= 1 && width == floor(width) &&
        is.list(input$values) && length(input$values) == length(ids) &&
        all(vapply(input$values, function(row) is.list(row) && length(row) == width &&
          all(vapply(row, function(x) is.numeric(x) && length(x) == 1L && is.finite(x), logical(1))),
          logical(1))), "Native prediction feature width or values are inconsistent")
      X <- matrix(unlist(input$values, use.names = FALSE), nrow = length(ids), byrow = TRUE)
      list(sample_ids = ids, X = X,
        feature_names = paste0(.nirs4all_role_namespace(key), "/", seq_len(width) - 1L))
    }))
  }
  targets <- function(ids) {
    .nirs4all_role_require(!is.null(config$targets), "Inference must not access target values")
    rows <- match(ids, config$targets$sample_ids)
    .nirs4all_role_require(!anyNA(rows), "Unknown target sample IDs")
    config$targets$values[rows, , drop = FALSE]
  }
  rows_json <- function(X) lapply(seq_len(nrow(X)), function(i) as.list(unname(X[i, ])))
  result <- function(task, block, model, refs = list(), handles = .nirs4all_role_empty()) {
    values <- stats::predict(model, block$X)
    if (is.numeric(values) && is.null(dim(values))) values <- matrix(values, ncol = 1L)
    values <- .nirs4all_role_matrix(values, block$sample_ids, "Methods predictions")
    .nirs4all_role_require(ncol(values) == length(config$target_names), "Prediction target width mismatch")
    node <- task$node_plan
    out <- list(node_id = node$node_id, outputs = .nirs4all_role_empty(),
      artifacts = refs, artifact_handles = handles,
      predictions = list(list(producer_node = node$node_id,
        partition = if (identical(task$phase, "FIT_CV")) "validation" else "final",
        fold_id = task$fold_id, sample_ids = as.list(block$sample_ids),
        values = rows_json(values), target_names = as.list(config$target_names))),
      lineage = list(record_id = paste("lineage:methods-r", task$run_id, node$node_id, task$phase,
          if (is.null(task$variant_id)) "base" else task$variant_id,
          if (is.null(task$fold_id)) "full" else task$fold_id, sep = ":"),
        run_id = task$run_id, node_id = node$node_id, phase = task$phase,
        controller_id = .nirs4all_role_id, controller_version = .nirs4all_role_version,
        variant_id = task$variant_id, fold_id = task$fold_id,
        branch_path = if (is.null(task$branch_path)) list() else task$branch_path,
        input_lineage = list(), artifact_refs = refs, params_fingerprint = node$params_fingerprint,
        data_model_shape_fingerprint = NULL, aggregation_policy_fingerprint = NULL,
        seed = "__DAGML_U64_SEED__", unsafe_flags = list(), metrics = .nirs4all_role_empty(),
        loss_attestations = list(), early_stopping_records = list()))
    if (identical(task$phase, "FIT_CV")) out$regression_targets <- list(list(level = "sample",
      unit_ids = lapply(block$sample_ids, function(id) list(level = "sample", id = id)),
      values = rows_json(targets(block$sample_ids)), target_names = as.list(config$target_names)))
    audit(task$phase, node$node_id, block$sample_ids)
    out
  }
  hash <- function(bytes) digest::digest(bytes, algo = "sha256", serialize = FALSE)
  check_artifact <- function(artifact, payload) {
    fingerprint <- hash(payload)
    .nirs4all_role_require(is.list(artifact) &&
      identical(artifact$controller_id, .nirs4all_role_id) &&
      identical(artifact$backend, "raw") && identical(artifact$kind, "methods_role_pipeline") &&
      identical(artifact$plugin, .nirs4all_role_plugin) &&
      identical(artifact$plugin_version, .nirs4all_role_version) &&
      identical(artifact$content_fingerprint, fingerprint) &&
      identical(artifact$uri, paste0("artifacts/", fingerprint, ".json")) &&
      is.numeric(artifact$size_bytes) && length(artifact$size_bytes) == 1L &&
      artifact$size_bytes == length(payload), "Methods RAW artifact identity, hash, size or URI mismatch")
    .nirs4all_role_ids(artifact$id, "Artifact ID")
  }
  keep <- function(entry) {
    .nirs4all_role_require(next_handle < .Machine$integer.max, "Methods R handle space exhausted")
    next_handle <<- next_handle + 1L
    assign(as.character(next_handle), entry, envir = models)
    list(handle = next_handle, kind = "model", owner_controller = .nirs4all_role_id)
  }
  portable <- function(task) {
    .nirs4all_role_require(identical(task$schema_version, 1L), "Unsupported portable bridge schema")
    if (identical(task$operation, "export_artifact_payload")) {
      .nirs4all_role_require(exists(task$artifact_id, envir = artifacts, inherits = FALSE), "Unknown Methods artifact")
      audit("export")
      return(list(operation = "exported_artifact_payload", schema_version = 1L,
        payload = as.list(as.integer(get(task$artifact_id, envir = artifacts, inherits = FALSE)))))
    }
    if (identical(task$operation, "release_hydrated_artifact_payload")) {
      handle <- task$handle
      key <- as.character(handle$handle)
      .nirs4all_role_require(identical(handle$owner_controller, .nirs4all_role_id) &&
        identical(handle$kind, "model") && (exists(key, envir = models, inherits = FALSE) ||
          (closed && exists(key, envir = released, inherits = FALSE))),
        "Unknown or foreign hydrated Methods handle")
      if (exists(key, envir = models, inherits = FALSE)) {
        entry <- get(key, envir = models, inherits = FALSE)
        rm(list = key, envir = models)
        dispose(entry$model, entry$node_id)
        assign(key, TRUE, envir = released)
        audit("release", entry$node_id)
      }
      return(list(operation = "released_hydrated_artifact_payload", schema_version = 1L))
    }
    .nirs4all_role_require(identical(task$operation, "hydrate_artifact_payload"), "Unsupported portable operation")
    request <- task$request
    .nirs4all_role_require(identical(request$controller_id, .nirs4all_role_id) &&
      request$node_id %in% names(config$operators), "Portable Methods controller or node mismatch")
    bytes <- task$payload
    .nirs4all_role_require(is.list(bytes) && length(bytes) > 0L &&
      all(vapply(bytes, function(x) is.numeric(x) && length(x) == 1L &&
        is.finite(x) && x == floor(x) && x >= 0 && x <= 255, logical(1))), "Payload must be a nonempty byte array")
    payload <- as.raw(unlist(bytes, use.names = FALSE))
    check_artifact(request$artifact, payload)
    saved <- jsonlite::fromJSON(rawToChar(payload), simplifyVector = FALSE)
    fields <- c("schema", "node_id", "params_fingerprint", "target_names", "steps", "feature_names", "states")
    .nirs4all_role_require(is.list(saved) && !anyDuplicated(names(saved)) &&
      setequal(names(saved), fields) && identical(saved$schema, "dagml.methods.regression.v1") &&
      identical(saved$node_id, request$node_id) && identical(saved$params_fingerprint, request$params_fingerprint),
      "Methods RAW wrapper identity or closed schema mismatch")
    .nirs4all_role_require(identical(.nirs4all_role_ids(saved$target_names, "Saved target names"),
      config$target_names), "Saved target names mismatch")
    feature_names <- .nirs4all_role_ids(saved$feature_names, "Saved feature names")
    .nirs4all_role_require(is.list(saved$states) && length(saved$states) > 0L &&
      length(saved$states) <= 128L && all(vapply(saved$states, function(state)
        is.list(state) && length(state) > 0L && all(vapply(state, function(x)
          is.numeric(x) && length(x) == 1L && is.finite(x) && x == floor(x) && x >= 0 && x <= 255,
          logical(1))), logical(1))), "Methods states must be nonempty native byte arrays")
    # The materialization request does not carry effective selected parameters.
    # Enforce the full current recipe with those parameters again at PREDICT,
    # before numerical execution; the native package validator owns preflight.
    native_steps <- .nirs4all_role_native_steps(.nirs4all_role_steps(list(),
      list(type = "N4mRolePipeline", steps = saved$steps)))
    model <- NULL
    retained <- FALSE
    on.exit(if (!retained) dispose(model, request$node_id), add = TRUE)
    model <- n4m::n4m_role_pipeline_import(native_steps,
      lapply(saved$states, function(state) as.raw(unlist(state, use.names = FALSE))), feature_names)
    .nirs4all_role_require(identical(tail(n4m::n4m_role_pipeline_steps(model)$role, 1L), "regressor"),
      "Methods R controller accepts regressors only")
    handle <- keep(list(model = model, node_id = saved$node_id,
      params_fingerprint = saved$params_fingerprint, steps = saved$steps,
      feature_names = feature_names, artifact = request$artifact))
    retained <- TRUE
    audit("hydrate", request$node_id)
    list(operation = "hydrated_artifact_payload", schema_version = 1L, handle = handle)
  }
  invoke <- function(task) {
    .nirs4all_role_require(!closed || identical(task$operation, "release_hydrated_artifact_payload"),
      "Methods R controller is closed")
    if (!is.null(task$operation)) return(portable(task))
    node <- task$node_plan
    .nirs4all_role_require(identical(node$kind, "model") &&
      identical(node$controller_id, .nirs4all_role_id) &&
      identical(node$controller_version, .nirs4all_role_version) &&
      node$node_id %in% names(config$operators), "Methods R current controller or node mismatch")
    .nirs4all_role_require(task$phase %in% c("FIT_CV", "REFIT", "PREDICT"), "Unsupported Methods phase")
    .nirs4all_role_require(!length(task$data_view_receipts) && !length(task$required_loss_attestations) &&
      is.null(task$residual_targets) && (!length(task$fit_influence) ||
        (identical(task$fit_influence$mechanism, "uniform_rows") && !length(task$fit_influence$row_weights))),
      "Generated views, custom losses, residuals and nonuniform influence need a specialized controller")
    .nirs4all_role_require(!(identical(task$phase, "FIT_CV") &&
      any(endsWith(names(task$prediction_inputs), ":test"))), "Additional CV test streams require a specialized controller")
    meta <- length(task$prediction_inputs) > 0L
    expected_steps <- .nirs4all_role_steps(node, config$operators[[node$node_id]])
    if (identical(task$phase, "PREDICT")) {
      inputs <- .nirs4all_role_map(task$artifact_inputs, "Native artifact inputs", TRUE)
      .nirs4all_role_require(length(inputs) == 1L, "PREDICT requires one attested model artifact")
      key <- names(inputs)[[1L]]
      handle <- task$input_handles[[key]]
      .nirs4all_role_require(identical(handle$owner_controller, .nirs4all_role_id) &&
        identical(handle$kind, "model") && exists(as.character(handle$handle), envir = models, inherits = FALSE),
        "PREDICT model handle owner or kind mismatch")
      entry <- get(as.character(handle$handle), envir = models, inherits = FALSE)
      input <- inputs[[key]]
      .nirs4all_role_require(identical(entry$node_id, node$node_id) &&
        identical(entry$params_fingerprint, node$params_fingerprint) &&
        identical(input$params_fingerprint, node$params_fingerprint) &&
        identical(input$node_id, node$node_id) && identical(input$controller_id, .nirs4all_role_id) &&
        .nirs4all_role_equal(input$artifact, entry$artifact) &&
        .nirs4all_role_equal(entry$steps, expected_steps), "PREDICT model binding or current recipe mismatch")
      block <- if (meta) predictions(task, TRUE) else features(task, "predict")
      .nirs4all_role_require(identical(entry$feature_names, block$feature_names),
        "Portable Methods feature order disagrees with current resolved inputs")
      return(result(task, block, entry$model))
    }
    .nirs4all_role_require(isTRUE(config$allow_fit), "Fitting is disabled for this replay adapter")
    train <- if (meta) predictions(task, FALSE) else
      features(task, if (identical(task$phase, "FIT_CV")) "fold_train" else "full_train")
    valid <- if (identical(task$phase, "FIT_CV")) {
      if (meta) predictions(task, TRUE) else features(task, "fold_validation")
    } else if (meta && any(endsWith(names(task$prediction_inputs), ":refit"))) predictions(task, TRUE) else train
    .nirs4all_role_require(identical(train$feature_names, valid$feature_names), "Fit and validation feature order mismatch")
    if (identical(task$phase, "FIT_CV")) .nirs4all_role_require(!any(train$sample_ids %in% valid$sample_ids),
      "Fit and validation sample IDs overlap")
    model <- NULL
    retained <- FALSE
    on.exit(if (!retained) dispose(model, node$node_id), add = TRUE)
    model <- n4m::n4m_estimator_fit(n4m::n4m_role_pipeline(.nirs4all_role_native_steps(expected_steps)),
      train$X, targets(train$sample_ids))
    .nirs4all_role_require(identical(tail(n4m::n4m_role_pipeline_steps(model)$role, 1L), "regressor"),
      "Methods R controller accepts regressors only")
    audit("fit", node$node_id, train$sample_ids)
    if (identical(task$phase, "FIT_CV")) return(result(task, valid, model))
    states <- n4m::n4m_role_pipeline_export(model, allow_training_rows = FALSE)
    .nirs4all_role_require(length(states) == length(expected_steps),
      "Portable Methods RAW requires one N4ME state per declared recipe step")
    saved <- list(schema = "dagml.methods.regression.v1", node_id = node$node_id,
      params_fingerprint = node$params_fingerprint, target_names = as.list(config$target_names),
      steps = expected_steps, feature_names = as.list(train$feature_names),
      states = lapply(states, function(state) as.list(as.integer(state$n4me))))
    payload <- charToRaw(as.character(jsonlite::toJSON(saved, auto_unbox = TRUE, null = "null", digits = I(17L))))
    fingerprint <- hash(payload)
    artifact_id <- paste("artifact:methods", task$run_id, node$node_id,
      if (is.null(task$variant_id)) "base" else task$variant_id, "refit", sep = ":")
    .nirs4all_role_require(!exists(artifact_id, envir = artifacts, inherits = FALSE), "Duplicate Methods REFIT artifact")
    ref <- list(id = artifact_id, kind = "methods_role_pipeline", controller_id = .nirs4all_role_id,
      backend = "raw", uri = paste0("artifacts/", fingerprint, ".json"), content_fingerprint = fingerprint,
      size_bytes = length(payload), plugin = .nirs4all_role_plugin, plugin_version = .nirs4all_role_version)
    out <- result(task, valid, model, list(ref))
    handle <- keep(list(model = model, node_id = node$node_id, params_fingerprint = node$params_fingerprint,
      steps = expected_steps, feature_names = train$feature_names, artifact = ref))
    out$artifact_handles[[artifact_id]] <- handle
    assign(artifact_id, payload, envir = artifacts)
    retained <- TRUE
    out
  }
  list(invoke = invoke, close = close)
}
