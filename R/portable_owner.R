# Owner transport for the portable JSON/YAML runner.
#
# Linear regression recipes are admitted, split, fitted, scored and selected by
# the Core operations `portable-pipeline-plan` / `portable-pipeline-run` (DAG
# single-holdout bank or its training sibling over Methods kernels). This file
# only serializes the complete original document and dataset, calls the
# existing `nirs4all-core-archive` CLI and projects the owner result. It holds
# no splitter, variant, fit, metric or selection logic and no local fallback.
# The residual B01 table below names the profiles that keep the unchanged local
# R runner until their owner seams exist.

.nirs4all_portable_owner_profile <- "portable_linear_regression.v1"

# Temporary residual B01: profiles declared by the parsed recipe that the
# linear owner route cannot carry today. Each entry is a REQUIRED later
# migration inside the same consolidation goal, not a permanent exception and
# not a claim of DAG/native parity. Selection is by declared profile only.
.nirs4all_portable_residual_b01 <- list(
  classification = list(
    reason = paste("Classification (n4m.SparsePLSDA, accuracy, ordered class labels):",
      "bank v1 selects on RMSE only and the native 1.1.0 pipeline_steps_v1",
      "controller is regression-only."),
    next_seam = paste("Native classifier variant of the 1.1.0 controller (labels,",
      "exact class order) plus an independently versioned bank v2 profile",
      "selecting on accuracy, maximized.")),
  feature_branch = list(
    reason = paste("branch + merge: features: RolePipeline has no topology role",
      "and the bank admits a single N4mRolePipeline target node."),
    next_seam = "Owner topology support for feature branches in the portable bank."),
  train_augmentation = list(
    reason = paste("train_augmentation: RolePipeline has no augmenter role and",
      "DAG native roles exclude augmenters."),
    next_seam = "Owner fit-population augmenter support."),
  opls_solver = list(
    reason = paste("PLS algo = \"opls_nipals\": R executes the existing N4M_ALGO_OPLS",
      "kernel, but the generic RolePipeline regressor models.pls.pls_regression",
      "fixes N4M_ALGO_PLS_REGRESSION (linear_adapters.cpp:358-363) and exposes no",
      "OPLS role; renaming it to a PLS NIPALS solver would change the algorithm."),
    next_seam = paste("Methods owner exposes the existing OPLS path as a generic",
      "RolePipeline regressor (methods-opls-role-20261108) and DAG lowers it; remove",
      "this key when that role is qualified. Only opls_nipals; no solver substitution.")),
  extra_fit_inputs = list(
    reason = paste("n4m.MBPLS block_sizes and n4m.GroupSparsePLS group_assignment:",
      "transport of these extra fit inputs through the pipeline_steps_v1",
      "controller is not shown in source."),
    next_seam = paste("Owner manifest admission of these parameters; move the",
      "profile to the owner route once the plan operation admits it.")))

.nirs4all_portable_profile <- function(plan) {
  learner <- plan$learner
  keys <- c(
    if (identical(learner$learner, "sparse_pls_da")) "classification",
    if (any(vapply(plan$preprocessing, function(step)
        identical(step$type, "FeatureConcat"), logical(1)))) "feature_branch",
    if (length(plan$augmentations)) "train_augmentation",
    if (identical(learner$learner, "pls") && identical(learner$algo, "opls_nipals"))
      "opls_solver",
    if (identical(learner$learner, "n4m_method") &&
        learner$method %in% c("mb_pls", "group_sparse_pls")) "extra_fit_inputs")
  if (!length(keys))
    return(list(route = "owner", name = .nirs4all_portable_owner_profile,
                residual = character(), reasons = character(),
                next_seams = character()))
  list(route = "residual_b01", name = "residual_b01", residual = keys,
       reasons = vapply(keys, function(key)
         .nirs4all_portable_residual_b01[[key]]$reason, character(1)),
       next_seams = vapply(keys, function(key)
         .nirs4all_portable_residual_b01[[key]]$next_seam, character(1)))
}

# `params`/`method_params` are mappings in every portable schema; an empty R
# list would otherwise serialize as an array and change the document type.
.nirs4all_portable_shape <- function(value) {
  if (!is.list(value)) return(value)
  names_ <- names(value)
  shaped <- lapply(value, .nirs4all_portable_shape)
  for (key in intersect(names_, c("params", "method_params")))
    if (is.list(shaped[[key]]) && !length(shaped[[key]]))
      shaped[[key]] <- structure(list(), names = character())
  shaped
}

.nirs4all_portable_json <- function(value) {
  as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null",
                                digits = I(17L), pretty = FALSE))
}

# The complete original document is forwarded: comments, root controls, unknown
# fields and explicit JSON/YAML types (`params: []` stays an array, `{}` stays an
# object); the owner admits or refuses them. A definition returned by
# nirs4all_load_pipeline() carries that original document and its visible
# baseline: unedited, the original is sent as is; each of the four visible
# fields (name, description, random_state, pipeline) that was edited replaces
# only its counterpart in the original, so edits reach the owner while unrelated
# root controls remain. An edited `pipeline` is the visible, comment-free list.
.nirs4all_portable_document <- function(source) {
  if (!inherits(source, "nirs4all_pipeline_definition"))
    return(nirs4all_portable_original(source))
  original <- attr(source, "original_document")
  baseline <- attr(source, "original_visible")
  if (is.null(original) || is.null(baseline)) {
    # Hand-built definition object: only its visible fields exist.
    document <- unclass(source)
    if (is.null(document$random_state)) document$random_state <- NULL
    if (identical(document$description, "")) document$description <- NULL
    return(document)
  }
  edited <- Filter(function(field) !identical(source[[field]], baseline[[field]]),
                   c("name", "description", "random_state", "pipeline"))
  if (!length(edited)) return(original)
  document <- if (nirs4all_portable_named(original)) original else list(pipeline = original)
  steps_key <- if (!"pipeline" %in% names(document) && "steps" %in% names(document))
    "steps" else "pipeline"
  for (field in edited) {
    key <- if (identical(field, "pipeline")) steps_key else field
    if (is.null(source[[field]])) document[[key]] <- NULL else document[[key]] <- source[[field]]
  }
  document
}

.nirs4all_portable_request <- function(source, dataset = NULL) {
  request <- list(schema_version = 1L, profile = .nirs4all_portable_owner_profile,
                  definition = .nirs4all_portable_document(source))
  if (!is.null(dataset)) request$dataset <- dataset
  request
}

# Dataset forms. Plain list/matrix: row-scoped positional execution IDs (the
# owner marks them positional; row names are not identity). Formats-backed:
# declared sample IDs and axis facts at full precision. Record-backed: the
# complete IO record, projected by the owner or refused by name. Nothing is
# reassembled or inferred here.
.nirs4all_portable_owner_dataset <- function(dataset) {
  if (inherits(dataset, "nirs4all_dataset") && is.null(dataset$X) &&
      !is.null(dataset$record))
    return(list(payload = dataset$record, core_cli = dataset$core_cli))
  data <- nirs4all_portable_dataset(dataset, classification = FALSE)
  payload <- list(
    X = lapply(seq_len(nrow(data$X)), function(i) as.list(as.numeric(data$X[i, ]))),
    y = as.list(as.numeric(data$y)))
  if (inherits(dataset, "nirs4all_dataset") && !is.null(dataset$sample_ids)) {
    payload$sample_ids <- as.list(dataset$sample_ids)
    payload$axis <- list(kind = dataset$axis_kind, unit = dataset$axis_unit,
                         signal_type = dataset$signal_type,
                         wavelengths = as.list(as.numeric(dataset$wavelengths)))
    # Optional dense `metadata`/`provenance` (PUBLIC_API "Dense row metadata and
    # provenance"): one opaque JSON object or list per row, in X/sample_ids
    # order, passed unchanged. They are not groups and not identity facts; the
    # owner refuses a length that differs from X.
    for (field in c("metadata", "provenance"))
      if (!is.null(dataset[[field]])) payload[[field]] <- dataset[[field]]
  }
  list(payload = payload, core_cli = NULL)
}

.nirs4all_portable_methods_library <- function(methods_library) {
  if (is.null(methods_library)) {
    methods_library <- Sys.getenv("N4M_LIB_PATH", "")
    if (!nzchar(methods_library)) methods_library <- Sys.getenv("N4M_LIBRARY_PATH", "")
  }
  if (!is.character(methods_library) || length(methods_library) != 1L ||
      is.na(methods_library) || !nzchar(methods_library) ||
      !file.exists(methods_library) || dir.exists(methods_library))
    stop(paste("The linear portable profile needs the native Methods library:",
               "pass methods_library or set N4M_LIB_PATH; there is no local fallback"),
         call. = FALSE)
  normalizePath(methods_library, mustWork = TRUE)
}

.nirs4all_portable_owner_condition <- function(message, operation, detail, subclass) {
  structure(class = c(subclass, "nirs4all_portable_owner_error", "error", "condition"),
            list(message = message, call = NULL, operation = operation, detail = detail))
}

# One Core CLI call. The owner response is decoded unchanged; a non-zero exit
# is signalled with the native message. Nothing is retried or rerouted.
.nirs4all_portable_owner_call <- function(operation, request, core_cli = NULL,
                                          methods_library = NULL) {
  core_cli <- .nirs4all_native_cli(if (is.null(core_cli)) "" else core_cli)
  run <- identical(operation, "portable-pipeline-run")
  if (run) methods_library <- .nirs4all_portable_methods_library(methods_library)
  directory <- tempfile("nirs4all-portable-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  input <- file.path(directory, "request.json")
  output <- file.path(directory, "result.json")
  log <- file.path(directory, "native.log")
  writeLines(.nirs4all_portable_json(request), input, useBytes = TRUE)
  args <- c(operation, "--input", input, "--output", output,
            if (run) c("--methods-library", methods_library))
  status <- system2(core_cli, vapply(args, shQuote, character(1)),
                    stdout = log, stderr = log)
  detail <- if (file.exists(log)) paste(readLines(log, warn = FALSE), collapse = "\n") else ""
  if (!identical(status, 0L) || !file.exists(output)) {
    if (grepl(paste0("unknown archive command: ", operation), detail, fixed = TRUE))
      stop(.nirs4all_portable_owner_condition(paste0("The native CLI does not provide ",
        operation, ". Use a nirs4all-core-archive build that includes the portable ",
        "pipeline commands; the linear profile has no local fallback."), operation,
        detail, "nirs4all_portable_owner_unavailable"))
    stop(.nirs4all_portable_owner_condition(paste0(operation, " refused (", status,
      "): ", detail), operation, detail, "nirs4all_portable_owner_refusal"))
  }
  jsonlite::fromJSON(output, simplifyVector = FALSE, bigint_as_char = TRUE)
}

# Projection only: field renaming and vector typing, no score, rank or winner.
.nirs4all_portable_project <- function(response) {
  need <- function(value, what) {
    if (is.null(value)) stop(paste("owner response lacks", what), call. = FALSE)
    value
  }
  if (!is.list(response) || !identical(response$schema_version, 1L) ||
      !identical(response$profile, .nirs4all_portable_owner_profile) ||
      !(is.character(response$mode) && length(response$mode) == 1L &&
        response$mode %in% c("training", "single_holdout")))
    stop("unsupported portable owner response schema, profile or mode", call. = FALSE)
  integers <- function(value) as.integer(unlist(value, use.names = FALSE))
  numbers <- function(value) as.numeric(unlist(value, use.names = FALSE))
  variant <- function(entry) list(
    n_components = if (is.null(entry$n_components)) NA_integer_ else
      as.integer(entry$n_components),
    rmse = as.numeric(need(entry$rmse, "rmse")),
    predictions = numbers(need(entry$predictions, "predictions")),
    params = entry$params, ordinal = entry$ordinal, variant_id = entry$variant_id)
  split <- need(response$split, "split")
  evaluation <- need(response$evaluation, "evaluation")
  list(name = response$name, rows = as.integer(need(response$rows, "rows")),
       cols = as.integer(need(response$cols, "cols")),
       split = list(kind = split$kind, trainIndices = integers(split$trainIndices),
                    testIndices = integers(split$testIndices)),
       preprocessing = response$preprocessing,
       variants = lapply(need(response$variants, "variants"), variant),
       selected = variant(need(response$selected, "selected")),
       targets = numbers(need(response$targets, "targets")),
       evaluation = list(scope = evaluation$scope,
                         independent_test = evaluation$independent_test),
       mode = response$mode, identities = response$identities, owner = response$owner,
       profile = list(route = "owner", name = response$profile,
                      owner_profile = response$owner_profile,
                      residual = character(), reasons = character(),
                      next_seams = character()))
}

.nirs4all_portable_run_owner <- function(source, dataset, core_cli, methods_library) {
  marshalled <- .nirs4all_portable_owner_dataset(dataset)
  if (is.null(core_cli)) core_cli <- marshalled$core_cli
  request <- .nirs4all_portable_request(source, marshalled$payload)
  .nirs4all_portable_project(.nirs4all_portable_owner_call(
    "portable-pipeline-run", request, core_cli, methods_library))
}
