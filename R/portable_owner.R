# Owner transport for the portable JSON/YAML runner.
#
# Linear regression and SparsePLS-DA classification recipes are admitted, split, fitted, scored and selected by
# the Core operations `portable-pipeline-plan` / `portable-pipeline-run` (DAG
# single-holdout bank or its training sibling over Methods kernels). This file
# only serializes the complete original document and dataset, calls the
# existing `nirs4all-core-archive` CLI and projects the owner result. It holds
# no splitter, variant, fit, metric or selection logic and no local fallback.
# The residual B01 table below names the profiles that keep the unchanged local
# R runner until their owner seams exist.

.nirs4all_portable_owner_profile <- "portable_linear_regression.v1"
# Classification (terminal n4m.SparsePLSDA): bank v2 selects on native accuracy.
# Executed on Linux against the current native cohort (classification, graph and
# OPLS R gates); not a universal or full-B01 claim.
.nirs4all_portable_owner_profile_classification <- "portable_linear_classification.v1"
# Regression feature branch -> merge: features -> terminal model, and standalone
# regression train augmentation / MBPLS block_sizes / GroupSparsePLS
# group_assignment (owner profile `feature_graph.v1`: DAG per-estimator graph; the
# join exists only for a branch). The feature-branch part was executed on Linux
# against the current native cohort; the standalone train-augmentation and
# MBPLS/GroupSparsePLS fit-input part is still source-only, awaiting the next
# pooled wave. The complete original definition is forwarded: topology,
# augmentation kinds/values/seeds, fit-input arrays and parameters stay owner work.
.nirs4all_portable_owner_profile_feature <- "portable_feature_regression.v1"

# Temporary residual B01: profiles declared by the parsed recipe that the
# linear owner route cannot carry today. Each entry is a REQUIRED later
# migration inside the same consolidation goal, not a permanent exception and
# not a claim of DAG/native parity. Selection is by declared profile only.
.nirs4all_portable_residual_b01 <- list(
  feature_branch = list(
    reason = paste("branch + merge: features combined with a classifier, training",
      "augmentation or extra fit inputs: the owner graph profile",
      "portable_feature_regression.v1 qualifies regression feature branches only."),
    next_seam = paste("Combined owner profile (graph classification, augmentation or",
      "fit-input transport) once each qualifies; a feature branch alone is",
      "owner-executed.")),
  train_augmentation = list(
    reason = paste("train_augmentation combined with a classifier: the owner graph",
      "profile portable_feature_regression.v1 qualifies regression only",
      "(standalone regression augmentation is owner-executed)."),
    next_seam = "Graph classification profile; then drop this combination."),
  extra_fit_inputs = list(
    reason = paste("n4m.MBPLS block_sizes / n4m.GroupSparsePLS group_assignment",
      "combined with a feature branch: the owner graph profile refuses fit inputs",
      "with a branch (standalone regression fit inputs are owner-executed)."),
    next_seam = "Owner branch plus fit-input profile once it qualifies."))

.nirs4all_portable_profile <- function(plan) {
  learner <- plan$learner
  classifier <- identical(learner$learner, "sparse_pls_da")
  branch <- any(vapply(plan$preprocessing, function(step)
    identical(step$type, "FeatureConcat"), logical(1)))
  others <- c(
    if (length(plan$augmentations)) "train_augmentation",
    if (identical(learner$learner, "n4m_method") &&
        learner$method %in% c("mb_pls", "group_sparse_pls")) "extra_fit_inputs")
  # Standalone regression feature branch, train augmentation and fit inputs are
  # owner-executed (profile portable_feature_regression.v1). Each of them stays a
  # declared residual only in the combinations the owner refuses until a combined
  # profile qualifies: with a classifier, or (augmentation / fit inputs) with a
  # feature branch.
  topology <- classifier || branch
  keys <- c(if (branch && (classifier || length(others))) "feature_branch",
            if (topology) others)
  if (!length(keys))
    return(list(route = "owner",
                name = if (classifier) .nirs4all_portable_owner_profile_classification else
                  if (branch || length(others)) .nirs4all_portable_owner_profile_feature else
                  .nirs4all_portable_owner_profile,
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

.nirs4all_portable_request <- function(source, dataset = NULL,
                                       profile = .nirs4all_portable_owner_profile) {
  request <- list(schema_version = 1L, profile = profile,
                  definition = .nirs4all_portable_document(source))
  if (!is.null(dataset)) request$dataset <- dataset
  request
}

# Dataset forms. Plain list/matrix: row-scoped positional execution IDs (the
# owner marks them positional; row names are not identity). Formats-backed:
# declared sample IDs and axis facts at full precision, plus metadata/provenance.
# Record-backed: the complete IO record, projected by the owner or refused by
# name (classification refuses it until Core carries IO class labels). Nothing
# is reassembled or inferred here.
#
# Classification keeps R's own class codec and sends only the existing dense `y`
# wire as nonnegative integral class ids: `as.integer(factor) - 1L` in the
# factor's own (possibly unsorted) level order, or, for numeric codes,
# `match(y, sort(unique(y))) - 1L`. The class names and numeric type stay in R
# (`class_values`) and are only looked up when decoding the owner's ids; there is
# no label-domain field on the wire.
.nirs4all_portable_owner_dataset <- function(dataset, classification = FALSE) {
  if (inherits(dataset, "nirs4all_dataset") && is.null(dataset$X) &&
      !is.null(dataset$record))
    return(list(payload = dataset$record, core_cli = dataset$core_cli,
                class_values = NULL))
  data <- nirs4all_portable_dataset(dataset, classification = classification)
  payload <- list(
    X = lapply(seq_len(nrow(data$X)), function(i) as.list(as.numeric(data$X[i, ]))),
    y = if (classification) as.list(as.integer(data$y) - 1L) else
      as.list(as.numeric(data$y)))
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
  list(payload = payload, core_cli = NULL, class_values = data$class_values)
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

# Projection only: field renaming, vector typing and, for classification, the
# lookup `class_values[id + 1L]` of owner class ids. No score, rank, winner,
# argmax or class count is computed here; `unseen_validation_classes` and the
# owner evidence are copied untouched.
.nirs4all_portable_project <- function(response,
                                       profile = .nirs4all_portable_owner_profile,
                                       class_values = NULL) {
  need <- function(value, what) {
    if (is.null(value)) stop(paste("owner response lacks", what), call. = FALSE)
    value
  }
  classification <- identical(profile, .nirs4all_portable_owner_profile_classification)
  if (!is.list(response) || !identical(response$schema_version, 1L) ||
      !identical(response$profile, profile) ||
      !(is.character(response$mode) && length(response$mode) == 1L &&
        response$mode %in% c("training", "single_holdout")))
    stop("unsupported portable owner response schema, profile or mode", call. = FALSE)
  integers <- function(value) as.integer(unlist(value, use.names = FALSE))
  numbers <- function(value) as.numeric(unlist(value, use.names = FALSE))
  decode <- function(value) {
    if (!classification) return(numbers(value))
    if (is.null(class_values))
      stop("the class domain of this dataset is owner-defined and cannot be decoded by R",
           call. = FALSE)
    ids <- integers(value)
    labels <- class_values[ids + 1L]
    if (anyNA(ids) || any(ids < 0L) || anyNA(labels))
      stop("owner returned a class id outside the declared class domain", call. = FALSE)
    unname(labels)   # as the legacy render_labels: values only, never input names
  }
  variant <- function(entry) {
    head <- list(n_components = if (is.null(entry$n_components)) NA_integer_ else
      as.integer(entry$n_components))
    score <- if (classification)
      list(accuracy = as.numeric(need(entry$accuracy, "accuracy"))) else
      list(rmse = as.numeric(need(entry$rmse, "rmse")))
    out <- c(head, score, list(predictions = decode(need(entry$predictions, "predictions")),
      params = entry$params, ordinal = entry$ordinal, variant_id = entry$variant_id))
    # Optional owner block widths of the feature join (feature-branch profile
    # only): object keyed by join id, ordered {branch_id, column_count}; copied
    # untouched, never computed, relabelled or rehashed here.
    if (!is.null(entry$feature_blocks)) out$feature_blocks <- entry$feature_blocks
    out
  }
  split <- need(response$split, "split")
  evaluation <- need(response$evaluation, "evaluation")
  result <- list(name = response$name, rows = as.integer(need(response$rows, "rows")),
       cols = as.integer(need(response$cols, "cols")),
       split = list(kind = split$kind, trainIndices = integers(split$trainIndices),
                    testIndices = integers(split$testIndices)),
       preprocessing = response$preprocessing,
       variants = lapply(need(response$variants, "variants"), variant),
       selected = variant(need(response$selected, "selected")),
       targets = decode(need(response$targets, "targets")),
       evaluation = list(scope = evaluation$scope,
                         independent_test = evaluation$independent_test),
       mode = response$mode, identities = response$identities, owner = response$owner,
       profile = list(route = "owner", name = response$profile,
                      owner_profile = response$owner_profile,
                      residual = character(), reasons = character(),
                      next_seams = character()))
  if (classification) {
    result$unseen_validation_classes <- need(response$unseen_validation_classes,
                                             "unseen_validation_classes")
    result$class_values <- class_values
  }
  result
}

.nirs4all_portable_run_owner <- function(source, dataset, core_cli, methods_library,
                                         profile = .nirs4all_portable_owner_profile) {
  classification <- identical(profile, .nirs4all_portable_owner_profile_classification)
  marshalled <- .nirs4all_portable_owner_dataset(dataset, classification)
  if (is.null(core_cli)) core_cli <- marshalled$core_cli
  request <- .nirs4all_portable_request(source, marshalled$payload, profile)
  .nirs4all_portable_project(.nirs4all_portable_owner_call(
    "portable-pipeline-run", request, core_cli, methods_library),
    profile, marshalled$class_values)
}
