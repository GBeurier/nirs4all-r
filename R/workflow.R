#' Run a native dense regression workflow
#'
#' The supported profile applies the requested preprocessing inside each
#' native DAG fold, compares PLS component counts by OOF RMSE, and refits the
#' selected pipeline on all training rows. `workdir` must be retained for
#' prediction or exported with [nirs4all_workflow_export()].
#'
#' @param X Finite samples-by-features matrix.
#' @param y Finite numeric target aligned to X.
#' @param components At least two distinct positive PLS component counts.
#' @param preprocessing Ordered character vector of `"snv"` or `"msc"`.
#' @param folds Number of CV folds.
#' @param sample_ids Optional unique IDs aligned to X.
#' @param cli Path to `dag-ml-cli`.
#' @param workdir New or empty directory for the native campaign.
#' @param root_seed Native DAG seed.
#' @return A `nirs4all_workflow` with native outcome and selected variant.
nirs4all_run <- function(X, y, components = c(1L, 2L, 3L),
                         preprocessing = "snv", folds = 5L,
                         sample_ids = NULL, cli = Sys.which("dag-ml-cli"),
                         workdir = tempfile("nirs4all-workflow-"),
                         root_seed = 1L) {
  X <- nirs4all_matrix(X)
  if (!is.numeric(y) || is.matrix(y) || length(y) != nrow(X) ||
      anyNA(y) || any(!is.finite(y)))
    stop("y must be one finite numeric target per X row", call. = FALSE)
  if (!is.numeric(components) || length(components) < 2L ||
      anyNA(components) || any(!is.finite(components)) ||
      any(components < 1L | components != floor(components)) ||
      any(components > .Machine$integer.max) || anyDuplicated(components))
    stop("components must contain at least two distinct positive integers",
         call. = FALSE)
  if (!is.character(preprocessing) || anyNA(preprocessing) ||
      !all(preprocessing %in% c("snv", "msc")))
    stop("preprocessing supports ordered 'snv' and 'msc' steps", call. = FALSE)
  if (length(preprocessing) == 0L)
    stop("preprocessing must contain at least one step", call. = FALSE)
  components <- as.integer(components)
  steps <- lapply(preprocessing, function(kind) switch(kind,
    snv = nirs4all_snv(), msc = nirs4all_msc()))
  candidates <- stats::setNames(lapply(components, function(count)
    nirs4all_pipeline(steps, nirs4all_pls(count))),
    paste0("pls_", components))
  outcome <- nirs4all_dag_cv_refit_predict(
    candidates, X, y, folds = folds, sample_ids = sample_ids,
    root_seed = root_seed, cli = cli, workdir = workdir,
    split_steps = TRUE)
  winner <- outcome$bundle$selected_variant_id
  if (!is.character(winner) || length(winner) != 1L || is.na(winner) ||
      !nzchar(winner) || !length(outcome$bundle$scores$reports))
    stop("native campaign lacks OOF selection evidence", call. = FALSE)
  structure(list(
    run_id = outcome$bundle$run_id,
    winner_variant_id = winner,
    workdir = outcome$workdir,
    outcome = outcome,
    config = list(components = components, preprocessing = preprocessing,
                  folds = folds, root_seed = root_seed)),
    class = "nirs4all_workflow")
}

#' Predict from the native full-data refit winner
nirs4all_workflow_predict <- function(object, X) {
  if (!inherits(object, "nirs4all_workflow"))
    stop("object must be a nirs4all_workflow", call. = FALSE)
  nirs4all_dag_predict(object$outcome, X)
}

#' Run a fresh native campaign on replacement data
nirs4all_workflow_retrain <- function(object, X, y, sample_ids = NULL,
                                     cli = Sys.which("dag-ml-cli"),
                                     workdir = tempfile("nirs4all-workflow-")) {
  if (!inherits(object, "nirs4all_workflow"))
    stop("object must be a nirs4all_workflow", call. = FALSE)
  do.call(nirs4all_run, c(list(X = X, y = y, sample_ids = sample_ids,
                           cli = cli, workdir = workdir), object$config))
}

#' Export a local native workflow and its refit sidecars
#'
#' The directory is a trusted local R/DAG bundle. It is not a cross-language
#' nirs4all archive.
nirs4all_workflow_export <- function(object, directory) {
  if (!inherits(object, "nirs4all_workflow"))
    stop("object must be a nirs4all_workflow", call. = FALSE)
  if (!is.character(directory) || length(directory) != 1L || is.na(directory) ||
      !nzchar(directory) || dir.exists(directory) || file.exists(directory))
    stop("directory must be a new path", call. = FALSE)
  source <- normalizePath(object$workdir, mustWork = TRUE)
  destination <- normalizePath(directory, mustWork = FALSE)
  if (startsWith(destination, paste0(source, .Platform$file.sep)))
    stop("export directory must be outside the run workdir", call. = FALSE)
  if (!dir.create(destination, recursive = TRUE))
    stop("could not create export directory", call. = FALSE)
  files <- list.files(source, all.files = TRUE, no.. = TRUE, recursive = TRUE,
                      include.dirs = TRUE)
  dir.create(file.path(destination, "workdir"))
  for (relative in files) {
    original <- file.path(source, relative)
    target <- file.path(destination, "workdir", relative)
    if (dir.exists(original)) dir.create(target, recursive = TRUE,
                                         showWarnings = FALSE)
    else {
      dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
      if (!file.copy(original, target, overwrite = FALSE))
        stop("failed to copy native workflow artifact", call. = FALSE)
    }
  }
  saved <- object
  saved$workdir <- "workdir"
  saved$outcome$workdir <- saved$workdir
  records <- saved$outcome$bundle$refit_artifacts
  for (index in seq_along(records)) {
    uri <- records[[index]]$artifact$uri
    if (is.character(uri) && length(uri) == 1L &&
        startsWith(uri, paste0(source, .Platform$file.sep))) {
      relative <- substring(uri, nchar(source) + 2L)
      records[[index]]$artifact$uri <- file.path("workdir", relative)
    } else if (identical(records[[index]]$artifact$backend, "rds")) {
      stop("refit sidecar lies outside the workflow workdir", call. = FALSE)
    }
  }
  saved$outcome$bundle$refit_artifacts <- records
  saveRDS(saved, file.path(destination, "workflow.rds"), version = 3L)
  invisible(destination)
}

#' Load a trusted local native workflow export
nirs4all_workflow_load <- function(directory) {
  directory <- normalizePath(directory, mustWork = TRUE)
  object <- readRDS(file.path(directory, "workflow.rds"))
  if (!inherits(object, "nirs4all_workflow") ||
      !identical(object$workdir, "workdir") ||
      !identical(object$outcome$workdir, "workdir") ||
      !identical(object$winner_variant_id,
                 object$outcome$bundle$selected_variant_id))
    stop("invalid native workflow export", call. = FALSE)
  object$workdir <- normalizePath(file.path(directory, "workdir"),
                                  mustWork = TRUE)
  object$outcome$workdir <- object$workdir
  records <- object$outcome$bundle$refit_artifacts
  for (index in seq_along(records)) {
    if (!identical(records[[index]]$artifact$backend, "rds")) next
    uri <- records[[index]]$artifact$uri
    prefix <- paste0("workdir", .Platform$file.sep, "artifacts",
                     .Platform$file.sep)
    if (!is.character(uri) || length(uri) != 1L ||
        !startsWith(uri, prefix) || grepl("(^|[/\\\\])\\.\\.([/\\\\]|$)", uri))
      stop("invalid relative refit sidecar path", call. = FALSE)
    records[[index]]$artifact$uri <- file.path(directory, uri)
  }
  object$outcome$bundle$refit_artifacts <- records
  object
}
