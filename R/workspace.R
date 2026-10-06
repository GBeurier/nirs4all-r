# Modern SDK workspace access. Each argv command owns and closes its resources.
.nirs4all_workspace_call <- function(python, operation, fields) {
  if (!requireNamespace("processx", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE))
    stop("workspace access requires processx and jsonlite", call. = FALSE)
  input <- tempfile("workspace-input-", fileext = ".json")
  output <- tempfile("workspace-output-", fileext = ".json")
  on.exit(unlink(c(input, output)), add = TRUE)
  request <- c(list(schema = "nirs4all.workspace-command.v1", operation = operation), fields)
  writeLines(jsonlite::toJSON(request, auto_unbox = TRUE, null = "null", digits = NA), input, useBytes = TRUE)
  process <- processx::run(python, c("-m", "nirs4all_core.workspace_cli", "--input", input, "--output", output),
                          error_on_status = FALSE, timeout = 120000)
  if (!file.exists(output)) stop(paste("workspace bridge failed:", process$stderr), call. = FALSE)
  response <- jsonlite::fromJSON(output, simplifyVector = FALSE)
  if (!isTRUE(response$ok)) stop(response$error, call. = FALSE)
  if (process$status != 0L) stop("workspace bridge returned an inconsistent status", call. = FALSE)
  response$result
}

.nirs4all_workspace_open <- function(object) {
  if (!inherits(object, "nirs4all_workspace") || isTRUE(object$closed)) stop("workspace is closed or invalid", call. = FALSE)
}

#' Open a validated SDK SQLite/Parquet workspace
#' @param directory Workspace directory.
#' @param python Python executable providing nirs4all-core and the full SDK.
#' @export
nirs4all_open_workspace <- function(directory, python = Sys.getenv("NIRS4ALL_WORKSPACE_PYTHON", "python3")) {
  directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  native <- .nirs4all_workspace_call(python, "open", list(path = directory))
  object <- new.env(parent = emptyenv())
  object$path <- directory; object$python <- python; object$closed <- FALSE; object$sessions <- list(); object$native <- native
  class(object) <- "nirs4all_workspace"
  object
}

#' List validated native runs in an SDK workspace
#' @export
nirs4all_workspace_runs <- function(object) {
  .nirs4all_workspace_open(object)
  .nirs4all_workspace_call(object$python, "open", list(path = object$path))$runs
}

#' Query exact native prediction rows from the SDK store
#' @export
nirs4all_workspace_predictions <- function(object, run_id) {
  .nirs4all_workspace_open(object)
  .nirs4all_workspace_call(object$python, "query", list(path = object$path, run_id = run_id))
}

#' Load a closeable portable native workspace session
#' @export
nirs4all_workspace_session <- function(object, run_id) {
  .nirs4all_workspace_open(object)
  if (!any(vapply(object$native$runs, function(run) identical(run$native_run_id, run_id), logical(1)))) stop("unknown workspace run", call. = FALSE)
  session <- new.env(parent = emptyenv()); session$workspace <- object; session$run_id <- run_id; session$closed <- FALSE
  class(session) <- "nirs4all_workspace_session"
  object$sessions[[length(object$sessions) + 1L]] <- session
  session
}

#' Predict through an SDK native session without fitting
#' @export
nirs4all_workspace_predict <- function(session, X, sample_ids) {
  if (!inherits(session, "nirs4all_workspace_session") || isTRUE(session$closed)) stop("workspace session is closed or invalid", call. = FALSE)
  .nirs4all_workspace_open(session$workspace)
  if (!is.matrix(X) || !is.numeric(X) || nrow(X) != length(sample_ids)) stop("X must be a numeric matrix with explicit sample IDs", call. = FALSE)
  .nirs4all_workspace_call(session$workspace$python, "predict", list(path = session$workspace$path, run_id = session$run_id,
      X = lapply(seq_len(nrow(X)), function(index) unname(as.list(X[index, ]))), sample_ids = unname(as.list(sample_ids))))
}

#' Export an exact immutable SDK snapshot
#' @export
nirs4all_workspace_export <- function(object, destination) {
  .nirs4all_workspace_open(object)
  .nirs4all_workspace_call(object$python, "export", list(path = object$path, destination = destination))$archive
}

#' Import an exact SDK snapshot without replacing a destination
#' @export
nirs4all_import_workspace <- function(archive, destination, python = Sys.getenv("NIRS4ALL_WORKSPACE_PYTHON", "python3")) {
  .nirs4all_workspace_call(python, "import", list(archive = archive, destination = destination))
  nirs4all_open_workspace(destination, python)
}

#' Close a workspace or its native session
#' @export
nirs4all_workspace_close <- function(object) {
  if (inherits(object, "nirs4all_workspace")) {
    for (session in object$sessions) session$closed <- TRUE
    object$sessions <- list()
  } else if (!inherits(object, "nirs4all_workspace_session")) stop("invalid workspace resource", call. = FALSE)
  object$closed <- TRUE
  invisible(NULL)
}
