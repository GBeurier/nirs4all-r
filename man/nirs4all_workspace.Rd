\name{nirs4all_workspace}
\alias{nirs4all_open_workspace}
\alias{nirs4all_workspace_runs}
\alias{nirs4all_workspace_predictions}
\alias{nirs4all_workspace_session}
\alias{nirs4all_workspace_predict}
\alias{nirs4all_workspace_export}
\alias{nirs4all_import_workspace}
\alias{nirs4all_workspace_close}
\title{Portable native sessions in a modern SDK workspace}
\description{Validate, query, export and import an SDK SQLite/Parquet workspace containing portable native experiment archives. Predictions execute the SDK native session without fitting.}
\usage{
nirs4all_open_workspace(directory, python = Sys.getenv("NIRS4ALL_WORKSPACE_PYTHON", "python3"))
nirs4all_workspace_runs(object)
nirs4all_workspace_predictions(object, run_id)
nirs4all_workspace_session(object, run_id)
nirs4all_workspace_predict(session, X, sample_ids)
nirs4all_workspace_export(object, destination)
nirs4all_import_workspace(archive, destination, python = Sys.getenv("NIRS4ALL_WORKSPACE_PYTHON", "python3"))
nirs4all_workspace_close(object)
}
\arguments{
\item{directory}{Existing validated workspace directory.}
\item{python}{Python executable providing nirs4all-core, the full nirs4all SDK and the native runtime.}
\item{object}{Open workspace or, for close, a workspace session.}
\item{run_id}{Native run identifier returned by the workspace.}
\item{session}{Open portable native workspace session.}
\item{X}{Complete numeric predictor matrix.}
\item{sample_ids}{Explicit sample identifiers in matrix row order.}
\item{destination}{New output archive or workspace path; existing paths are refused.}
\item{archive}{Validated exported workspace ZIP.}
}
\value{Open and import return a closeable workspace environment. Session returns a closeable session environment. Runs and predictions return SDK records; predict returns native predictions and sample identity. Export returns the archive path. Close invisibly returns NULL.}
\details{Requires processx and jsonlite. Each operation uses argv to invoke the public Python workspace JSON CLI and closes SDK resources before returning. Closing a workspace also closes its child sessions. The workspace contains native models portable across qualified runtimes; arbitrary foreign host model weights and legacy DuckDB migration are outside this profile.}
