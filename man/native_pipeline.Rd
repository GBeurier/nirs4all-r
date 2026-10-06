\name{nirs4all_run_pipeline}
\alias{nirs4all_run_pipeline}
\alias{nirs4all_pipeline_predict}
\alias{nirs4all_pipeline_export}
\alias{nirs4all_pipeline_load}
\alias{nirs4all_pipeline_retrain}
\title{Catalog native pipeline workflows}
\description{Transport a finite catalog-native recipe to Core and DAG for cross-validation, selection, full refit and prediction from captured native state.}
\usage{
nirs4all_run_pipeline(dataset, pipeline, source_id = "spectra",
 methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
 run_id = paste0("run:r:pipeline:", basename(tempfile())),
 cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive"))
nirs4all_pipeline_predict(object, X, sample_ids = NULL,
 methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
 cli = object$cli)
nirs4all_pipeline_export(object, path)
nirs4all_pipeline_load(path,
 cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive"))
nirs4all_pipeline_retrain(object, dataset, ...)
}
\arguments{
\item{dataset}{A public dataset record or dataset object.}
\item{pipeline}{Named list containing steps and candidates. Steps specify method_id, role and a named params list; candidates are parameter lists for the final model.}
\item{source_id}{Numeric source identity.}
\item{methods_library}{Path to the native Methods shared library.}
\item{run_id}{Fresh campaign identity.}
\item{cli}{Core native CLI executable.}
\item{object}{A native pipeline returned by run or load.}
\item{X}{Numeric prediction matrix.}
\item{sample_ids}{Prediction sample identities.}
\item{path}{Native JSON envelope file. Export refuses overwrite.}
\item{...}{Options for a fresh training campaign.}
}
\details{The transport is nirs4all.native-pipeline.v1 JSON containing a native Package V2. It is separate from .n4a archive transport. Package fingerprints retain exact native JSON bytes. Numerical fitting and cold prediction execute in native Methods and DAG. Dependent origins and groups require declared leakage-safe folds. This API does not promise arbitrary Python model portability.}
\value{Run, load and retrain return a native pipeline; predict returns native replay outputs; export invisibly returns its path.}
