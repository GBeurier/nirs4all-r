\name{nirs4all_run_multimodal}
\alias{nirs4all_run_multimodal}
\alias{nirs4all_multimodal_predict}
\alias{nirs4all_native_multimodal_export}
\alias{nirs4all_native_multimodal_load}
\alias{nirs4all_multimodal_retrain}
\title{Native multimodal workflows with explicit source policies}
\description{Transport a finite catalog-native recipe to Core and DAG for cross-validation, selection, full refit and prediction from captured native state.}
\usage{
nirs4all_run_multimodal(dataset, pipeline, source_policies,
 methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
 run_id = paste0("run:r:multimodal:", basename(tempfile())),
 cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive"))
nirs4all_multimodal_predict(object, dataset,
 methods_library = NULL,
 cli = object$cli)
nirs4all_native_multimodal_export(object, path)
nirs4all_native_multimodal_load(path,
 cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive"),
  methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")))
nirs4all_multimodal_retrain(object, dataset,
 methods_library = NULL,
 run_id = paste0("run:r:retrain:", basename(tempfile())), cli = object$cli)
}
\arguments{
\item{dataset}{A public dataset v2 record, exact JSON text, or dataset object. Prediction data must be target-free.}
\item{pipeline}{Named list containing steps and candidates. Steps specify method_id, role and a named params list; candidates are parameter lists for the final model.}
\item{source_policies}{Ordered source policy lists: source_id, encoder (identity or ragged_summary) and missing_policy (reject or zero_with_indicator).}
\item{methods_library}{Path to the native Methods shared library. Prediction and retraining inherit the object runtime when NULL; an explicit path overrides it. Runtime paths are not serialized into model exports.}
\item{run_id}{Fresh campaign identity.}
\item{cli}{Core native CLI executable.}
\item{object}{A native pipeline returned by run or load.}
\item{path}{Native JSON envelope file. Export refuses overwrite.}
}
\details{The transport is nirs4all.native-multimodal.v1 JSON containing one native Package V2 per target. It is separate from .n4a archive transport. Package fingerprints retain exact native JSON bytes. Numerical fitting and cold prediction execute in native Methods and DAG. Dependent origins and groups require declared leakage-safe folds. This API does not promise arbitrary Python model portability.}
\value{Run, load and retrain return a native pipeline; predict returns native replay outputs; export invisibly returns its path.}
