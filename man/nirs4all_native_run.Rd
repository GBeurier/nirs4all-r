\name{nirs4all_native_run}
\alias{nirs4all_native_run}
\alias{nirs4all_native_predict}
\alias{nirs4all_native_retrain}
\alias{nirs4all_native_export}
\alias{nirs4all_native_load}
\title{Run and replay the shared native archive workflow}
\usage{
nirs4all_native_run(dataset, archive, source_id = "spectra", components = c(1L, 2L), preprocessing = "snv_savgol", methods_library = Sys.getenv("N4M_LIBRARY_PATH",
    Sys.getenv("N4M_LIB_PATH")), run_id = paste0("run:r:", basename(tempfile())), results_directory = paste0(archive,
    ".results"), cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive"))

nirs4all_native_predict(object, X, sample_ids = NULL, methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
    cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive"))

nirs4all_native_retrain(object, dataset, archive, ..., cli = object$cli)

nirs4all_native_export(object, directory)

nirs4all_native_load(directory, cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive"))
}
\arguments{
\item{preprocessing}{Native preprocessing recipe, including the raw baseline and the SNV/Savitzky-Golay profile.}
  \item{dataset}{IO dataset record, dataset object, or native JSON dataset file.}
  \item{archive}{New output native archive path.}
  \item{source_id}{Selected dense source identifier; for structured uncertainty, optional source selection.}
  \item{components}{Distinct positive PLS component counts to compare by native OOF selection.}
  \item{methods_library}{Path to the installed native libn4m shared library.}
  \item{run_id}{Native run identifier.}
  \item{results_directory}{Directory for the native result triple emitted by the campaign.}
  \item{cli}{Core archive executable, or DAG-ML executable for the local workflow. Native objects retain the supplied runtime path.}
  \item{object}{Workflow, tuning result, multimodal predictor or result view returned by the corresponding API.}
  \item{X}{Finite samples-by-features matrix, or a structured dataset on uncertainty routes.}
  \item{sample_ids}{Ordered unique sample IDs. Structured uncertainty inputs use their own IDs instead.}
  \item{...}{Additional arguments forwarded to the corresponding native operation.}
  \item{directory}{New export directory, or an existing exported directory when loading.}
}
\description{Transport an IO-owned dataset to the Core dense regression profile. Native DAG-ML owns CV, OOF selection and full-data refit; Methods owns SNV, Savitzky-Golay and PLS. Predictions use the portable archive without fitting. Export writes path-free metadata and an archive through Core's exclusive directory publisher. Loading verifies metadata against the archive's training outcome. Objects retain their CLI for subsequent operations.}
\value{The corresponding native workflow, tuning study, calibrated result, dataset, predictor, result view, or invisible output path.}
\seealso{\code{\link{nirs4all_core_archive}}}
