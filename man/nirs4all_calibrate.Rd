\name{nirs4all_calibrate}
\alias{nirs4all_calibrate}
\alias{nirs4all_predict_calibrated}
\alias{nirs4all_conformal_metrics}
\alias{nirs4all_export_calibrated}
\alias{nirs4all_load_calibrated}
\alias{nirs4all_robustness}
\title{Calibrate and audit frozen native regression predictions}
\usage{
nirs4all_calibrate(model, data, coverages = c(0.9), small_sample_policy = "error", source_id = "spectra",
    archive = tempfile(fileext = ".n4a"), methods_library = Sys.getenv("N4M_LIBRARY_PATH",
        Sys.getenv("N4M_LIB_PATH")), cli = Sys.getenv("NIRS4ALL_CORE_CLI"))

nirs4all_predict_calibrated(model, X, sample_ids = NULL, methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
    cli = Sys.getenv("NIRS4ALL_CORE_CLI"), source_id = NULL)

nirs4all_conformal_metrics(model, prediction, y, sample_ids, cli = Sys.getenv("NIRS4ALL_CORE_CLI"))

nirs4all_export_calibrated(model, path)

nirs4all_load_calibrated(path, cli = Sys.getenv("NIRS4ALL_CORE_CLI"))

nirs4all_robustness(model, X, y, sample_ids = NULL, scenarios = NULL, methods_library = Sys.getenv("N4M_LIBRARY_PATH",
    Sys.getenv("N4M_LIB_PATH")), cli = Sys.getenv("NIRS4ALL_CORE_CLI"), source_id = NULL)
}
\arguments{
  \item{model}{Native workflow, calibrated object, or native archive path as supported by the operation.}
  \item{data}{IO dataset record, dataset object, or native JSON dataset file.}
  \item{coverages}{Requested split conformal coverage probabilities.}
  \item{small_sample_policy}{Native small-sample policy, error or unbounded.}
  \item{source_id}{Selected dense source identifier; for structured uncertainty, optional source selection.}
  \item{archive}{New output native archive path.}
  \item{methods_library}{Path to the installed native libn4m shared library.}
  \item{cli}{Core archive executable, or DAG-ML executable for the local workflow. Native objects retain the supplied runtime path.}
  \item{X}{Finite samples-by-features matrix, or a structured dataset on uncertainty routes.}
  \item{sample_ids}{Ordered unique sample IDs. Structured uncertainty inputs use their own IDs instead.}
  \item{prediction}{Untouched result from native calibrated prediction.}
  \item{y}{Numeric target values aligned to the feature rows or explicit sample IDs.}
  \item{path}{New output file, or an existing native archive when loading or replaying.}
  \item{scenarios}{Native frozen-audit scenario records; defaults to observed and seeded Gaussian spectral noise.}
}
\description{Attach native split conformal calibration from a held-out labelled cohort to a frozen archive, replay prediction intervals, evaluate keyed truth, and audit observed or seeded spectral-noise scenarios. Native validation rejects training/calibration identity overlap and incompatible schemas. Positional prediction requires explicit sample IDs; structured datasets retain their own IDs. Metrics require untouched native JSON attached to calibration and prediction results. Robustness audits do not retrain the model.}
\value{The corresponding native workflow, tuning study, calibrated result, dataset, predictor, result view, or invisible output path.}
\seealso{\code{\link{nirs4all_core_archive}}}
