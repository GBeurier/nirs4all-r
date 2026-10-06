\name{nirs4all_run}
\alias{nirs4all_run}
\alias{nirs4all_workflow_predict}
\alias{nirs4all_workflow_retrain}
\alias{nirs4all_workflow_export}
\alias{nirs4all_workflow_load}
\title{Run and persist a local native DAG regression campaign}
\usage{
nirs4all_run(X, y, components = c(1L, 2L, 3L), preprocessing = "snv", folds = 5L, sample_ids = NULL,
    cli = Sys.which("dag-ml-cli"), workdir = tempfile("nirs4all-workflow-"), root_seed = 1L)

nirs4all_workflow_predict(object, X)

nirs4all_workflow_retrain(object, X, y, sample_ids = NULL, cli = Sys.which("dag-ml-cli"), workdir = tempfile("nirs4all-workflow-"))

nirs4all_workflow_export(object, directory)

nirs4all_workflow_load(directory)
}
\arguments{
  \item{X}{Finite samples-by-features matrix, or a structured dataset on uncertainty routes.}
  \item{y}{Numeric target values aligned to the feature rows or explicit sample IDs.}
  \item{components}{Distinct positive PLS component counts to compare by native OOF selection.}
  \item{preprocessing}{Ordered local SNV/MSC steps; the shared archive workflow uses its native SNV/Savitzky-Golay profile.}
  \item{folds}{Number of cross-validation folds for the local DAG campaign.}
  \item{sample_ids}{Ordered unique sample IDs. Structured uncertainty inputs use their own IDs instead.}
  \item{cli}{Core archive executable, or DAG-ML executable for the local workflow. Native objects retain the supplied runtime path.}
  \item{workdir}{New or empty native working directory to retain.}
  \item{root_seed}{Native DAG root seed.}
  \item{object}{Workflow, tuning result, multimodal predictor or result view returned by the corresponding API.}
  \item{directory}{New export directory, or an existing exported directory when loading.}
}
\description{Compare PLS component candidates by native out-of-fold RMSE, apply SNV or MSC within each fold, and refit the selected pipeline. Prediction uses the retained full-data refit. Exported directories contain trusted local R sidecars and are relocatable; they are distinct from portable Core archives.}
\value{The corresponding native workflow, tuning study, calibrated result, dataset, predictor, result view, or invisible output path.}
\seealso{\code{\link{nirs4all_core_archive}}}
