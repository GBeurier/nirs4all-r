\name{nirs4all_dataset}
\alias{nirs4all_dataset}
\alias{nirs4all_multimodal_fit}
\alias{nirs4all_multimodal_export}
\alias{nirs4all_multimodal_load}
\alias{nirs4all_multimodal_archive_predict}
\title{Construct aligned datasets and replay complete multimodal state}
\usage{
nirs4all_dataset(value, core_cli = Sys.which("nirs4all-core-archive"))

nirs4all_multimodal_fit(recipe, dataset, core_cli = Sys.which("nirs4all-core-archive"))

nirs4all_multimodal_export(object, path = NULL)

nirs4all_multimodal_load(value, core_cli = Sys.which("nirs4all-core-archive"))

nirs4all_multimodal_archive_predict(path, dataset, core_cli = Sys.which("nirs4all-core-archive"), dag_cli = Sys.which("dag-ml-cli"),
    workdir = tempfile("nirs4all-raw-archive-"), adapter_script = system.file("adapters", "nirs4all_core_multimodal_adapter.R",
        package = "nirs4all"))
}
\arguments{
  \item{value}{Native dataset or predictor envelope, or its JSON file.}
  \item{core_cli}{Core archive executable; an executable name is resolved through PATH.}
  \item{recipe}{Complete native multimodal recipe for the supported four-source profile.}
  \item{dataset}{IO dataset record, dataset object, or native JSON dataset file.}
  \item{object}{Workflow, tuning result, multimodal predictor or result view returned by the corresponding API.}
  \item{path}{New output file, or an existing native archive when loading or replaying.}
  \item{dag_cli}{Installed DAG-ML executable for native process replay.}
  \item{workdir}{New or empty native working directory to retain.}
  \item{adapter_script}{Installed persistent R adapter script for target-free archive replay.}
}
\description{Normalize a raw dataset through native IO, compose the supported four-source scaler/tensor-PCA/mixed-column/Ridge profile through n4m, and persist complete native N4MF state with its IO schemas. Prediction requires a target-free prediction cohort. Native archive replay additionally uses Core and a persistent DAG-ML R adapter; this process route requires POSIX. Native numerical algorithms and parsers stay in their upstream packages.}
\value{The corresponding native workflow, tuning study, calibrated result, dataset, predictor, result view, or invisible output path.}
\seealso{\code{\link{nirs4all_core_archive}}}
