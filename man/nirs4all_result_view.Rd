\name{nirs4all_result_view}
\alias{nirs4all_result_view}
\alias{nirs4all_result_compare}
\alias{nirs4all_result_predictions}
\alias{nirs4all_open_experiment}
\alias{nirs4all_save_experiment}
\title{Inspect native results and save verified experiment directories}
\usage{
nirs4all_result_view(object)

nirs4all_result_compare(object, variant_id = NULL, partition = NULL)

nirs4all_result_predictions(object, variant_id = NULL, partition = NULL, fold_id = NULL)

nirs4all_open_experiment(directory, cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive"))

nirs4all_save_experiment(native_directory, destination, model_archive = NULL, cli = Sys.getenv("NIRS4ALL_CORE_CLI",
    "nirs4all-core-archive"))
}
\arguments{
  \item{object}{Workflow, tuning result, multimodal predictor or result view returned by the corresponding API.}
  \item{variant_id}{Optional exact candidate identifier to select.}
  \item{partition}{Optional native result partition to select.}
  \item{fold_id}{Optional native fold identifier to select.}
  \item{directory}{New export directory, or an existing exported directory when loading.}
  \item{cli}{Core archive executable, or DAG-ML executable for the local workflow. Native objects retain the supplied runtime path.}
  \item{native_directory}{Directory holding the native result manifest, score set and predictions Parquet.}
  \item{destination}{New experiment destination directory.}
  \item{model_archive}{Optional portable model archive to bind to the native results.}
}
\description{Expose stored candidate scores and identity-aligned prediction rows without calculating scores in R. Native archive objects are checked against their original training outcome. Experiment opening checks member hashes and invokes Core to verify the Parquet projection and optional model closure. Legacy local score views are explicitly marked unverified. Dataset and task display fields are descriptive and unattested.}
\value{The corresponding native workflow, tuning study, calibrated result, dataset, predictor, result view, or invisible output path.}
\seealso{\code{\link{nirs4all_core_archive}}}
