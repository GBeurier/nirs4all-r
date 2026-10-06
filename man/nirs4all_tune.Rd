\name{nirs4all_tune}
\alias{nirs4all_generate}
\alias{nirs4all_tune}
\alias{nirs4all_tuning_predict}
\alias{nirs4all_resume_tuning}
\alias{nirs4all_tuning_export}
\alias{nirs4all_load_tuning}
\title{Generate constrained candidates and run native resumable tuning}
\usage{
nirs4all_generate(choices, strategy = "cartesian", constraints = list(), count = NULL, seed = 0, max_variants = 10000,
    cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive"))

nirs4all_tune(data, trials = 8L, seed = 91L, sampler = "random", metric = "rmse", source_id = "spectra",
    checkpoint = NULL, methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
    archive = tempfile("nirs4all-tuning-", fileext = ".n4a"), run_id = paste0("run:tuning:",
        basename(tempfile())), cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive"))

nirs4all_tuning_predict(object, X, ...)

nirs4all_resume_tuning(object, data, trials, ...)

nirs4all_tuning_export(object, directory)

nirs4all_load_tuning(directory, cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive"))
}
\arguments{
  \item{choices}{Named lists of candidate values, including structured choices.}
  \item{strategy}{Native generation strategy: cartesian, zip or random.}
  \item{constraints}{Native constraint record, such as excluded combinations of dimension labels.}
  \item{count}{Optional number of variants for native random selection.}
  \item{seed}{Nonnegative exactly representable numeric seed in 0 through 2^53-1.}
  \item{max_variants}{Native upper bound on generated variants.}
  \item{cli}{Core archive executable, or DAG-ML executable for the local workflow. Native objects retain the supplied runtime path.}
  \item{data}{IO dataset record, dataset object, or native JSON dataset file.}
  \item{trials}{Total trial budget, an integer from 1 to 256.}
  \item{sampler}{Native tuning sampler identifier.}
  \item{metric}{Native selection metric identifier.}
  \item{source_id}{Selected dense source identifier; for structured uncertainty, optional source selection.}
  \item{checkpoint}{Optional previous tuning result or checkpoint archive.}
  \item{methods_library}{Path to the installed native libn4m shared library.}
  \item{archive}{New output native archive path.}
  \item{run_id}{Native run identifier.}
  \item{object}{Workflow, tuning result, multimodal predictor or result view returned by the corresponding API.}
  \item{X}{Finite samples-by-features matrix, or a structured dataset on uncertainty routes.}
  \item{...}{Additional arguments forwarded to the corresponding native operation.}
  \item{directory}{New export directory, or an existing exported directory when loading.}
}
\description{Delegate candidate generation and OOF hyperparameter search to Core and DAG-ML. The bounded dense regression study searches PLS components 1 through 3. Resume accepts a total trial budget and preserves completed trials. Seeds must be exact nonnegative integers no greater than 2^53-1; generated variant seeds are returned as exact decimal strings. Exports preserve native archive and training identities.}
\value{The corresponding native workflow, tuning study, calibrated result, dataset, predictor, result view, or invisible output path.}
\seealso{\code{\link{nirs4all_core_archive}}}
