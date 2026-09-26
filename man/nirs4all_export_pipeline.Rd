\name{nirs4all_export_pipeline}
\alias{nirs4all_export_pipeline}
\title{Export a qualified R pipeline recipe}
\usage{
nirs4all_export_pipeline(pipeline, format = c("json", "yaml"),
                        file = NULL, name = "pipeline",
                        scope = c("cross_language", "r_native"))
}
\arguments{
\item{pipeline}{An unfitted \code{nirs4all_pipeline()}.}
\item{format}{\code{"json"} or \code{"yaml"}.}
\item{file}{Optional output path. If omitted, serialized text is returned.}
\item{name}{Pipeline name in the exported definition.}
\item{scope}{\code{"cross_language"} for the qualified n4m-only recipe (the default), or \code{"r_native"} for an R-specific learner recipe. The latter is not a Python/WASM model alias and permits non-default SNV parameters.}
}
\value{Serialized text, invisibly if \code{file} is provided.}
\description{Exports n4m-backed preprocessing, including train-fitted \code{n4m.SPA} and the native dispatcher \code{n4m.Selector}, with Python's named feature-branch plus \code{merge: features} syntax. A closed \code{train_augmentation} prefix uses \code{n4m.NativeXAugmentation}, exact positional \code{values} and a required \code{seed}; R applies it only after splitting to training X. Other consumers must implement the same phase contract before claiming recipe portability. The generic selector carries its method, component count and native method parameters; it is an R/Python n4m fit recipe and does not assert per-method Core/WASM parity. The default cross-language scope accepts qualified native models and refuses non-default SNV because Core/WASM currently ignores its parameters. The R-native scope accepts selected ranger, glmnet and torch MLP controllers with explicitly R-namespaced model aliases but refuses train augmentation nodes. Those model aliases and their trained binaries are not portable to Python/WASM. This is a fit recipe, not trained model or fitted preprocessing state.}
