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
\description{Exports n4m-backed preprocessing and a qualified native learner recipe, including Python's named feature-branch plus \code{merge: features} syntax. The default cross-language scope accepts native PLS or sparse PLS-DA only and refuses non-default SNV because Core/WASM currently ignores its parameters. The R-native scope accepts selected ranger, glmnet and torch MLP controllers with explicitly R-namespaced model aliases. Those model aliases and their trained binaries are not portable to Python/WASM. This is a fit recipe, not trained model or fitted preprocessing state.}
