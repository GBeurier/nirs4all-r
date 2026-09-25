\name{nirs4all_export_pipeline}
\alias{nirs4all_export_pipeline}
\title{Export a qualified R pipeline as a cross-language recipe}
\usage{
nirs4all_export_pipeline(pipeline, format = c("json", "yaml"),
                        file = NULL, name = "pipeline")
}
\arguments{
\item{pipeline}{An unfitted \code{nirs4all_pipeline()}.}
\item{format}{\code{"json"} or \code{"yaml"}.}
\item{file}{Optional output path. If omitted, serialized text is returned.}
\item{name}{Pipeline name in the exported definition.}
}
\value{Serialized text, invisibly if \code{file} is provided.}
\description{Exports the shared flat Core portable subset (default SNV, unit-spacing Savitzky-Golay and default native PLS) and Python's named feature-branch plus \code{merge: features} syntax for those operators. Branch recipes are checked against Python but are not yet qualified in Core/WASM. Unsupported n4m steps or parameters fail explicitly. This is a fit recipe, not trained model state.}
