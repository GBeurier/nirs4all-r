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
\description{Exports only the currently shared Core portable subset: default SNV, Savitzky-Golay with unit spacing, and default native PLS settings. Unsupported n4m steps or parameters fail explicitly. This is a fit recipe, not trained model state.}
