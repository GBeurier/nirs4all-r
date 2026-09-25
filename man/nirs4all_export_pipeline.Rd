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
\description{Exports n4m-backed preprocessing and the qualified native PLS recipe, including Python's named feature-branch plus \code{merge: features} syntax. LSNV, RNV, area normalization, detrend, MSC and EMSC recipes are checked against the independent Python n4m binding but are not yet qualified in Core/WASM. Non-default SNV is refused because Core/WASM currently ignores its parameters. Unsupported learner settings fail explicitly. This is a fit recipe, not trained model or fitted preprocessing state.}
