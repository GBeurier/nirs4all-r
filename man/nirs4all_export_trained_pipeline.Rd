\name{nirs4all_export_trained_pipeline}
\alias{nirs4all_export_trained_pipeline}
\title{Export a trained native pipeline for R/Python replay}
\usage{
nirs4all_export_trained_pipeline(object, file = NULL)
}
\arguments{
\item{object}{A fitted native n4m PLS pipeline.}
\item{file}{Optional output path. If omitted, returns JSON text.}
}
\value{JSON text, invisibly when \code{file} is supplied.}
\description{Serializes a portable n4m recipe, fitted MSC/EMSC references and the exact N4MM model payload in one JSON document. No RDS, Python pickle or executable code is included. Only the n4m PLS recipe accepted by \code{nirs4all_export_pipeline()} is currently supported. The JSON recipe also permits fitting the pipeline again in either language.}
