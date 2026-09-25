\name{nirs4all_import_trained_pipeline}
\alias{nirs4all_import_trained_pipeline}
\title{Import a trained native pipeline from R or Python}
\usage{
nirs4all_import_trained_pipeline(source)
}
\arguments{
\item{source}{JSON text or a path to a JSON document.}
}
\value{A fitted pipeline accepted by \code{nirs4all_predict()}.}
\description{Validates the closed portable envelope, recipe, fitted references, payload hash and native N4MM descriptor before returning a predictor. The source must be trusted as model data; no host-language code is deserialized.}
