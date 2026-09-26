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
\description{Validates the closed portable envelope, recipe, fitted references or selector indices, payload hashes and native N4MP/N4MM descriptors before returning a predictor. Version 7 requires a nonempty closed train-only augmentation prefix before the fitted N4MP chain; older schemas reject that prefix. The ordered augmentation recipe is retained for retraining but ignored during prediction. The source must be trusted as model data; no host-language code is deserialized.}
