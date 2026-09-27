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
\description{Validates the closed portable envelope, recipe, fitted references or selector indices, payload hashes and native N4MP/N4MM descriptors before returning a predictor. Version 7 requires a nonempty closed train-only augmentation prefix before the fitted N4MP chain; older schemas reject that prefix. Version 8 requires \code{n_features} to be a positive JSON integer equal to the width of the fitted states, and a classifier label table (\code{class_names}) to be a non-empty list of unique strings or finite numbers with a label for every native class ID; numeric labels stay numbers, compared by value, and an integer beyond 2^53 is refused. An envelope holding a NUL character (the JSON escape \code{\\u0000} or a NUL byte) is refused before parsing. The ordered augmentation recipe is retained for retraining but ignored during prediction. The source must be trusted as model data; no host-language code is deserialized.}
