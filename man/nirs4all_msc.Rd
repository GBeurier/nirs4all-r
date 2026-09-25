\name{nirs4all_msc}
\alias{nirs4all_msc}
\title{Define a train-fitted Multiplicative Scatter Correction step}
\description{Learn a reference spectrum from training rows with the native n4m MSC operator, then reuse only that reference for validation and prediction. The reference is stored with the fitted R pipeline.}
\usage{nirs4all_msc()}
\value{A preprocessing step for \code{nirs4all_pipeline()}.}
