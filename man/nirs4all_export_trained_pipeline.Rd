\name{nirs4all_export_trained_pipeline}
\alias{nirs4all_export_trained_pipeline}
\title{Export a trained native pipeline for R/Python replay}
\usage{
nirs4all_export_trained_pipeline(object, file = NULL)
}
\arguments{
\item{object}{A fitted native n4m PLS, affine MethodResult or sparse PLS-DA pipeline.}
\item{file}{Optional output path. If omitted, returns JSON text.}
}
\value{JSON text, invisibly when \code{file} is supplied.}
\description{Serializes a portable n4m recipe, fitted preprocessing state and the exact N4MM model payload in one JSON document. No RDS, Python pickle or executable code is included. Version 1 covers n4m PLS regression; version 2 adds sparse PLS-DA with ordered class labels and a native affine N4MM predictor. Version 3 covers PLS regression with external train-fitted SPA selection. Version 4 covers PLS regression with generic native \code{n4m.Selector} steps. Version 5 covers twelve native MethodResult regressors with a prediction-only affine N4MM payload and an asserted refit recipe; the binary does not attest the fitting method. Selector indices use native ranking order and zero-based values on the wire; prediction projects them in original spectral order. The JSON recipe also permits fitting the pipeline again in either language.}
