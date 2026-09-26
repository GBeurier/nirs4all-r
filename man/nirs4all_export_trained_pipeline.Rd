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
\description{Serializes a portable n4m recipe, fitted preprocessing state and the exact N4MM model payload in one JSON document. No RDS, Python pickle or executable code is included. Versions 1--5 cover native PLS, sparse PLS-DA, selector and qualified affine profiles. Version 6 combines a fitted N4MP preprocessing chain and N4MM predictor. Version 7 adds an ordered train-only \code{n4m.NativeXAugmentation} recipe to that profile. Prediction skips augmentation; retraining reapplies it before fitting preprocessing. The N4MM payload does not attest the original augmentation or fitting-method history, so the recipe is an assertion rather than cryptographic provenance.}
