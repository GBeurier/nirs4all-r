\name{nirs4all_export_trained_pipeline}
\alias{nirs4all_export_trained_pipeline}
\title{Export a trained native pipeline for R/Python replay}
\usage{
nirs4all_export_trained_pipeline(object, file = NULL, allow_training_rows = FALSE)
}
\arguments{
\item{object}{A fitted native n4m PLS, affine MethodResult, sparse PLS-DA or generic n4m role pipeline.}
\item{file}{Optional output path. If omitted, returns JSON text.}
\item{allow_training_rows}{Some fitted role steps (kernel and local models, for example) keep training rows in their state. Their export is refused unless this is \code{TRUE}, which copies those rows into the envelope. Other envelope versions carry no training rows.}
}
\value{JSON text, invisibly when \code{file} is supplied.}
\description{Serializes a portable n4m recipe, fitted preprocessing state and the exact N4MM model payload in one JSON document. No RDS, Python pickle or executable code is included. Versions 1--5 cover native PLS, sparse PLS-DA, selector and qualified affine profiles. Version 6 combines a fitted N4MP preprocessing chain and N4MM predictor. Version 7 adds an ordered train-only \code{n4m.NativeXAugmentation} recipe to that profile. Prediction skips augmentation; retraining reapplies it before fitting preprocessing. The N4MM payload does not attest the original augmentation or fitting-method history, so the recipe is an assertion rather than cryptographic provenance. Version 8 covers any recipe of generic n4m role steps (\code{\link{nirs4all_fit_role_recipe}}): each fitted step travels as its native N4ME state with a SHA-256 and a flag saying whether it keeps training rows, readable by every n4m binding; the input column names travel too when the pipeline was fitted on a matrix with column names.}
