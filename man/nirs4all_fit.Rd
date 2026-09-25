\name{nirs4all_fit}
\alias{nirs4all_fit}
\alias{nirs4all_predict}
\alias{predict.nirs4all_fitted}
\title{Fit and predict with a local R pipeline}
\description{Applies n4m preprocessing and delegates model fitting and prediction to the selected controller. This local fit/predict path is not a DAG-ML CV or HPO campaign.}
\usage{
nirs4all_fit(pipeline, X, y)
nirs4all_predict(object, X)
\method{predict}{nirs4all_fitted}(object, newdata, ...)
}
\arguments{
\item{pipeline}{Pipeline definition.}
\item{X}{Finite numeric samples-by-features matrix.}
\item{y}{Finite numeric target vector, one element per row.}
\item{object}{Fitted pipeline.}
\item{newdata}{Prediction matrix.}
\item{...}{Unused; retained for the S3 generic.}
}
\value{\code{nirs4all_fit} returns a fitted pipeline; predict functions return a numeric vector.}
