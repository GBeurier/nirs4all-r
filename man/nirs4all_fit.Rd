\name{nirs4all_fit}
\alias{nirs4all_fit}
\alias{nirs4all_predict}
\alias{nirs4all_predict_proba}
\alias{predict.nirs4all_fitted}
\title{Fit and predict with a local R pipeline}
\description{Applies n4m preprocessing and delegates model fitting and prediction to the selected controller. This local fit/predict path is not a DAG-ML CV or HPO campaign.}
\usage{
nirs4all_fit(pipeline, X, y = NULL)
nirs4all_predict(object, X)
nirs4all_predict_proba(object, X)
\method{predict}{nirs4all_fitted}(object, newdata, ...)
}
\arguments{
\item{pipeline}{Pipeline definition.}
\item{X}{Finite numeric samples-by-features matrix or a
\code{nirs4all_dataset} from \code{nirs4all_from_formats()}.}
\item{y}{Finite numeric regression targets or factor/character class labels, one element per row; omit for a dataset.}
\item{object}{Fitted pipeline.}
\item{newdata}{Prediction matrix or \code{nirs4all_dataset}.}
\item{...}{Unused; retained for the S3 generic.}
}
\value{\code{nirs4all_fit} returns a fitted pipeline. Regression prediction returns a numeric vector; classification prediction returns a factor, and \code{nirs4all_predict_proba} returns a samples-by-classes probability matrix.}
