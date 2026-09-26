\name{nirs4all_xgboost}
\alias{nirs4all_xgboost}
\alias{nirs4all_xgboost_classifier}
\title{Optional XGBoost regression and classification controllers}
\description{Fits a fresh CPU XGBoost booster on each training scope. Classification supplies factor labels and a probability matrix with the original class levels. The fitted booster is stored as XGBoost model bytes within the R model sidecar, not as a cross-language n4m model.}
\usage{
nirs4all_xgboost(nrounds = 100L, max_depth = 6L, eta = 0.3,
                 seed = 1L, nthread = 1L)
nirs4all_xgboost_classifier(nrounds = 100L, max_depth = 6L,
                            eta = 0.3, seed = 1L, nthread = 1L)
}
\arguments{
\item{nrounds}{Positive number of boosting rounds.}
\item{max_depth}{Non-negative tree depth; zero means no depth limit.}
\item{eta}{Learning rate in (0, 1].}
\item{seed}{Non-negative XGBoost seed.}
\item{nthread}{Positive number of CPU threads.}
}
\value{A learner controller accepted by \code{nirs4all_pipeline}.}
