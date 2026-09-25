\name{nirs4all_emsc}
\alias{nirs4all_emsc}
\title{Define a train-fitted Extended Multiplicative Scatter Correction step}
\usage{nirs4all_emsc(degree = 2L)}
\arguments{\item{degree}{Positive polynomial degree; at least \code{degree + 2} features are required.}}
\value{A \code{nirs4all_step} definition.}
\description{Uses native n4m EMSC. Each fit learns a reference solely from its training rows; validation and prediction reuse that reference.}
