\name{nirs4all_spa}
\alias{nirs4all_spa}
\title{Define a train-fitted n4m SPA variable-selection step}
\usage{nirs4all_spa(top_k, n_components = 2L)}
\arguments{
\item{top_k}{Number of input features to retain.}
\item{n_components}{Number of PLS components used by the n4m selector.}
}
\value{A \code{nirs4all_step} definition.}
\description{Successive Projections Algorithm selects wavelengths from the training predictors and numeric target. Validation and prediction reuse the selected column indices unchanged.}
