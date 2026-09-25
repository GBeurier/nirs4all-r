\name{nirs4all_parsnip}
\alias{nirs4all_parsnip}
\title{Optional parsnip regression controller}
\usage{
nirs4all_parsnip(spec)
}
\arguments{
\item{spec}{A \code{parsnip} regression \code{model_spec} with an engine selected.}
}
\description{
Accepts a fully specified \code{parsnip} model specification. The model is fitted
separately within every nirs4all training scope; preprocessing is not fitted
on validation or prediction samples. Only one-column numeric regression
predictions are supported.
}
