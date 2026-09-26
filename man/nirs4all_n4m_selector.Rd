\name{nirs4all_n4m_selector}
\alias{nirs4all_n4m_selector}
\title{Define a train-fitted native n4m variable selector}
\usage{
nirs4all_n4m_selector(method, n_components = 2L, params = list())
}
\arguments{
\item{method}{A supported n4m dispatcher selector name ending in \code{_select}.}
\item{n_components}{Positive native component count.}
\item{params}{Named list of native method parameters.}
}
\description{
The native selector sees only the training matrix and target. Its selected
indices are validated and stored in the fitted pipeline. Later matrices are
projected in original spectral order. The constructor accepts the native
dispatcher selector vocabulary; cross-language parity must be established
separately for each method.
}
