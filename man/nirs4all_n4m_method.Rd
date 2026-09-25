\name{nirs4all_n4m_method}
\alias{nirs4all_n4m_method}
\title{Native n4m linear-method controller}
\usage{
nirs4all_n4m_method(method, n_components = 2L, params = list())
}
\arguments{
\item{method}{One of \code{ridge}, \code{ridge_pls}, \code{robust_pls},
\code{cppls}, \code{sparse_simpls}, \code{ecr},
\code{continuum_regression}, or \code{mir_pls}.}
\item{n_components}{Positive component count; ignored by ridge's solver.}
\item{params}{Named method-specific scalar parameter list.}
}
\description{
Exposes MethodResult regressors whose fitted coefficients obey
\code{(X - x_mean) \%*\% coefficients + y_mean}. Other n4m methods may need
algorithm-specific prediction state and are deliberately excluded.
MethodResult coefficients are wrapped as N4MM affine predictor bytes when
serialized. These bytes preserve prediction, not the original fitting method.
}
