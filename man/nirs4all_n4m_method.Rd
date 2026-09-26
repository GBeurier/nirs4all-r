\name{nirs4all_n4m_method}
\alias{nirs4all_n4m_method}
\title{Native n4m linear-method controller}
\usage{
nirs4all_n4m_method(method, n_components = 2L, params = list())
}
\arguments{
\item{method}{One of \code{ridge}, \code{ridge_pls}, \code{robust_pls},
\code{cppls}, \code{sparse_simpls}, \code{ecr},
\code{continuum_regression}, \code{mir_pls}, \code{fused_sparse_pls},
\code{bagging_pls}, \code{boosting_pls},
\code{random_subspace_pls}, \code{n_pls}, or \code{mb_pls}.}
\item{n_components}{Positive component count; ignored by ridge's solver.}
\item{params}{Named method-specific parameter list. N-PLS requires
positive \code{mode_j} and \code{mode_k} whose product equals the fitted feature
width; MB-PLS requires positive integer \code{block_sizes} summing to the
fitted feature width; boosting \code{learning_rate} must be in \code{(0, 1]}.}
}
\description{
Exposes MethodResult regressors with affine held-out prediction. Most use
\code{(X - x_mean) \%*\% coefficients + y_mean}; MB-PLS uses original-scale
coefficients and an explicit intercept. Other n4m methods may need
algorithm-specific prediction state and are deliberately excluded.
MethodResult coefficients are wrapped as N4MM affine predictor bytes when
serialized. These bytes preserve prediction, not the original fitting method.
}
