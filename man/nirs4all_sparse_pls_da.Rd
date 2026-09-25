\name{nirs4all_sparse_pls_da}
\alias{nirs4all_sparse_pls_da}
\title{Native n4m sparse PLS-DA classification controller}
\usage{
nirs4all_sparse_pls_da(n_components = 2L, sparsity_lambda = 0.05)
}
\arguments{
\item{n_components}{Positive number of latent components.}
\item{sparsity_lambda}{Finite non-negative regularization strength.}
}
\description{
Fits sparse PLS-DA with the native n4m kernel. Out-of-sample class decisions
use the native coefficients and centering state, as in the Python
\code{SparsePLSDAClassifier}. \code{predict_proba()} applies softmax to the
decision scores for DAG compatibility; its outputs are not calibrated probabilities.
}
