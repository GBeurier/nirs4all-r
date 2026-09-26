\name{nirs4all_group_sparse_pls}
\alias{nirs4all_group_sparse_pls}
\title{Native group-sparse PLS regression controller}
\usage{
nirs4all_group_sparse_pls(n_components = 2L, group_assignment,
                           group_lambda = 0.05)
}
\arguments{
\item{n_components}{Positive number of SIMPLS components.}
\item{group_assignment}{Non-negative integer group ID for each transformed
feature; optional names must match transformed feature names in order.}
\item{group_lambda}{Finite non-negative coefficient group penalty.}
}
\description{
Fits the n4m post-SIMPLS group-penalized coefficient model. This is a
predictive-coefficient approximation, not \code{sgPLS::gPLS}. Group IDs must
describe the columns after any preceding pipeline transformations.
}
