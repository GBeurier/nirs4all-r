\name{nirs4all_native_splitter}
\alias{nirs4all_native_splitter}
\title{Specify an n4m native sample splitter}
\description{Creates a bounded splitter specification. n4m performs the numerical split. Only the three fold kinds can drive DAG cross-validation.}
\usage{nirs4all_native_splitter(kind, ...)}
\arguments{
\item{kind}{One of \code{kennard_stone}, \code{spxy}, \code{spxy_fold}, \code{spxy_group_fold}, \code{kmeans}, \code{kbins_stratified}, \code{binned_strat_group_fold}, \code{systematic_circular}, or \code{data_twinning}.}
\item{...}{Named kind-specific n4m parameters. Fold kinds require \code{n_splits}.}
}
\value{A \code{nirs4all_native_splitter} specification.}
