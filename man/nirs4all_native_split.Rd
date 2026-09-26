\name{nirs4all_native_split}
\alias{nirs4all_native_split}
\title{Split sample identities with n4m's native engine}
\description{Runs one n4m native split and returns its ordered train/test indices with corresponding sample IDs. String group labels are encoded as exact integer categories for the native engine; their original values remain available in the caller's data.}
\usage{nirs4all_native_split(splitter, X, y = NULL, sample_ids = NULL,
  group_ids = NULL, fold = 1L)}
\arguments{
\item{splitter}{A \code{nirs4all_native_splitter()} specification.}
\item{X}{Finite numeric samples-by-features matrix.}
\item{y}{Finite numeric target vector for target-aware kinds.}
\item{sample_ids}{Optional unique sample IDs aligned with \code{X}.}
\item{group_ids}{Aligned character/factor group labels for group fold kinds only.}
\item{fold}{One-based fold number for fold kinds.}
}
\value{Native train/test indices and corresponding sample IDs.}
