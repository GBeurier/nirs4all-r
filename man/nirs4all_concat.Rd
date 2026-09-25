\name{nirs4all_concat}
\alias{nirs4all_concat}
\title{Concatenate parallel preprocessing branches}
\description{Each branch starts from the same input matrix. Train-fitted states,
such as MSC references, are learned independently on the training rows of
their branch. Output columns are prefixed by the branch name.}
\usage{
nirs4all_concat(branches)
}
\arguments{
\item{branches}{Named list of at least two non-empty lists of nirs4all steps.}
}
\value{A composable nirs4all preprocessing step.}
