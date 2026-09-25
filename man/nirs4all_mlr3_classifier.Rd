\name{nirs4all_mlr3_classifier}
\alias{nirs4all_mlr3_classifier}
\title{Optional mlr3 classification learner controller}
\usage{
nirs4all_mlr3_classifier(learner)
}
\arguments{
\item{learner}{An untrained \code{mlr3} \code{LearnerClassif} instance supporting probability prediction.}
}
\description{
Clones the supplied learner for each fit and obtains class probabilities.
DAG-ML, not mlr3, owns the CV/OOF/refit phases. The fitted learner remains
an R-specific artifact.
}
