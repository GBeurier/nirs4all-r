\name{nirs4all_mlr3}
\alias{nirs4all_mlr3}
\title{Optional mlr3 regression learner controller}
\usage{
nirs4all_mlr3(learner)
}
\arguments{
\item{learner}{An untrained \code{mlr3} \code{LearnerRegr} instance.}
}
\description{
Clones the supplied learner before each fit so every fold owns independent
mutable state. DAG-ML, not mlr3, controls the CV/OOF/refit phases. Only
numeric one-target regression responses are currently supported.
}
