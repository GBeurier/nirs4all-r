\name{nirs4all_torch_mlp}
\alias{nirs4all_torch_mlp}
\title{R torch multilayer-perceptron regression controller}
\description{Fits a one-hidden-layer CPU neural network with full-batch Adam and mean squared error. Feature and target scaling is estimated only from training rows. The trained module is R-specific and is serialized with torch, never with base RDS pointers alone.}
\usage{
nirs4all_torch_mlp(hidden = 32L, epochs = 100L,
                   learning_rate = 0.001, seed = 1L)
}
\arguments{
\item{hidden}{Positive hidden-layer width.}
\item{epochs}{Positive optimization epoch count.}
\item{learning_rate}{Positive Adam learning rate.}
\item{seed}{Non-negative torch random seed.}
}
\value{A learner controller accepted by \code{nirs4all_pipeline}.}
