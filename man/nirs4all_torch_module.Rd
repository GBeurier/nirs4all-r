\name{nirs4all_torch_module}
\alias{nirs4all_torch_module}
\title{Optional R torch module regression controller}
\usage{
nirs4all_torch_module(builder, name = "custom", epochs = 100L,
                      learning_rate = 0.001, seed = 1L)
}
\arguments{
\item{builder}{Function accepting one integer \code{n_features} and returning an untrained \code{torch} \code{nn_module}.}
\item{name}{Stable model label recorded in the controller specification.}
\item{epochs}{Positive number of full-batch Adam epochs.}
\item{learning_rate}{Positive Adam learning rate.}
\item{seed}{Non-negative torch seed.}
}
\description{
The builder receives the number of input features and must return a fresh
CPU-compatible \code{torch} \code{nn_module} whose forward pass maps an
\code{N x p} float tensor to an \code{N x 1} float tensor. Each DAG-ML fold
invokes the builder anew, with independent normalization and optimizer state.
Only trusted, self-contained R builder functions should be used: they are
serialized into the R-specific process-adapter specification and are not
portable Python weights, ONNX, or N4MM artifacts.
}
