\name{nirs4all_torch_module_classifier}
\alias{nirs4all_torch_module_classifier}
\title{Optional R torch module classification controller}
\usage{
nirs4all_torch_module_classifier(builder, name = "custom",
                                  epochs = 100L,
                                  learning_rate = 0.001, seed = 1L)
}
\arguments{
\item{builder}{Function accepting integer \code{n_features} and \code{n_classes} and returning a fresh untrained CPU-compatible \code{torch} \code{nn_module}.}
\item{name}{Stable model label recorded in the controller specification.}
\item{epochs}{Positive number of full-batch Adam epochs.}
\item{learning_rate}{Positive Adam learning rate.}
\item{seed}{Non-negative torch seed.}
}
\description{
The builder must emit an \code{N x n_classes} raw-logits tensor. Each fit,
including each DAG-ML fold, receives an independent module, optimizer and
feature-normalization state. Only trusted R builder functions should be used:
the builder and weights are R-specific and are not portable Python or ONNX
artifacts.
}
\value{A classification learner controller accepted by \code{nirs4all_pipeline}.}
