\name{nirs4all_torch_mlp_classifier}
\alias{nirs4all_torch_mlp_classifier}
\title{Optional R torch multilayer-perceptron classification controller}
\usage{
nirs4all_torch_mlp_classifier(hidden = 32L, epochs = 100L,
                                 learning_rate = 0.001, seed = 1L)
}
\arguments{
\item{hidden}{Positive hidden-layer width.}
\item{epochs}{Positive number of full-batch Adam epochs.}
\item{learning_rate}{Positive Adam learning rate.}
\item{seed}{Non-negative torch seed.}
}
\description{
Fits a CPU multilayer perceptron on factor class labels using cross-entropy.
The output is a factor or a matrix of softmax class probabilities. Feature
normalization is fitted only on training rows. Weights are R-torch artifacts,
not portable Python or ONNX weights.
}
\value{A classification learner controller accepted by \code{nirs4all_pipeline}.}
