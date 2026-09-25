\name{nirs4all_parsnip_classifier}
\alias{nirs4all_parsnip_classifier}
\title{Optional parsnip classification controller}
\usage{
nirs4all_parsnip_classifier(spec)
}
\arguments{
\item{spec}{A \code{parsnip} classification \code{model_spec} with an engine selected.}
}
\description{
Fits a separate model in each training scope. Class predictions are factors;
probability columns must match the training class labels and are validated by
\code{nirs4all_predict_proba()}. The fitted engine remains an R-specific artifact.
}
