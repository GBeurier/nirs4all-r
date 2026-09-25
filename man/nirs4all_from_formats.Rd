\name{nirs4all_from_formats}
\alias{nirs4all_from_formats}
\title{Convert a nirs4all-formats spectral dataset for modelling}
\usage{
nirs4all_from_formats(source, target = NULL, signal = NULL)
}
\arguments{
\item{source}{A file path or a \code{nirs4allformats_dataset}.}
\item{target}{Name of one numeric target column, or \code{NULL} for prediction.}
\item{signal}{Optional signal name when \code{source} is a file path.}
}
\description{
The Rust-backed \code{nirs4allformats} package owns file detection and decoding.
This function checks the rectangular R view, records explicit sample and
spectral-axis identities, and selects one finite numeric target. Heterogeneous
axes or multidimensional signals require the lossless records interface of
\code{nirs4allformats} and are not silently flattened.
}
\value{A \code{nirs4all_dataset} containing \code{X}, optional \code{y},
sample IDs, spectral axis and source metadata. Pass it directly to
\code{nirs4all_fit()} or \code{predict()}.}
