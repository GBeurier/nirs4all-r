\name{nirs4all_native_augmentation}
\alias{nirs4all_native_augmentation}
\title{Specify a seeded n4m training-only augmentation}
\description{Constructs an X-only, shape-preserving native augmentation specification. The numerical kernel runs in n4m during fit, never during validation or prediction. Mixed-Y and wavelength-axis-dependent augmenters are not exposed.}
\usage{nirs4all_native_augmentation(kind, params = numeric(), seed = 0)}
\arguments{
\item{kind}{One of the 22 supported X-only native n4m kinds.}
\item{params}{Finite positional parameter vector following the n4m native contract.}
\item{seed}{Exact nonnegative integer seed, at most \code{2^53-1}.}
}
\value{A \code{nirs4all_native_augmentation} specification.}
