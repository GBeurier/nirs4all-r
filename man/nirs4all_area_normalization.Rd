\name{nirs4all_area_normalization}
\alias{nirs4all_area_normalization}
\title{Define an area-normalization preprocessing step}
\description{Use libn4m's native spectrum-area normalization.}
\usage{nirs4all_area_normalization(method = "sum")}
\arguments{
\item{method}{One of \code{"sum"}, \code{"abs_sum"}, or \code{"trapz"}.}
}
\value{A preprocessing step for \code{nirs4all_pipeline()}.}
