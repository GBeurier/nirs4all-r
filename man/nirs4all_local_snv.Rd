\name{nirs4all_local_snv}
\alias{nirs4all_local_snv}
\title{Define a local SNV preprocessing step}
\description{Use libn4m's sliding-window Standard Normal Variate transform.}
\usage{nirs4all_local_snv(window = 11L, pad_mode = "reflect", constant_value = 0)}
\arguments{
\item{window}{Odd sliding-window length.}
\item{pad_mode}{One of \code{"reflect"}, \code{"edge"}, or \code{"constant"}.}
\item{constant_value}{Padding value in constant mode.}
}
\value{A preprocessing step for \code{nirs4all_pipeline()}.}
