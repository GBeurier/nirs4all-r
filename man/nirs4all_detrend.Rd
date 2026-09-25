\name{nirs4all_detrend}
\alias{nirs4all_detrend}
\title{Define a polynomial detrend preprocessing step}
\description{Use libn4m to remove a per-spectrum polynomial baseline.}
\usage{nirs4all_detrend(polyorder = 1L)}
\arguments{
\item{polyorder}{Non-negative baseline polynomial order.}
}
\value{A preprocessing step for \code{nirs4all_pipeline()}.}
