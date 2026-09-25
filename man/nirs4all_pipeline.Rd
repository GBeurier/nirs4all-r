\name{nirs4all_pipeline}
\alias{nirs4all_pipeline}
\alias{nirs4all_snv}
\alias{nirs4all_savgol}
\title{Define a local R spectroscopy pipeline}
\description{Constructs a preprocessing sequence and one learner controller. Built-in steps delegate their numerical work to the n4m C ABI. Pipelines can also run through the native DAG-ML campaign helper.}
\usage{
nirs4all_pipeline(steps = list(), learner = nirs4all_pls())
nirs4all_snv(ddof = 0L, with_mean = TRUE, with_std = TRUE)
nirs4all_savgol(window_length, polyorder = 2L, deriv = 0L,
               delta = 1, mode = "interp", cval = 0)
}
\arguments{
\item{steps}{List of preprocessing steps.}
\item{learner}{Learner controller.}
\item{ddof}{Degrees-of-freedom correction for row standard deviation.}
\item{with_mean}{Center each row before SNV scaling.}
\item{with_std}{Scale each row by standard deviation.}
\item{window_length}{Odd Savitzky-Golay window size.}
\item{polyorder}{Polynomial order.}
\item{deriv}{Derivative order.}
\item{delta}{Wavelength spacing.}
\item{mode}{Boundary mode supported by n4m.}
\item{cval}{Constant boundary fill value.}
}
\value{\code{nirs4all_pipeline} returns a pipeline definition; the other functions return preprocessing steps.}
