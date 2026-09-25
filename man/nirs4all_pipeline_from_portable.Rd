\name{nirs4all_pipeline_from_portable}
\alias{nirs4all_pipeline_from_portable}
\title{Convert a portable JSON/YAML recipe into an R pipeline}
\usage{nirs4all_pipeline_from_portable(source)}
\arguments{\item{source}{Definition accepted by \code{nirs4all_load_pipeline()}.}}
\value{An unfitted \code{nirs4all_pipeline()}.}
\description{The bounded reader accepts native SNV, Savitzky-Golay, LSNV, RNV,
area normalization, detrend, train-fitted MSC/EMSC, feature-only branches
merged by concatenation, PLS components, eight qualified native affine
n4m regressions and sparse PLS-DA classification.
Splitters and component sweeps are refused because a single
fitted pipeline cannot represent an entire selection experiment.}
