\name{nirs4all_robust_snv}
\alias{nirs4all_robust_snv}
\title{Define a robust SNV preprocessing step}
\description{Use libn4m's median and robust-dispersion row normalization.}
\usage{nirs4all_robust_snv(with_center = TRUE, with_scale = TRUE, k = 1.4826)}
\arguments{
\item{with_center}{Center each row by its median.}
\item{with_scale}{Scale each row by robust dispersion.}
\item{k}{Positive robust scale factor.}
}
\value{A preprocessing step for \code{nirs4all_pipeline()}.}
