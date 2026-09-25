\name{nirs4all_save}
\alias{nirs4all_save}
\alias{nirs4all_load}
\title{Save and load a fitted R pipeline}
\description{Writes an RDS bundle. An n4m PLS model is converted to portable N4MM bytes; an R torch module is converted with torch's own serializer. The surrounding R object is not a cross-language pipeline bundle. Do not load untrusted RDS files.}
\usage{
nirs4all_save(object, file)
nirs4all_load(file)
}
\arguments{
\item{object}{Fitted pipeline.}
\item{file}{RDS file path.}
}
\value{\code{nirs4all_save} invisibly returns the path; \code{nirs4all_load} returns a fitted pipeline.}
