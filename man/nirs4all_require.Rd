\name{nirs4all_require}
\alias{nirs4all_require}
\title{Load an upstream domain namespace}
\usage{nirs4all_require(name)}
\arguments{\item{name}{One domain key returned by \code{nirs4all_upstreams()}.}}
\value{The loaded R package namespace.}
\description{Loads the owning upstream package or fails explicitly if it is unavailable.}
