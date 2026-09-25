\name{nirs4all_load_pipeline}
\alias{nirs4all_load_pipeline}
\title{Read a Python-style portable pipeline definition}
\usage{nirs4all_load_pipeline(source)}
\arguments{\item{source}{Pipeline list, JSON/YAML text or path to a \code{.json}, \code{.yaml} or \code{.yml} file.}}
\value{A \code{nirs4all_pipeline_definition}.}
\description{Reads the bounded portable PipelineConfigs subset inherited from the former \code{nirs4all-core} R binding. Unsupported operators are rejected when the definition is translated or run.}
