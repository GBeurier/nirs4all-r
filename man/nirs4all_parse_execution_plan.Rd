\name{nirs4all_parse_execution_plan}
\alias{nirs4all_parse_execution_plan}
\title{Translate a portable pipeline definition}
\usage{nirs4all_parse_execution_plan(source)}
\arguments{\item{source}{Definition accepted by \code{nirs4all_load_pipeline()}.}}
\value{List containing the optional splitter, preprocessing steps and PLS component variants.}
\description{Supports the bounded native subset: Kennard-Stone holdout, SNV, Savitzky-Golay and PLS component sweeps. Python-style classes, selected \code{n4m.*} aliases and language-neutral method IDs resolve to the same R n4m steps. Unsupported classes, parameters and step ordering fail explicitly.}
