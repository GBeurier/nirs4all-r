\name{nirs4all_expand_portable_pipelines}
\alias{nirs4all_expand_portable_pipelines}
\title{Expand a bounded Python-style generator recipe into R pipelines}
\usage{
nirs4all_expand_portable_pipelines(source)
}
\arguments{
\item{source}{Definition accepted by \code{nirs4all_load_pipeline()}.}
}
\value{
Named list of unfitted \code{nirs4all_pipeline()} variants.
}
\description{
Supports the n4m preprocessing classes accepted by the portable reader,
a PLS component \code{_range_}, and preprocessing \code{_or_} or \code{_cartesian_} stages
without selection modifiers. Variant order matches Python's left-to-right
Cartesian product. Unsupported generators and splitters are rejected. Pass the
resulting named list to \code{nirs4all_dag_cv_refit_predict()} for native
CV/OOF selection and refit.
}
