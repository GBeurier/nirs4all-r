\name{nirs4all_expand_portable_pipelines}
\alias{nirs4all_expand_portable_pipelines}
\title{Expand a bounded Python-style generator recipe into R pipelines}
\usage{
nirs4all_expand_portable_pipelines(source, core_cli = NULL)
}
\arguments{
\item{source}{Definition accepted by \code{nirs4all_load_pipeline()}.}

\item{core_cli}{Optional \code{nirs4all-core-archive} executable (or \code{""} for
\code{NIRS4ALL_CORE_CLI}/\code{PATH}).}
}
\value{
Named list (\code{variant_0001}, \code{variant_0002}, ...) of unfitted
\code{nirs4all_pipeline()} variants.
}
\description{
No-fit operation. The complete original definition is sent to the Core
\code{portable-pipeline-expand} command, which owns the \code{_or_} / \code{_cartesian_}
structural generators (left-to-right product, first axis slowest), the
inclusive component ranges of each structural variant, the search bounds and
the refusals (modifiers, nested generators, splitters, unknown roots or
intents). R only builds one unfitted \code{nirs4all_pipeline()} per returned
concrete definition, in the owner's order, with
\code{nirs4all_pipeline_from_portable()}; it generates and enumerates nothing and
has no local fallback. Requires a \code{nirs4all-core-archive} build that provides
\code{portable-pipeline-expand} (no Methods library, no dataset, no fit). Pass the
resulting named list to \code{nirs4all_dag_cv_refit_predict()} for native CV/OOF
selection and refit.
}
