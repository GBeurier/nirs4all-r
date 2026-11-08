\name{nirs4all_parse_execution_plan}
\alias{nirs4all_parse_execution_plan}
\title{Translate a portable pipeline definition}
\usage{nirs4all_parse_execution_plan(source, core_cli = NULL)}
\arguments{
\item{source}{Definition accepted by \code{nirs4all_load_pipeline()}.}
\item{core_cli}{Optional \code{nirs4all-core-archive} executable (or \code{""} for \code{NIRS4ALL_CORE_CLI}/\code{PATH}) used to project the no-fit owner admission.}
}
\value{List containing the optional splitter, ordered training-only augmentations, preprocessing steps, model component variants, the declared \code{profile} (\code{name}), the \code{admission} source (\code{"source_preflight"} is only the host preflight, not a native admission; \code{"owner"} when \code{core_cli} is given) and, then, the unchanged owner admission in \code{owner}.}
\description{Supports the bounded native subset, including Kennard-Stone holdout, a closed \code{train_augmentation} prefix, n4m preprocessing, \code{n4m.Selector} and PLS component sweeps. Augmentation nodes require \code{n4m.NativeXAugmentation}, kind, positional values and an exact seed; they must follow any splitter and precede preprocessing. The generic selector has a closed method vocabulary and native parameter schema; portable recipes require explicit top-k values and random seeds where applicable. Python-style classes, selected \code{n4m.*} aliases and language-neutral method IDs resolve to the same R n4m steps. Unsupported classes, parameters and steps after the model fail explicitly.}
