\name{nirs4all_run_portable_pipeline}
\alias{nirs4all_run_portable_pipeline}
\title{Run a Python-style JSON/YAML portable pipeline}
\usage{nirs4all_run_portable_pipeline(source, dataset, core_cli = NULL, methods_library = NULL)}
\arguments{
\item{source}{Definition accepted by \code{nirs4all_load_pipeline()}.}
\item{dataset}{List with \code{X}, \code{y} and optional
\code{rows}/\code{cols}, or a \code{nirs4all_dataset} from
\code{nirs4all_from_formats()}. Classification labels may be factor,
character or finite numeric class codes; numeric codes retain their type
in the returned predictions. A record-backed \code{nirs4all_dataset()} is
accepted by the owner-executed linear regression profile only.}
\item{core_cli}{Optional \code{nirs4all-core-archive} executable. Defaults to
the record-backed dataset's own CLI, \code{NIRS4ALL_CORE_CLI}, then \code{PATH}.}
\item{methods_library}{Optional absolute path of \code{libn4m}. Defaults to
\code{N4M_LIB_PATH}, then \code{N4M_LIBRARY_PATH}.}
}
\value{Split indices (zero-based, owner order), variant predictions and
task-specific RMSE or accuracy, selected variant, targets, evaluation scope and
the declared \code{profile}. Owner runs add \code{mode}, \code{identities} and
\code{owner}.}
\description{The declared profile decides the route before execution. Linear regression recipes (n4m preprocessing chain, PLS or an affine n4m model, optional Kennard-Stone holdout) are admitted, split, fitted, scored and selected by the Core owner operation \code{portable-pipeline-run}; R serializes the complete original document and dataset and projects the result. A missing Core CLI or Methods library, or an owner refusal, is an error: there is no R fallback for that profile. Classification, feature branches, training augmentations, models with extra fit inputs and the PLS algorithm \code{opls_nipals} are the named temporary residual B01 profiles; they keep the existing local R execution, chosen by the declared profile (\code{result$profile}) and never by catching an owner error. Kennard-Stone selection-validation scores are not independent test scores; without a splitter the scope is training. Use \code{nirs4all_dag_cv_refit_predict()} for general CV/OOF/refit.}
