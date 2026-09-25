\name{nirs4all_run_portable_pipeline}
\alias{nirs4all_run_portable_pipeline}
\title{Run a Python-style JSON/YAML portable pipeline}
\usage{nirs4all_run_portable_pipeline(source, dataset)}
\arguments{
\item{source}{Definition accepted by \code{nirs4all_load_pipeline()}.}
\item{dataset}{List with \code{X}, \code{y} and optional
\code{rows}/\code{cols}, or a \code{nirs4all_dataset} from
\code{nirs4all_from_formats()}. Classification labels may be factor,
character or finite numeric class codes; numeric codes retain their type
in the returned predictions.}
}
\value{Split indices, variant predictions and task-specific RMSE or accuracy,
selected variant and evaluation scope.}
\description{Runs the bounded portable subset using the product R pipeline and native n4m numerical methods. PLS regression variants use RMSE; sparse PLS-DA classification variants use accuracy. Kennard-Stone selection-validation scores are not independent test scores; without a splitter the scope is training. Use \code{nirs4all_dag_cv_refit_predict()} for general CV/OOF/refit.}
