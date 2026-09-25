\name{nirs4all_dag_cv_refit_predict}
\alias{nirs4all_dag_cv_refit_predict}
\title{Run a native DAG-ML CV, refit and predict campaign from R matrices}
\description{Builds an explicit single-node fold campaign. DAG-ML owns phase scheduling, out-of-fold evidence, variant selection, refit and replay. A named list of pipelines compares fixed candidates by OOF RMSE and refits the winner. The replay cohort is the training cohort, not an independent test set. Arbitrary branch/merge graphs and adaptive HPO are not yet exposed.}
\usage{
nirs4all_dag_cv_refit_predict(pipeline, X, y = NULL, folds = 5L,
  sample_ids = NULL, root_seed = 1L, cli = Sys.which("dag-ml-cli"),
  workdir = tempfile("nirs4all-dag-"), process_workers = 1L)
}
\arguments{
\item{pipeline}{Pipeline definition with a built-in learner, or a named list
of at least two such pipelines to compare by native OOF RMSE.}
\item{X}{Finite numeric samples-by-features matrix or a
\code{nirs4all_dataset} from \code{nirs4all_from_formats()}.}
\item{y}{Finite numeric target vector; omit for a dataset.}
\item{folds}{Integer number of non-shuffled folds.}
\item{sample_ids}{Optional unique sample IDs aligned to X and y.}
\item{root_seed}{Non-negative DAG-ML root seed.}
\item{cli}{Path to the dag-ml-cli executable.}
\item{workdir}{New or empty output directory.}
\item{process_workers}{Positive process-adapter worker count.}
}
\value{Native DAG-ML outcome list with the persisted workdir path.}
