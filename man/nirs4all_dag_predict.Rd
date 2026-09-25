\name{nirs4all_dag_predict}
\alias{nirs4all_dag_predict}
\title{Predict new samples from a native DAG-ML refit artifact}
\description{Checks the SHA-256 fingerprints of the winning R model and any preceding transform artifacts, then predicts new samples locally. This does not run a new DAG-ML PREDICT phase or score the new cohort. Load only trusted RDS artifacts.}
\usage{
nirs4all_dag_predict(outcome, X)
}
\arguments{
\item{outcome}{Result of \code{nirs4all_dag_cv_refit_predict()} with its persistent workdir and artifact available.}
\item{X}{Finite numeric samples-by-features matrix or a \code{nirs4all_dataset} from \code{nirs4all_from_formats()}.}
}
\value{Numeric predictions in input row order.}
