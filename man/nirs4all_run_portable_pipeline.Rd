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
\description{The declared profile decides the route before execution. Linear regression recipes (n4m preprocessing chain, PLS including \code{opls_nipals}, or an affine n4m model, optional Kennard-Stone holdout) and SparsePLS-DA classification recipes are admitted, split, fitted, scored and selected by the Core owner operation \code{portable-pipeline-run}; R serializes the complete original document and dataset and projects the result. Classification sends R's existing class codes as nonnegative integer ids on the dense \code{y} wire and decodes the returned ids with its own class domain (\code{class_values}); accuracy, the winner and \code{unseen_validation_classes} come from the owner. A missing Core CLI or Methods library, or an owner refusal, is an error: there is no R fallback for these profiles. A regression feature branch (\code{branch} + \code{merge: features} + terminal model, profile \code{portable_feature_regression.v1}) is owner-executed on the DAG per-estimator graph with a native feature join; the optional owner \code{feature_blocks} widths are copied unchanged, with no merged labels or axis and no fitted-model or replay scope. A standalone regression \code{train_augmentation} prefix and/or an MBPLS \code{block_sizes} / GroupSparsePLS \code{group_assignment} terminal use the same profile and are sent unchanged (values, exact seeds and arrays as declared; the owner resolves the augmenters, types the values and supplies the feature-axis inputs). A classifier with a feature branch or training augmentation (\code{portable_feature_classification.v1}) and a branch with MBPLS / GroupSparsePLS fit inputs (\code{portable_feature_regression.v1}, explicit arrays describing the joined feature axis) are staged for the same owner graph route (private source; activation after the owner gates). Only combinations no owner profile admits (augmentation with a branch, fit inputs with a classifier) stay in the residual table and the unchanged local runner, chosen by the declared profile (\code{result$profile}) and never by catching an owner error. Kennard-Stone selection-validation scores are not independent test scores; without a splitter the scope is training. Use \code{nirs4all_dag_cv_refit_predict()} for general CV/OOF/refit.}
