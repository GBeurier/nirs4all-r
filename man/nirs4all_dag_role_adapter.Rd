\name{nirs4all_dag_role_adapter}
\alias{nirs4all_dag_role_adapter}
\title{Prepare a native Methods multimodal DAG process adapter}
\usage{
nirs4all_dag_role_adapter(sources, operators, controller_manifest,
  trusted_manifest, targets = NULL, target_names = "y", allow_fit = FALSE,
  workdir = tempfile("nirs4all-role-adapter-"))
}
\arguments{
\item{sources}{Named list of numeric source tables, each with a finite matrix
\code{X}, explicit unique \code{sample_ids}, and optional ordered
\code{feature_names}. Column names are used when present; otherwise names are
zero-based positional strings. Each source may have an independent row order.}
\item{operators}{Named map of model node IDs to their current compiled native
operators: \code{n4m:<method-id>} or \code{N4mRolePipeline} recipes.}
\item{controller_manifest}{Native DAG-ML model manifest for
\code{controller:methods.r.regression}, version \code{1.0.0}.}
\item{trusted_manifest}{Independently trusted current installed manifest.
Must equal \code{controller_manifest}; a package cannot supply its own trust.}
\item{targets}{Optional table with numeric \code{values} and explicit unique
\code{sample_ids}. Values is a vector or samples-by-targets matrix.}
\item{target_names}{Unique ordered response names.}
\item{allow_fit}{Explicitly allow FIT_CV/REFIT; defaults to FALSE. A replay
adapter must receive no target values.}
\item{workdir}{New or empty local directory for the adapter config and launcher.}
}
\value{List with executable \code{adapter}, \code{manifest}, \code{workdir}
and \code{audit_path} containing lifecycle metadata.}
\description{Prepares a persistent JSONL Methods regression controller for an
existing native DAG-ML plan with named numeric sources and native upstream
OOF inputs. Native Methods owns fitting, prediction and opaque N4ME state.
DAG-ML owns signing, folds, OOF construction, selection, refit and replay.}
\details{Use the executable with native DAG-ML process wrappers. Validate
training/replay signatures, plans, complete artifact coverage and signed
controller manifests against independently trusted current manifests in
native DAG-ML before invoking callbacks. This adapter does not implement
canonical fingerprints, search spaces or a scheduler.

The RAW codec is \code{dagml.methods.regression.v1}, with plugin
\code{dagml.methods.r.regression}, version \code{1.0.0}. It preserves the
ordered native recipe and requires one N4ME state per declared step. Export
refuses retained training rows. Hydration verifies artifact identity, hashes,
size and relative URI; prediction verifies the full effective current recipe
and independently resolved ordered feature names. Native pointers are
released on success, worker close and failure, using the upstream n4m R
finalizer. Only locally trusted RDS configs are read.

The numeric regression profile excludes raw N-D encoders, classification,
generated-view consumption attestations, custom losses, residual targets,
additional FIT_CV test streams and nonuniform fit influence. It currently
requires a POSIX shell. Existing matrix/N4MM APIs are separate and unchanged.}
