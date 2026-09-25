\name{nirs4all_import_native_model}
\alias{nirs4all_import_native_model}
\title{Import native N4MM pipeline state against an explicit R recipe}
\usage{
nirs4all_import_native_model(bytes, pipeline, feature_names = NULL)
}
\arguments{
\item{bytes}{Raw N4MM format-1 or format-2 bytes.}
\item{pipeline}{Matching unfitted \code{nirs4all_pipeline()} definition or
its portable JSON/YAML recipe.}
\item{feature_names}{Optional ordered feature names from training.}
}
\value{A fitted pipeline that predicts directly on raw spectra.}
\description{The n4m decoder validates the bytes and any embedded
preprocessing profile. A mismatched R recipe or feature width is refused.
The caller supplies the recipe and, if relevant, the original ordered
feature names. For plain format-1 PLS, scaling flags are caller assertions
because the native descriptor does not expose those training flags.}
