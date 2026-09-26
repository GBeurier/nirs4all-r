\name{nirs4all_fit_role_recipe}
\alias{nirs4all_fit_role_recipe}
\title{Fit a recipe of generic n4m role steps}
\usage{
nirs4all_fit_role_recipe(recipe, X, y)
}
\arguments{
\item{recipe}{A recipe list, or JSON/YAML text or file (see \code{\link{nirs4all_load_pipeline}}).}
\item{X}{Numeric samples-by-features matrix.}
\item{y}{Numeric targets (regression) or class labels (classification).}
}
\value{A fitted pipeline.}
\description{Steps are \code{"n4m:<catalog method id>"} tokens (or \code{list(class = token, params = list(...))}): sample filters (they drop training rows), then transformers and selectors, then one regressor or classifier. Every step is fitted by n4m; the result predicts with \code{\link{nirs4all_predict}}, retrains with \code{\link{nirs4all_retrain}} and exports with \code{\link{nirs4all_export_trained_pipeline}} as a version 7 envelope that the Python, JS/WASM and Rust n4m bindings replay identically.}
