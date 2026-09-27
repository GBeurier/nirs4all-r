\name{nirs4all_fit_role_recipe}
\alias{nirs4all_fit_role_recipe}
\title{Fit a recipe of generic n4m role steps}
\usage{
nirs4all_fit_role_recipe(recipe, X, y)
}
\arguments{
\item{recipe}{A recipe list, or JSON/YAML text or file (see \code{\link{nirs4all_load_pipeline}}).}
\item{X}{Numeric samples-by-features matrix.}
\item{y}{Responses (a numeric vector, or a matrix with one row per sample) for a final regressor, or class labels for a final classifier. Their length must equal \code{nrow(X)}: nothing is recycled.}
}
\value{A fitted pipeline.}
\description{Steps are \code{"n4m:<catalog method id>"} tokens (or \code{list(class = token, params = list(...))}): sample filters (they drop training rows), then transformers and selectors, then one regressor or classifier. The recipe is an n4m native role pipeline: n4m validates it, fits every step, routes every response column to the supervised steps, and stores the column names of \code{X} (a matrix without column names is positional). The result predicts with \code{\link{nirs4all_predict}}, retrains with \code{\link{nirs4all_retrain}} and exports with \code{\link{nirs4all_export_trained_pipeline}} as a version 8 envelope that the Python, JS/WASM and Rust n4m bindings replay identically.}
