args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) stop("expected outcome, spectra and result paths")
suppressPackageStartupMessages(library(nirs4all))
outcome <- readRDS(args[[1L]])
X <- readRDS(args[[2L]])
predictions <- nirs4all_dag_predict(outcome, X)
writeLines(as.character(jsonlite::toJSON(predictions, auto_unbox = FALSE,
                                          digits = NA)), args[[3L]])
