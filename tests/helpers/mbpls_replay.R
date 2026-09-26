suppressPackageStartupMessages(library(nirs4all))

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
held <- readRDS(args[[1L]])
saved <- nirs4all_load(args[[2L]])
trained <- nirs4all_import_trained_pipeline(args[[3L]])
cat(jsonlite::toJSON(list(
  saved = unname(as.numeric(predict(saved, held))),
  trained = unname(as.numeric(predict(trained, held)))),
  auto_unbox = FALSE, digits = 17L))
