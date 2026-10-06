args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
# Source the current product facade; numerical dependencies are installed native.
source(file.path(args[[3L]], "R", "core_archive.R"))
source(file.path(args[[3L]], "R", "dataset_multimodal.R"))
suppressPackageStartupMessages(library(n4m))
p <- args[[1L]]; cli <- args[[2L]]
read <- function(name) jsonlite::fromJSON(file.path(p, paste0(name, ".json")), simplifyVector = FALSE)
check <- function(result) {
  expected <- read("expected")
  stopifnot(identical(as.list(result$sample_ids), expected$sample_ids),
    max(abs(result$values - as.double(unlist(expected$values)))) < 1e-8)
}
train <- nirs4all_dataset(file.path(p, "train.json"), cli)
new <- nirs4all_dataset(file.path(p, "predict.json"), cli)
replay <- nirs4all_multimodal_load(file.path(p, "predictor.json"), cli)
trace("n4m_fit", tracer = quote(stop("FIT forbidden during cold replay")), print = FALSE, where = asNamespace("n4m"))
check(stats::predict(replay, new))
untrace("n4m_fit", where = asNamespace("n4m")); n4m::n4m_close(replay$native)
wrong <- read("predict"); wrong$dataset$sources[[1L]]$axis_units$wavelength <- "cm-1"
replay <- nirs4all_multimodal_load(file.path(p, "predictor-wasm.json"), cli)
check(stats::predict(replay, new))
stopifnot(inherits(try(stats::predict(replay, wrong), silent = TRUE), "try-error"))
n4m::n4m_close(replay$native)
fresh <- nirs4all_multimodal_fit(read("recipe"), train, cli)
check(stats::predict(fresh, new)); nirs4all_multimodal_export(fresh, file.path(p, "predictor-r.json")); n4m::n4m_close(fresh$native)
cat("PASS R actual U07 raw native fit, Python/WASM cold N4MF replay, no FIT, schema failures\n")
