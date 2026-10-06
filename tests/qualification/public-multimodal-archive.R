args <- commandArgs(TRUE)
stopifnot(length(args) %in% c(3L, 4L))
fixture <- args[[1L]]; core_cli <- args[[2L]]; dag_cli <- args[[3L]]
adapter_options <- list()
if (length(args) == 4L) {
  source_root <- args[[4L]]
  for (file in c("trained_roles.R", "core_archive.R", "dataset_multimodal.R", "multimodal_archive.R"))
    source(file.path(source_root, "R", file))
  adapter_options$adapter_script <- file.path(source_root,"inst/adapters/nirs4all_core_multimodal_adapter.R")
} else {
  library(nirs4all)
}
replay <- function(dataset) do.call(nirs4all_multimodal_archive_predict,
  c(list(path = file.path(fixture, "u07-native.n4a"), dataset = dataset,
    core_cli = core_cli, dag_cli = dag_cli), adapter_options))
expected <- jsonlite::fromJSON(file.path(fixture, "expected.json"))
result <- replay(file.path(fixture, "predict.json"))
stopifnot(identical(result$sample_ids, expected$sample_ids),
  identical(result$target_names, expected$target_names), identical(result$training_performed, FALSE),
  max(abs(as.double(result$values) - as.double(expected$values))) < 1e-8,
  identical(vapply(result$audit, `[[`, character(1), "operation"), c("hydrate", "PREDICT", "dispose", "release")))
if (file.exists(file.path(fixture,"logical-host-dataset.json"))) {
  logical_result <- replay(file.path(fixture,"logical-host-dataset.json"))
  stopifnot(identical(logical_result$sample_ids,expected$sample_ids),max(abs(as.double(logical_result$values)-as.double(expected$values)))<1e-8,
    identical(vapply(logical_result$audit, `[[`, character(1), "operation"),c("hydrate","PREDICT","dispose","release")))
  writeLines(jsonlite::toJSON(logical_result,auto_unbox=TRUE,null="null",digits=I(17L)),file.path(fixture,"r-logical-archive-evidence.json"))
}
wrong <- jsonlite::fromJSON(file.path(fixture, "predict.json"), simplifyVector = FALSE)
wrong$dataset$sources[[1L]]$axis_units$wavelength <- "cm-1"
failure <- tryCatch({replay(wrong); NULL}, error = identity)
stopifnot(inherits(failure, "error"), grepl("schema differs", conditionMessage(failure), fixed = TRUE))
evidence_name <- if (length(args) == 3L) "r-defaults-core-archive-evidence.json" else "r-core-archive-evidence.json"
writeLines(jsonlite::toJSON(result, auto_unbox = TRUE, null = "null", digits = I(17L)), file.path(fixture, evidence_name))
cat("PASS R unchanged native .n4a archive, independent raw cohort, no training, schema failure\n")
