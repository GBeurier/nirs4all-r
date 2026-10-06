args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
directory <- args[[1L]]; core_cli <- args[[2L]]; source_root <- args[[3L]]
for (file in c("core_archive.R", "dataset_multimodal.R")) source(file.path(source_root,"R",file))
expected <- jsonlite::fromJSON(file.path(directory,"content-expected.json"))
original <- nirs4all_dataset(file.path(directory,"predict.json"),core_cli)
host <- nirs4all_dataset(file.path(directory,"logical-host-dataset.json"),core_cli)
alternate <- nirs4all_dataset(file.path(directory,"reordered-host-dataset.json"),core_cli)
for (value in list(original,host,alternate)) {
  actual <- .nirs4all_dataset_native(value,"dataset-content",core_cli)
  stopifnot(identical(actual$fingerprint,expected$fingerprint),identical(actual$canonical_content_utf8,expected$canonical_content_utf8))
}
# Numeric object metadata must reach Methods without R's 15-digit text rounding.
number <- 1.2345678901234567
raw <- list(sources=list(metadata=list(rows=list(list(number,"category")))))
stopifnot(identical(as.double(.nirs4all_mm_blocks(raw)$metadata[1L,1L]),number))
writeLines(jsonlite::toJSON(list(fingerprint=expected$fingerprint,numeric_roundtrip=TRUE),auto_unbox=TRUE),file.path(directory,"r-content-evidence.json"))
cat("PASS R logical raw-content bytes and numeric metadata precision\n")
