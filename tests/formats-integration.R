strict <- identical(Sys.getenv("NIRS4ALL_REQUIRE_FORMATS"), "1")
available <- requireNamespace("nirs4allformats", quietly = TRUE)
if (!available && strict) stop("strict formats integration requires nirs4allformats")

if (available) {
  library(nirs4all)
  path <- system.file("extdata", "formats_integration.csv",
                      package = "nirs4all", mustWork = TRUE)
  raw <- nirs4allformats::nirs4allformats_open_dataset(path)
  dataset <- nirs4all_from_formats(raw, target = "protein")
  stopifnot(identical(dim(dataset$X), c(12L, 8L)),
            identical(dataset$sample_ids, raw$sample_ids),
            identical(rownames(dataset$X), raw$sample_ids),
            identical(unname(dataset$y), raw$targets$protein),
            identical(dataset$wavelengths, raw$wavelengths),
            identical(dataset$axis_kind,
                      if (is.null(raw$axis_kind)) "unspecified" else raw$axis_kind),
            identical(dataset$signal_type, "absorbance"))
  from_path <- nirs4all_from_formats(path, target = "protein")
  stopifnot(identical(from_path$X, dataset$X),
            identical(from_path$y, dataset$y))
  pipeline <- nirs4all_pipeline(list(nirs4all_snv()),
                               nirs4all_pls(n_components = 2L))
  fitted <- nirs4all_fit(pipeline, dataset)
  explicit <- nirs4all_fit(pipeline, dataset$X, dataset$y)
  predict_dataset <- nirs4all_from_formats(path)
  stopifnot(isTRUE(all.equal(predict(fitted, predict_dataset),
                             predict(explicit, dataset$X), tolerance = 1e-12)))
  trained_bundle <- nirs4all_export_trained_pipeline(fitted)
  transferred <- nirs4all_import_trained_pipeline(trained_bundle)
  stopifnot(isTRUE(all.equal(predict(transferred, predict_dataset),
                             predict(fitted, predict_dataset), tolerance = 1e-12)))
  changed_axis <- raw
  changed_axis$axis_unit <- "cm-1"
  changed <- nirs4all_from_formats(changed_axis)
  bad <- tryCatch(predict(fitted, changed), error = identity)
  stopifnot(inherits(bad, "error"), grepl("feature names", conditionMessage(bad)))
  bad <- tryCatch(predict(transferred, changed), error = identity)
  stopifnot(inherits(bad, "error"), grepl("feature names", conditionMessage(bad)))
  duplicate <- raw
  duplicate$sample_ids[[2L]] <- duplicate$sample_ids[[1L]]
  bad <- tryCatch(nirs4all_from_formats(duplicate, "protein"), error = identity)
  stopifnot(inherits(bad, "error"), grepl("unique", conditionMessage(bad)))
  bad <- tryCatch(nirs4all_from_formats(raw, "missing"), error = identity)
  stopifnot(inherits(bad, "error"), grepl("not present", conditionMessage(bad)))
  message("nirs4all-formats → nirs4all R matrix/target/axis integration passed")
}
