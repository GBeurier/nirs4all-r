# Common CPU ArchiveV2 workflow; source overlay permits qualification without installing.
if (nzchar(Sys.getenv("NIRS4ALL_CORE_CLI")) && nzchar(Sys.getenv("N4M_LIBRARY_PATH")) &&
    nzchar(Sys.getenv("NIRS4ALL_WORKFLOW_DATASET"))) {
  library(nirs4all)
  api <- asNamespace("nirs4all")
  source_file <- Sys.getenv("NIRS4ALL_WORKFLOW_R_SOURCE")
  if (nzchar(source_file)) {
    api <- new.env(parent = api)
    sys.source(source_file, envir = api)
  }
  cli <- Sys.getenv("NIRS4ALL_CORE_CLI")
  directory <- tempfile("nirs4all-common-workflow-")
  dir.create(directory)
  fixture <- jsonlite::fromJSON(Sys.getenv("NIRS4ALL_WORKFLOW_DATASET"), simplifyVector = FALSE)
  X <- do.call(rbind, lapply(fixture$dataset$sources[[1]]$array$values, unlist))
  colnames(X) <- paste0("wavelength_", seq_len(ncol(X)))
  model <- api$nirs4all_native_run(fixture, file.path(directory, "model.n4a"),
    components = c(first = 1L, second = 2L), cli = cli)
  old_cli <- Sys.getenv("NIRS4ALL_CORE_CLI")
  Sys.setenv(NIRS4ALL_CORE_CLI = "missing-default-native-cli")
  named <- api$nirs4all_native_predict(model, X, sample_ids = setNames(paste0("fresh", seq_len(nrow(X))), seq_len(nrow(X))))
  unnamed <- api$nirs4all_native_predict(model, unname(X))
  stopifnot(identical(named$outputs[[1]]$predictions[[1]]$values,
                     unnamed$outputs[[1]]$predictions[[1]]$values),
            is.character(attr(named, "native_json")),
            identical(jsonlite::fromJSON(attr(named, "native_json"), simplifyVector = FALSE,
                       bigint_as_char = TRUE), structure(named, native_json = NULL)))
  singleton <- api$nirs4all_native_predict(model, X[1L, , drop = FALSE], sample_ids = c(named_id = "only"))
  stopifnot(identical(singleton$outputs[[1]]$predictions[[1]]$sample_ids, list("only")))
  fresh <- api$nirs4all_native_retrain(model, fixture, file.path(directory, "fresh.n4a"))
  stopifnot(!identical(fresh$outcome$training_outcome$outcome_fingerprint,
                      model$outcome$training_outcome$outcome_fingerprint))
  exported <- file.path(directory, "exported")
  api$nirs4all_native_export(model, exported)
  loaded <- api$nirs4all_native_load(exported, cli = cli)
  stopifnot(identical(loaded$cli, cli))
  overridden <- api$nirs4all_native_predict(loaded, X, cli = cli)
  stopifnot(identical(overridden$outputs[[1]]$predictions[[1]]$values,
                      named$outputs[[1]]$predictions[[1]]$values))
  Sys.setenv(NIRS4ALL_CORE_CLI = old_cli)
  unlink(directory, recursive = TRUE)
  message("R_COMMON_WORKFLOW_PASS named_components named_matrix singleton_ids stored_cli native_json")
}
