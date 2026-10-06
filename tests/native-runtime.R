library(nirs4all)
cli <- Sys.getenv("NIRS4ALL_CORE_CLI")
fixture <- Sys.getenv("NIRS4ALL_WORKFLOW_DATASET")
if (nzchar(cli) && nzchar(fixture)) {
  directory <- tempfile("nirs4all runtime with spaces ")
  dir.create(directory)
  local({
    on.exit(unlink(directory, recursive = TRUE))
    previous <- Sys.getenv("NIRS4ALL_CORE_CLI")
    on.exit(Sys.setenv(NIRS4ALL_CORE_CLI = previous), add = TRUE)
    data <- jsonlite::fromJSON(fixture, simplifyVector = FALSE)
    X <- do.call(rbind, lapply(data$dataset$sources[[1L]]$array$values, unlist))
    model <- nirs4all_native_run(data, file.path(directory, "model.n4a"), cli = cli)
    study <- nirs4all_tune(data, trials = 1L, archive = file.path(directory, "study.n4a"), cli = cli)
    Sys.setenv(NIRS4ALL_CORE_CLI = "missing-default-native-cli")
    stopifnot(length(nirs4all_native_predict(model, X)$outputs) == 1L,
              length(nirs4all_tuning_predict(study, X)$outputs) == 1L)
    continued <- nirs4all_resume_tuning(study, data, 2L,
      archive = file.path(directory, "continued.n4a"))
    stopifnot(identical(continued$cli, study$cli))
    calibration <- jsonlite::fromJSON(file.path(Sys.getenv("NIRS4ALL_BLOCK5_FIXTURE"),
      "dataset-calibration.json"), simplifyVector = FALSE)
    current <- jsonlite::fromJSON(file.path(Sys.getenv("NIRS4ALL_BLOCK5_FIXTURE"),
      "independent.json"))
    calibrated <- nirs4all_calibrate(model, calibration, coverages = 0.8,
      archive = file.path(directory, "calibrated.n4a"))
    intervals <- nirs4all_predict_calibrated(calibrated, current$x, current$sample_ids)
    stopifnot(length(nirs4all_conformal_metrics(calibrated, intervals, current$y,
      current$sample_ids)$coverages) == 1L,
      identical(nirs4all_robustness(calibrated, current$x, current$y,
        current$sample_ids)$mode, "clean_frozen"))
    destination <- file.path(directory, "export")
    nirs4all_native_export(model, destination)
    loaded <- nirs4all_native_load(destination, cli = cli)
    stopifnot(identical(nirs4all_native_predict(loaded, X)$outputs[[1L]]$predictions[[1L]]$values,
                        nirs4all_native_predict(model, X)$outputs[[1L]]$predictions[[1L]]$values))
    bad <- model
    bad$archive <- file.path(directory, "absent.n4a")
    failed <- file.path(directory, "failed-export")
    stopifnot(inherits(try(suppressWarnings(nirs4all_native_export(bad, failed)), silent = TRUE), "try-error"),
              !file.exists(failed))
    sentinel <- file.path(destination, "foreign.txt")
    writeLines("other owner", sentinel)
    stopifnot(inherits(try(nirs4all_native_export(model, destination), silent = TRUE), "try-error"),
              identical(readLines(sentinel), "other owner"))
    if (.Platform$OS.type != "windows") {
      link <- file.path(directory, "native cli")
      stopifnot(file.symlink(cli, link))
      relocated <- nirs4all_native_load(destination, cli = link)
      stopifnot(length(nirs4all_native_predict(relocated, X)$outputs) == 1L)
    }
  })
  message("R_NATIVE_RUNTIME_PASS stored_cli resume export_rollback paths_with_spaces")
}
