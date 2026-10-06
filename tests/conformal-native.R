fixture <- Sys.getenv("NIRS4ALL_BLOCK5_FIXTURE")
if (nzchar(fixture)) {
  root <- Sys.getenv("NIRS4ALL_R_SOURCE")
  if (nzchar(root)) {
    source(file.path(root, "R", "native_workflow.R"))
    source(file.path(root, "R", "core_archive.R"))
    source(file.path(root, "R", "dataset_multimodal.R"))
    source(file.path(root, "R", "conformal.R"))
    source(file.path(root, "R", "robustness.R"))
  } else library(nirs4all)
  data <- jsonlite::fromJSON(file.path(fixture, "dataset-calibration.json"), simplifyVector = FALSE)
  input <- jsonlite::fromJSON(file.path(fixture, "independent.json"))
  reference <- jsonlite::fromJSON(file.path(fixture, "shared-prediction.json"), simplifyVector = FALSE)
  directory <- tempfile("nirs4all-conformal-"); dir.create(directory)
  calibrated <- nirs4all_calibrate(file.path(fixture, "shared-model.n4a"), data,
    coverages = c(0.8, 0.9), archive = file.path(directory, "r-calibrated.n4a"))
  prediction <- nirs4all_predict_calibrated(calibrated, input$x, input$sample_ids)
  frame_prediction <- nirs4all_predict_calibrated(calibrated, as.data.frame(input$x), input$sample_ids)
  stopifnot(identical(frame_prediction$interval_block, prediction$interval_block))
  stopifnot(identical(prediction$sample_ids, reference$sample_ids),
    isTRUE(all.equal(prediction$interval_block$intervals, reference$interval_block$intervals, tolerance = 1e-12)))
  metrics <- nirs4all_conformal_metrics(calibrated, prediction, input$y, input$sample_ids)
  stopifnot(length(metrics$coverages) == 2L)
  relocated <- file.path(directory, "relocated.n4a")
  nirs4all_export_calibrated(calibrated, relocated)
  unlink(calibrated$archive)
  loaded <- nirs4all_load_calibrated(relocated)
  replay <- nirs4all_predict_calibrated(loaded, input$x, input$sample_ids)
  stopifnot(identical(replay$interval_block$intervals, prediction$interval_block$intervals))
  singleton <- nirs4all_predict_calibrated(loaded, input$x[1L, , drop = FALSE], input$sample_ids[1L])
  stopifnot(identical(singleton$sample_ids, list(input$sample_ids[1L])),
            length(nirs4all_conformal_metrics(loaded, singleton, input$y[1L], input$sample_ids[1L])$coverages) == 2L)
  structured <- nirs4all_dataset(file.path(fixture,"dataset-predict.json"), core_cli = Sys.getenv("NIRS4ALL_CORE_CLI"))$record
  ids <- as.list(input$sample_ids)
  matched <- nirs4all_predict_calibrated(loaded, structured)
  stopifnot(identical(matched$point_prediction$values, replay$point_prediction$values))
  wrong <- structured
  wrong$dataset$sources[[1L]]$axis_units <- list(wavelength = "nm")
  stopifnot(inherits(try(nirs4all_predict_calibrated(loaded, wrong), silent = TRUE), "try-error"))
  aliases <- data
  aliases$origin_ids <- rep(list("train.s0"), length(ids))
  aliases$dataset$groups <- list(dtype = "<U64", shape = list(length(ids)), values = aliases$dataset$sample_ids)
  stopifnot(inherits(try(nirs4all_calibrate(file.path(fixture,"shared-model.n4a"), aliases,
    coverages = 0.8, archive = file.path(directory,"refused-aliases.n4a")), silent = TRUE), "try-error"),
    !file.exists(file.path(directory,"refused-aliases.n4a")))
  precise <- nirs4all_calibrate(file.path(fixture,"shared-model.n4a"), data,
    coverages = 0.68269, archive = file.path(directory,"precise.n4a"))
  stopifnot(identical(as.numeric(precise$calibration$coverages[[1L]]), 0.68269))
  report <- nirs4all_robustness(loaded, input$x, input$y, input$sample_ids)
  structured_report <- nirs4all_robustness(loaded, structured, input$y)
  singleton_report <- nirs4all_robustness(loaded, input$x[1L, , drop = FALSE], input$y[1L], input$sample_ids[1L])
  stopifnot(identical(structured_report$scenarios[[1L]]$point_predictions, report$scenarios[[1L]]$point_predictions),
            length(singleton_report$scenarios[[1L]]$point_predictions) == 1L)
  expected <- jsonlite::fromJSON(file.path(fixture, "shared-robustness.json"), simplifyVector = FALSE)
  stopifnot(identical(report$mode, "clean_frozen"),
    isTRUE(all.equal(report$scenarios[[2L]]$point_predictions, expected$scenarios[[2L]]$point_predictions, tolerance = 1e-12)))
  rejected <- try(nirs4all_predict_calibrated(loaded, input$x, unlist(data$dataset$sample_ids)), silent = TRUE)
  stopifnot(inherits(rejected, "try-error"))
  unlink(directory, recursive = TRUE)
  cat("native R conformal and seeded robustness cross-language gate passed\n")
}
