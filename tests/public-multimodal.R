library(nirs4all)
fixture <- Sys.getenv("NIRS4ALL_MULTIMODAL_FIXTURE")
if (nzchar(fixture)) {
  local({
    data <- nirs4all_dataset(file.path(fixture, "predict.json"))
    expected <- jsonlite::fromJSON(file.path(fixture, "expected.json"), simplifyVector = FALSE)
    check <- function(result) {
      stopifnot(identical(as.list(result$sample_ids), expected$sample_ids),
                identical(as.list(result$target_names), expected$target_names),
                max(abs(result$values - as.double(unlist(expected$values)))) < 1e-8)
    }
    model <- nirs4all_multimodal_load(file.path(fixture, "predictor.json"))
    on.exit(n4m::n4m_close(model$native))
    trace("n4m_fit", tracer = quote(stop("FIT forbidden in cold replay")),
          print = FALSE, where = asNamespace("n4m"))
    traced <- TRUE
    on.exit(if (traced) untrace("n4m_fit", where = asNamespace("n4m")), add = TRUE)
    check(stats::predict(model, data))
    wrong <- data$record
    wrong$dataset$sources[[1L]]$axis_units$wavelength <- "cm-1"
    stopifnot(inherits(try(stats::predict(model, wrong), silent = TRUE), "try-error"))
    untrace("n4m_fit", where = asNamespace("n4m"))
    traced <- FALSE
    recipe <- jsonlite::fromJSON(file.path(fixture, "recipe.json"), simplifyVector = FALSE)
    fresh <- nirs4all_multimodal_fit(recipe, file.path(fixture, "train.json"))
    on.exit(n4m::n4m_close(fresh$native), add = TRUE)
    check(stats::predict(fresh, data))
    output <- tempfile("nirs4all-r-multimodal-", fileext = ".json")
    on.exit(unlink(output), add = TRUE)
    nirs4all_multimodal_export(fresh, output)
    replay <- nirs4all_multimodal_load(output)
    on.exit(n4m::n4m_close(replay$native), add = TRUE)
    cli <- Sys.getenv("NIRS4ALL_CORE_CLI")
    on.exit(Sys.setenv(NIRS4ALL_CORE_CLI = cli), add = TRUE)
    Sys.setenv(NIRS4ALL_CORE_CLI = "missing-default-native-cli")
    check(stats::predict(fresh, data))
    check(stats::predict(replay, data))
  })
  message("R_MULTIMODAL_PASS Python_state cold_predict raw_fit unicode schema_refusal reload")
}
