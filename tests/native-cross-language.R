python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON", "")
if (nzchar(python)) {
  library(nirs4all)
  stopifnot(file.exists(python))
  helper <- if (file.exists("helpers/native_n4mm_peer.py"))
    "helpers/native_n4mm_peer.py" else "tests/helpers/native_n4mm_peer.py"
  stopifnot(file.exists(helper))
  X <- matrix(seq_len(120) / 37, nrow = 12L, ncol = 10L)
  X <- X + outer(seq_len(12L), seq_len(10L),
                 function(i, j) sin(i * j / 7))
  y <- 2 + 0.7 * X[, 2L] - 0.3 * X[, 7L]
  pipeline <- nirs4all_pipeline(
    list(nirs4all_snv(), nirs4all_savgol(5L)), nirs4all_pls(2L))
  recipe_json <- nirs4all_export_pipeline(pipeline, "json")
  stopifnot(inherits(nirs4all_pipeline_from_portable(recipe_json),
                     "nirs4all_pipeline"))
  fitted <- nirs4all_fit(pipeline, X, y)
  stopifnot(identical(fitted$preprocessing_owner, "embedded_methods"))
  bytes <- nirs4all_export_native_model(fitted)
  work <- tempfile("nirs4all-native-peer-")
  dir.create(work)
  model_file <- file.path(work, "model.n4mm")
  request_file <- file.path(work, "request.json")
  result_file <- file.path(work, "result.json")
  writeBin(bytes, model_file)
  writeLines(as.character(jsonlite::toJSON(
    list(X = lapply(seq_len(nrow(X)), function(index)
      unname(as.numeric(X[index, ]))), y = unname(as.numeric(y))),
    auto_unbox = TRUE,
    digits = NA)), request_file)
  output <- suppressWarnings(system2(python,
    c(shQuote(helper), "predict", shQuote(model_file), shQuote(request_file),
      shQuote(result_file)), stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L) stop("Python native replay failed: ",
                         paste(output, collapse = "\n"))
  actual <- jsonlite::fromJSON(result_file)$predictions
  expected <- predict(fitted, X)
  stopifnot(length(actual) == length(expected),
            max(abs(actual - expected)) < 1e-10)
  unlink(c(model_file, result_file))
  output <- suppressWarnings(system2(python,
    c(shQuote(helper), "fit", shQuote(model_file), shQuote(request_file),
      shQuote(result_file)), stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L) stop("Python native fit failed: ",
                         paste(output, collapse = "\n"))
  python_bytes <- readBin(model_file, what = "raw", n = file.info(model_file)$size)
  imported <- nirs4all_import_native_model(python_bytes, recipe_json)
  python_predictions <- jsonlite::fromJSON(result_file)$predictions
  stopifnot(max(abs(predict(imported, X) - python_predictions)) < 1e-10,
            max(abs(predict(imported, X) - expected)) < 1e-10)
  plain_pipeline <- nirs4all_pipeline(learner = nirs4all_pls(2L))
  plain_recipe <- nirs4all_export_pipeline(plain_pipeline, "json")
  plain_fit <- nirs4all_fit(plain_pipeline, X, y)
  plain_bytes <- nirs4all_export_native_model(plain_fit)
  stopifnot(identical(n4m::n4m_model_descriptor(plain_bytes)$format_version, 1L))
  writeBin(plain_bytes, model_file)
  output <- suppressWarnings(system2(python,
    c(shQuote(helper), "predict_plain", shQuote(model_file), shQuote(request_file),
      shQuote(result_file)), stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L) stop("Python plain N4MM replay failed: ",
                         paste(output, collapse = "\n"))
  plain_expected <- predict(plain_fit, X)
  stopifnot(max(abs(jsonlite::fromJSON(result_file)$predictions -
                    plain_expected)) < 1e-10)
  unlink(c(model_file, result_file))
  output <- suppressWarnings(system2(python,
    c(shQuote(helper), "fit_plain", shQuote(model_file), shQuote(request_file),
      shQuote(result_file)), stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L) stop("Python plain N4MM fit failed: ",
                         paste(output, collapse = "\n"))
  plain_python_bytes <- readBin(model_file, what = "raw",
                                n = file.info(model_file)$size)
  plain_imported <- nirs4all_import_native_model(plain_python_bytes, plain_recipe)
  stopifnot(max(abs(predict(plain_imported, X) -
                    jsonlite::fromJSON(result_file)$predictions)) < 1e-10,
            max(abs(predict(plain_imported, X) - plain_expected)) < 1e-10)
  unlink(work, recursive = TRUE)
}
