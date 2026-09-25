library(nirs4all)

X <- outer(seq_len(24L), seq_len(8L),
  function(i, j) sin(i * j / 7) + i * j / 50)
y <- X[, 2L] - 0.3 * X[, 5L]
train <- X[1:17, , drop = FALSE]
validation <- X[18:24, , drop = FALSE]
recipes <- list(
  plain = nirs4all_pipeline(learner = nirs4all_pls(2L)),
  stateless = nirs4all_pipeline(list(nirs4all_snv(),
    nirs4all_detrend(1L)), nirs4all_pls(2L)),
  stateful = nirs4all_pipeline(list(nirs4all_msc(),
    nirs4all_emsc(2L)), nirs4all_pls(2L)),
  embedded = nirs4all_pipeline(list(nirs4all_snv(),
    nirs4all_savgol(5L)), nirs4all_pls(2L)),
  branch = nirs4all_pipeline(list(nirs4all_concat(list(
    msc = list(nirs4all_msc()),
    emsc = list(nirs4all_emsc(2L))))), nirs4all_pls(2L)))

python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON")
python_root <- Sys.getenv("NIRS4ALL_PYTHON_FULL_ROOT")
for (name in names(recipes)) {
  fitted <- nirs4all_fit(recipes[[name]], train, y[1:17])
  text <- nirs4all_export_trained_pipeline(fitted)
  restored <- nirs4all_import_trained_pipeline(text)
  expected <- as.numeric(predict(fitted, validation))
  manifest <- jsonlite::fromJSON(
    jsonlite::fromJSON(text, simplifyVector = FALSE)$manifest_json,
    simplifyVector = FALSE)
  stopifnot(max(abs(predict(restored, validation) - expected)) < 1e-12,
    isTRUE(all.equal(nirs4all_pipeline_from_portable(
      manifest$recipe),
      recipes[[name]])))
  bundle <- tempfile(fileext = ".json")
  nirs4all_export_trained_pipeline(fitted, bundle)
  stopifnot(max(abs(predict(nirs4all_import_trained_pipeline(bundle),
                            validation) - expected)) < 1e-12)
  if (nzchar(python) && nzchar(python_root)) {
    helper <- if (file.exists("helpers/trained_n4m_peer.py"))
      "helpers/trained_n4m_peer.py" else
      "tests/helpers/trained_n4m_peer.py"
    request <- tempfile(fileext = ".json")
    response <- tempfile(fileext = ".json")
    python_bundle <- tempfile(fileext = ".json")
    writeLines(as.character(jsonlite::toJSON(list(
      python_root = python_root, bundle = bundle,
      python_bundle = python_bundle,
      train = unname(split(train, row(train))),
      validation = unname(split(validation, row(validation))),
      y = unname(as.list(y[1:17]))), auto_unbox = TRUE,
      digits = 17L)), request)
    output <- suppressWarnings(system2(python,
      c(shQuote(helper), shQuote(request), shQuote(response)),
      stdout = TRUE, stderr = TRUE))
    status <- attr(output, "status")
    if (is.null(status)) status <- 0L
    if (status != 0L)
      stop(sprintf("Python trained %s replay failed: %s",
        name, paste(output, collapse = "\n")))
    result <- jsonlite::fromJSON(response)
    stopifnot(max(abs(result$predictions - expected)) < 1e-10,
      max(abs(result$retrained_predictions - expected)) < 1e-8,
      max(abs(result$python_predictions - expected)) < 1e-8)
    python_fitted <- nirs4all_import_trained_pipeline(python_bundle)
    stopifnot(max(abs(predict(python_fitted, validation) - expected)) < 1e-8)
    r_retrained <- nirs4all_retrain(python_fitted, train, y[1:17])
    stopifnot(max(abs(predict(r_retrained, validation) - expected)) < 1e-8,
      !identical(r_retrained$state, python_fitted$state))
    unlink(c(request, response, python_bundle))
  }
  unlink(bundle)
}

fitted <- nirs4all_fit(recipes$stateful, train, y[1:17])
document <- jsonlite::fromJSON(nirs4all_export_trained_pipeline(fitted),
                                simplifyVector = FALSE)
tampered <- document
tampered$model$sha256 <- paste0("0", substring(document$model$sha256, 2L))
stopifnot(inherits(try(nirs4all_import_trained_pipeline(as.character(
  jsonlite::toJSON(tampered, auto_unbox = TRUE))), silent = TRUE), "try-error"))
tampered <- document
tampered$manifest_json <- sub("n4m.MSC", "n4m.SNV", tampered$manifest_json,
                              fixed = TRUE)
stopifnot(inherits(try(nirs4all_import_trained_pipeline(as.character(
  jsonlite::toJSON(tampered, auto_unbox = TRUE))),
  silent = TRUE), "try-error"))
tampered <- document
manifest <- jsonlite::fromJSON(tampered$manifest_json, simplifyVector = FALSE)
manifest$step_states[[1L]]$reference[[1L]] <- Inf
tampered$manifest_json <- as.character(jsonlite::toJSON(manifest,
  auto_unbox = TRUE, na = "null"))
tampered$manifest_sha256 <- digest::digest(tampered$manifest_json,
  algo = "sha256", serialize = FALSE)
stopifnot(inherits(try(nirs4all_import_trained_pipeline(as.character(
  jsonlite::toJSON(tampered, auto_unbox = TRUE))), silent = TRUE), "try-error"))

nondefault <- nirs4all_pipeline(list(nirs4all_snv(ddof = 1L)),
                                nirs4all_pls(2L))
stopifnot(inherits(try(nirs4all_export_trained_pipeline(
  nirs4all_fit(nondefault, train, y[1:17])), silent = TRUE), "try-error"))
named_train <- train
named_validation <- validation
colnames(named_train) <- colnames(named_validation) <- paste0("w", seq_len(ncol(X)))
named_fit <- nirs4all_fit(recipes$stateful, named_train, y[1:17])
named_restore <- nirs4all_import_trained_pipeline(
  nirs4all_export_trained_pipeline(named_fit))
stopifnot(max(abs(predict(named_restore, named_validation) -
                  predict(named_fit, named_validation))) < 1e-12,
  inherits(try(nirs4all_retrain(named_restore, train, y[1:17]),
    silent = TRUE), "try-error"),
  inherits(try(predict(named_restore, validation), silent = TRUE), "try-error"),
  inherits(try(predict(named_restore,
    named_validation[, rev(seq_len(ncol(X))), drop = FALSE]), silent = TRUE),
    "try-error"))
raw_fit <- nirs4all_fit(recipes$plain, train, y[1:17])
raw_imported <- nirs4all_import_native_model(
  nirs4all_export_native_model(raw_fit), recipes$plain)
portable_again <- nirs4all_import_trained_pipeline(
  nirs4all_export_trained_pipeline(raw_imported))
stopifnot(max(abs(predict(portable_again, validation) -
                  predict(raw_fit, validation))) < 1e-12)
message("trained native R/Python portable replay and retraining passed")
