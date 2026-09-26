library(nirs4all)

X <- outer(seq_len(37L), seq_len(12L), function(i, j)
  sin(i * j / 11) + cos(i / 3 + j / 7) + i * j / 170)
y <- 0.9 + 0.6 * X[, 3L] - 0.4 * X[, 9L]
train <- X[1:28, , drop = FALSE]
validation <- X[29:37, , drop = FALSE]
recipes <- list(
  direct = nirs4all_pipeline(list(nirs4all_spa(5L, 2L)),
                             nirs4all_pls(2L)),
  sequential = nirs4all_pipeline(list(nirs4all_spa(5L, 2L),
    nirs4all_msc()), nirs4all_pls(2L)),
  branch = nirs4all_pipeline(list(nirs4all_concat(list(
    selected = list(nirs4all_spa(5L, 2L)),
    baseline = list(nirs4all_snv())))), nirs4all_pls(2L)))
python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON")
python_root <- Sys.getenv("NIRS4ALL_PYTHON_FULL_ROOT")
matrix_rows <- function(value) lapply(seq_len(nrow(value)), function(index)
  unname(as.list(as.numeric(value[index, ]))))

for (name in names(recipes)) {
  fitted <- nirs4all_fit(recipes[[name]], train, y[1:28])
  text <- nirs4all_export_trained_pipeline(fitted)
  document <- jsonlite::fromJSON(text, simplifyVector = FALSE)
  manifest <- jsonlite::fromJSON(document$manifest_json, simplifyVector = FALSE)
  wire_state <- if (!identical(name, "branch")) manifest$step_states[[1L]] else
    manifest$step_states[[1L]]$branches$selected[[1L]]
  ranked <- if (!identical(name, "branch")) fitted$step_states[[1L]] else
    fitted$step_states[[1L]]$selected[[1L]]
  stopifnot(identical(document$schema, "nirs4all.n4m.trained_pipeline.v3"),
    identical(wire_state$kind, "selector"),
    identical(as.integer(unlist(wire_state$selected_indices)), ranked - 1L))
  imported <- nirs4all_import_trained_pipeline(text)
  expected <- predict(fitted, validation)
  stopifnot(max(abs(predict(imported, validation) - expected)) < 1e-12,
    max(abs(predict(nirs4all_retrain(imported, train, y[1:28]),
                    validation) - expected)) < 1e-10)

  if (identical(name, "direct")) {
    mutate <- function(value) {
      altered <- document
      changed <- manifest
      changed$step_states[[1L]]$selected_indices <- value
      altered$manifest_json <- as.character(jsonlite::toJSON(changed,
        auto_unbox = TRUE, null = "null", digits = 17L))
      altered$manifest_sha256 <- digest::digest(altered$manifest_json,
        algo = "sha256", serialize = FALSE)
      as.character(jsonlite::toJSON(altered, auto_unbox = TRUE))
    }
    invalid <- list(list(0L, 1L),
      list(0L, 1L, 2L, 3L, 12L),
      list(0L, 1L, 2L, 3L, 3L),
      list(0, 1, 2, 3, 4.5),
      list("0", 1L, 2L, 3L, 4L))
    stopifnot(all(vapply(invalid, function(indices)
      inherits(try(nirs4all_import_trained_pipeline(mutate(indices)),
                   silent = TRUE), "try-error"), logical(1))))
    altered <- document
    altered$schema <- "nirs4all.n4m.trained_pipeline.v1"
    stopifnot(inherits(try(nirs4all_import_trained_pipeline(
      as.character(jsonlite::toJSON(altered, auto_unbox = TRUE))),
      silent = TRUE), "try-error"))
  }

  if (nzchar(python) && nzchar(python_root)) {
    helper <- if (file.exists("helpers/trained_n4m_peer.py"))
      "helpers/trained_n4m_peer.py" else "tests/helpers/trained_n4m_peer.py"
    request <- tempfile(fileext = ".json")
    response <- tempfile(fileext = ".json")
    bundle <- tempfile(fileext = ".json")
    python_bundle <- tempfile(fileext = ".json")
    writeLines(text, bundle, useBytes = TRUE)
    writeLines(as.character(jsonlite::toJSON(list(
      python_root = python_root, bundle = bundle,
      python_bundle = python_bundle,
      train = matrix_rows(train), validation = matrix_rows(validation),
      y = unname(as.list(y[1:28]))), auto_unbox = TRUE,
      digits = 17L)), request)
    output <- suppressWarnings(system2(python,
      c(shQuote(helper), shQuote(request), shQuote(response)),
      stdout = TRUE, stderr = TRUE))
    status <- attr(output, "status")
    if (is.null(status)) status <- 0L
    if (status != 0L)
      stop("Python trained SPA peer failed: ", paste(output, collapse = "\n"))
    result <- jsonlite::fromJSON(response)
    stopifnot(max(abs(result$predictions - expected)) < 1e-10,
      max(abs(result$retrained_predictions - expected)) < 1e-8,
      max(abs(result$python_predictions - expected)) < 1e-8)
    python_fitted <- nirs4all_import_trained_pipeline(python_bundle)
    stopifnot(max(abs(predict(python_fitted, validation) - expected)) < 1e-8,
      max(abs(predict(nirs4all_retrain(python_fitted, train, y[1:28]),
                      validation) - expected)) < 1e-8)
    unlink(c(request, response, bundle, python_bundle))
  }
}
