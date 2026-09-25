library(nirs4all)

rows <- as.vector(rbind(1:12, 51:62, 101:112))
validation <- as.vector(rbind(13:15, 63:65, 113:115))
X <- as.matrix(iris[rows, 1:4])
y <- iris$Species[rows]
held <- as.matrix(iris[validation, 1:4])
python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON")
python_root <- Sys.getenv("NIRS4ALL_PYTHON_FULL_ROOT")
matrix_rows <- function(value) lapply(seq_len(nrow(value)), function(index)
  unname(as.list(as.numeric(value[index, ]))))

for (steps in list(list(), list(nirs4all_snv()), list(nirs4all_msc()))) {
  pipeline <- nirs4all_pipeline(steps, nirs4all_sparse_pls_da(2L, 0.05))
  fitted <- nirs4all_fit(pipeline, X, y)
  bundle <- nirs4all_export_trained_pipeline(fitted)
  document <- jsonlite::fromJSON(bundle, simplifyVector = FALSE)
  stopifnot(identical(document$schema, "nirs4all.n4m.trained_pipeline.v2"))
  imported <- nirs4all_import_trained_pipeline(bundle)
  expected <- predict(fitted, held)
  expected_proba <- nirs4all_predict_proba(fitted, held)
  stopifnot(identical(predict(imported, held), expected),
            identical(nirs4all_predict_proba(imported, held), expected_proba))
  refitted <- nirs4all_retrain(imported, X, y)
  stopifnot(identical(predict(refitted, held), expected))

  tampered <- document
  tampered$model$sha256 <- paste0(
    if (startsWith(document$model$sha256, "0")) "1" else "0",
    substring(document$model$sha256, 2L))
  stopifnot(inherits(try(nirs4all_import_trained_pipeline(as.character(
    jsonlite::toJSON(tampered, auto_unbox = TRUE))), silent = TRUE), "try-error"))
  tampered <- document
  manifest <- jsonlite::fromJSON(tampered$manifest_json, simplifyVector = FALSE)
  manifest$classes <- manifest$classes[-1L]
  tampered$manifest_json <- as.character(jsonlite::toJSON(manifest,
    auto_unbox = TRUE, null = "null", digits = 17L))
  tampered$manifest_sha256 <- digest::digest(tampered$manifest_json,
    algo = "sha256", serialize = FALSE)
  stopifnot(inherits(try(nirs4all_import_trained_pipeline(as.character(
    jsonlite::toJSON(tampered, auto_unbox = TRUE))), silent = TRUE), "try-error"))

  if (nzchar(python) && nzchar(python_root)) {
    helper <- if (file.exists("helpers/trained_sparse_plsda_peer.py"))
      "helpers/trained_sparse_plsda_peer.py" else
      "tests/helpers/trained_sparse_plsda_peer.py"
    request <- tempfile(fileext = ".json")
    response <- tempfile(fileext = ".json")
    r_bundle <- tempfile(fileext = ".json")
    python_bundle <- tempfile(fileext = ".json")
    writeLines(bundle, r_bundle, useBytes = TRUE)
    recipe <- jsonlite::fromJSON(nirs4all_export_pipeline(pipeline),
      simplifyVector = FALSE)
    writeLines(as.character(jsonlite::toJSON(list(
      python_root = python_root, r_bundle = r_bundle,
      python_bundle = python_bundle, recipe = recipe,
      feature_names = unname(as.list(colnames(X))),
      train = matrix_rows(X), validation = matrix_rows(held),
      labels = unname(as.list(as.character(y)))),
      auto_unbox = TRUE, digits = 17L)), request)
    output <- suppressWarnings(system2(python,
      c(shQuote(helper), shQuote(request), shQuote(response)),
      stdout = TRUE, stderr = TRUE))
    status <- attr(output, "status")
    if (is.null(status)) status <- 0L
    if (status != 0L)
      stop("Python sparse PLS-DA peer failed: ", paste(output, collapse = "\n"))
    result <- jsonlite::fromJSON(response)
    stopifnot(identical(result$classes, levels(y)),
      identical(result$r_predictions, as.character(expected)),
      identical(result$retrained_predictions, as.character(expected)),
      max(abs(result$r_probabilities - expected_proba)) < 1e-10,
      max(abs(result$r_scores - result$python_scores)) < 1e-8,
      identical(result$python_predictions, as.character(expected)))
    python_fitted <- nirs4all_import_trained_pipeline(python_bundle)
    stopifnot(identical(predict(python_fitted, held), expected),
      max(abs(nirs4all_predict_proba(python_fitted, held) -
        expected_proba)) < 1e-8)
    unlink(c(request, response, r_bundle, python_bundle))
  }
}
message("trained sparse PLS-DA R/Python transfer passed")
