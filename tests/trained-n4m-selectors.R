library(nirs4all)

source(if (file.exists("helpers/n4m_selector_cases.R"))
  "helpers/n4m_selector_cases.R" else "tests/helpers/n4m_selector_cases.R")
cases <- n4m_selector_cases()
X <- outer(seq_len(37L), seq_len(12L), function(i, j)
  sin(i * j / 11) + cos(i / 3 + j / 7) + i * j / 170)
y <- 0.9 + 0.6 * X[, 3L] - 0.4 * X[, 9L]
train <- X[1:28, , drop = FALSE]
held <- X[29:37, , drop = FALSE]

for (method in names(cases)) {
  pipeline <- nirs4all_pipeline(list(nirs4all_n4m_selector(method, 2L,
    cases[[method]])), nirs4all_pls(1L))
  fitted <- nirs4all_fit(pipeline, train, y[1:28])
  bundle <- nirs4all_export_trained_pipeline(fitted)
  document <- jsonlite::fromJSON(bundle, simplifyVector = FALSE)
  manifest <- jsonlite::fromJSON(document$manifest_json, simplifyVector = FALSE)
  selected <- fitted$step_states[[1L]]
  stopifnot(identical(document$schema,
    "nirs4all.n4m.trained_pipeline.v4"),
    identical(manifest$step_states[[1L]]$kind, "selector"),
    identical(as.integer(unlist(manifest$step_states[[1L]]$selected_indices)),
              selected - 1L))
  restored <- nirs4all_import_trained_pipeline(bundle)
  expected <- predict(fitted, held)
  stopifnot(identical(restored$step_states, fitted$step_states),
    max(abs(predict(restored, held) - expected)) < 1e-12,
    max(abs(predict(nirs4all_retrain(restored, train, y[1:28]),
                    held) - expected)) < 1e-8)
}

python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON")
python_root <- Sys.getenv("NIRS4ALL_PYTHON_FULL_ROOT")
matrix_rows <- function(value) lapply(seq_len(nrow(value)), function(index)
  unname(as.list(as.numeric(value[index, ]))))
profiles <- list(
  direct = nirs4all_pipeline(list(nirs4all_n4m_selector(
    "wvc_select", 2L, list(top_k = 5L, normalize = FALSE))),
    nirs4all_pls(2L)),
  sequential = nirs4all_pipeline(list(nirs4all_snv(),
    nirs4all_n4m_selector("interval_select", 2L,
      list(interval_width = 3L, step = 1L))), nirs4all_pls(2L)),
  branch = nirs4all_pipeline(list(nirs4all_concat(list(
    native = list(nirs4all_n4m_selector("wvc_select", 2L,
      list(top_k = 5L))), baseline = list(nirs4all_snv())))),
    nirs4all_pls(2L)))
for (name in names(profiles)) {
  fitted <- nirs4all_fit(profiles[[name]], train, y[1:28])
  bundle_text <- nirs4all_export_trained_pipeline(fitted)
  imported <- nirs4all_import_trained_pipeline(bundle_text)
  expected <- predict(fitted, held)
  stopifnot(max(abs(predict(imported, held) - expected)) < 1e-12)
  if (nzchar(python) && nzchar(python_root)) {
    helper <- if (file.exists("helpers/trained_n4m_peer.py"))
      "helpers/trained_n4m_peer.py" else "tests/helpers/trained_n4m_peer.py"
    request <- tempfile(fileext = ".json")
    response <- tempfile(fileext = ".json")
    bundle <- tempfile(fileext = ".json")
    python_bundle <- tempfile(fileext = ".json")
    writeLines(bundle_text, bundle, useBytes = TRUE)
    writeLines(as.character(jsonlite::toJSON(list(
      python_root = python_root, bundle = bundle,
      python_bundle = python_bundle, train = matrix_rows(train),
      validation = matrix_rows(held), y = unname(as.list(y[1:28]))),
      auto_unbox = TRUE, digits = 17L)), request)
    output <- suppressWarnings(system2(python,
      c(shQuote(helper), shQuote(request), shQuote(response)),
      stdout = TRUE, stderr = TRUE))
    status <- attr(output, "status")
    if (is.null(status)) status <- 0L
    if (status != 0L)
      stop("Python generic trained peer failed: ",
           paste(output, collapse = "\n"))
    result <- jsonlite::fromJSON(response)
    stopifnot(max(abs(result$predictions - expected)) < 1e-10,
      max(abs(result$retrained_predictions - expected)) < 1e-8,
      max(abs(result$python_predictions - expected)) < 1e-8)
    python_fitted <- nirs4all_import_trained_pipeline(python_bundle)
    stopifnot(max(abs(predict(python_fitted, held) - expected)) < 1e-8,
      max(abs(predict(nirs4all_retrain(python_fitted, train, y[1:28]),
        held) - expected)) < 1e-8)
    unlink(c(request, response, bundle, python_bundle))
  }
}

fitted <- nirs4all_fit(profiles$direct, train, y[1:28])
document <- jsonlite::fromJSON(nirs4all_export_trained_pipeline(fitted),
  simplifyVector = FALSE)
manifest <- jsonlite::fromJSON(document$manifest_json, simplifyVector = FALSE)
tamper <- function(indices = NULL, schema = NULL, edit_recipe = NULL) {
  changed <- document
  state <- manifest
  if (!is.null(indices)) state$step_states[[1L]]$selected_indices <- indices
  if (!is.null(edit_recipe)) state$recipe <- edit_recipe
  if (!is.null(schema)) changed$schema <- schema
  changed$manifest_json <- as.character(jsonlite::toJSON(state,
    auto_unbox = TRUE, null = "null", digits = 17L))
  changed$manifest_sha256 <- digest::digest(changed$manifest_json,
    algo = "sha256", serialize = FALSE)
  as.character(jsonlite::toJSON(changed, auto_unbox = TRUE))
}
original <- manifest$step_states[[1L]]$selected_indices
invalid <- list(list(0L, 1L, 2L, 3L, 12L),
                list(0L, 1L, 2L, 3L, 3L),
                list(0L, 1L, 2L, 3L, 4.5),
                list(), original[-1L])
stopifnot(all(vapply(invalid, function(indices)
  inherits(try(nirs4all_import_trained_pipeline(tamper(indices)),
               silent = TRUE), "try-error"), logical(1))))
for (schema in c("nirs4all.n4m.trained_pipeline.v1",
                 "nirs4all.n4m.trained_pipeline.v3"))
  stopifnot(inherits(try(nirs4all_import_trained_pipeline(tamper(
    schema = schema)), silent = TRUE), "try-error"))
changed_recipe <- manifest$recipe
changed_recipe$pipeline[[2L]]$model$params$n_components <- 1L
stopifnot(inherits(try(nirs4all_import_trained_pipeline(tamper(
  edit_recipe = changed_recipe)), silent = TRUE), "try-error"))
bad_hash <- document
bad_hash$manifest_sha256 <- paste0(
  if (startsWith(document$manifest_sha256, "0")) "1" else "0",
  substring(document$manifest_sha256, 2L))
stopifnot(inherits(try(nirs4all_import_trained_pipeline(as.character(
  jsonlite::toJSON(bad_hash, auto_unbox = TRUE))), silent = TRUE), "try-error"))
bad_hash <- document
bad_hash$model$sha256 <- paste0(
  if (startsWith(document$model$sha256, "0")) "1" else "0",
  substring(document$model$sha256, 2L))
stopifnot(inherits(try(nirs4all_import_trained_pipeline(as.character(
  jsonlite::toJSON(bad_hash, auto_unbox = TRUE))), silent = TRUE), "try-error"))
mixed <- nirs4all_pipeline(list(nirs4all_spa(5L, 2L),
  nirs4all_n4m_selector("wvc_select", 2L, list(top_k = 3L))),
  nirs4all_pls(1L))
stopifnot(inherits(try(nirs4all_export_trained_pipeline(
  nirs4all_fit(mixed, train, y[1:28])), silent = TRUE), "try-error"))
