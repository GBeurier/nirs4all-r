library(nirs4all)

X <- outer(seq_len(21L), seq_len(12L), function(s, b)
  sin(s * b / 9) + cos(s + b / 7) + s * b / 100)
colnames(X) <- paste0("wl", seq_len(ncol(X)))
y <- 1.3 + 0.7 * X[, 2L] - 0.4 * X[, 6L]
held <- X[c(2L, 8L, 17L), , drop = FALSE] + 0.031
params <- list(
  ridge = list(), ridge_pls = list(), robust_pls = list(),
  cppls = list(), sparse_simpls = list(), ecr = list(),
  continuum_regression = list(), mir_pls = list(),
  fused_sparse_pls = list(),
  bagging_pls = list(n_estimators = 7L, seed = 13L),
  boosting_pls = list(n_estimators = 7L, learning_rate = 0.3),
  random_subspace_pls = list(n_estimators = 7L,
    features_per_subspace = 5L, seed = 13L))
rows <- function(value) lapply(seq_len(nrow(value)), function(index)
  unname(as.list(as.numeric(value[index, ]))))
python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON")
python_root <- Sys.getenv("NIRS4ALL_PYTHON_FULL_ROOT")
helper <- if (file.exists("helpers/trained_n4m_peer.py"))
  "helpers/trained_n4m_peer.py" else "tests/helpers/trained_n4m_peer.py"
if (nzchar(python) && nzchar(python_root))
  stopifnot(file.exists(helper), file.exists(python), dir.exists(python_root))

check <- function(pipeline) {
  fitted <- nirs4all_fit(pipeline, X, y)
  bundle_text <- nirs4all_export_trained_pipeline(fitted)
  document <- jsonlite::fromJSON(bundle_text, simplifyVector = FALSE)
  manifest <- jsonlite::fromJSON(document$manifest_json,
    simplifyVector = FALSE)
  method <- pipeline$learner$spec$method
  expected_class <- tail(nirs4all_portable_class_names(
    nirs4all_load_pipeline(nirs4all_export_pipeline(pipeline))), 1L)
  stopifnot(identical(document$schema,
    "nirs4all.n4m.trained_pipeline.v5"),
    identical(manifest$fit_recipe_assertion,
      list(kind = "affine_recipe", recipe_class = expected_class)),
    identical(manifest$preprocessing_owner, "external"),
    identical(n4m::n4m_model_descriptor(
      jsonlite::base64_dec(document$model$payload))$algorithm, 11L))
  imported <- nirs4all_import_trained_pipeline(bundle_text)
  expected <- predict(fitted, held)
  stopifnot(max(abs(predict(imported, held) - expected)) < 1e-12,
    max(abs(predict(nirs4all_retrain(imported, X, y), held) - expected)) < 1e-8)
  stopifnot(inherits(try(predict(imported, held[, rev(seq_len(ncol(held))),
    drop = FALSE]), silent = TRUE), "try-error"),
    inherits(try(predict(imported, unname(held)), silent = TRUE), "try-error"))
  path <- tempfile(fileext = ".rds")
  nirs4all_save(imported, path)
  stopifnot(max(abs(predict(nirs4all_load(path), held) - expected)) < 1e-12)
  unlink(path)
  if (nzchar(python) && nzchar(python_root)) {
    request <- tempfile(fileext = ".json")
    response <- tempfile(fileext = ".json")
    bundle <- tempfile(fileext = ".json")
    python_bundle <- tempfile(fileext = ".json")
    writeLines(bundle_text, bundle, useBytes = TRUE)
    writeLines(as.character(jsonlite::toJSON(list(
      python_root = python_root, bundle = bundle,
      python_bundle = python_bundle, train = rows(X),
      validation = rows(held), y = unname(as.list(y)),
      feature_names = unname(as.list(colnames(X)))),
      auto_unbox = TRUE, digits = 17L)), request)
    output <- suppressWarnings(system2(python,
      c(shQuote(helper), shQuote(request), shQuote(response)),
      stdout = TRUE, stderr = TRUE))
    status <- attr(output, "status")
    if (!is.null(status) && status != 0L)
      stop("Python trained affine peer failed for ", method, ": ",
           paste(output, collapse = "\n"))
    result <- jsonlite::fromJSON(response)
    stopifnot(max(abs(result$predictions - expected)) < 1e-10,
      max(abs(result$retrained_predictions - expected)) < 1e-8,
      max(abs(result$python_predictions - expected)) < 1e-8,
      isTRUE(result$rejected_reordered), isTRUE(result$rejected_unnamed))
    python_fitted <- nirs4all_import_trained_pipeline(python_bundle)
    stopifnot(max(abs(predict(python_fitted, held) - expected)) < 1e-8,
      max(abs(predict(nirs4all_retrain(python_fitted, X, y), held) - expected)) <
        1e-8)
    unlink(c(request, response, bundle, python_bundle))
  }
  list(document = document, manifest = manifest, fitted = fitted)
}

for (method in names(params))
  check(nirs4all_pipeline(learner = nirs4all_n4m_method(
    method, 2L, params[[method]])))

# External train-fitted references and branch widths remain part of the
# manifest; the affine N4MM model only sees the transformed feature width.
profiles <- list(
  msc = nirs4all_pipeline(list(nirs4all_msc()),
    nirs4all_n4m_method("fused_sparse_pls")),
  emsc = nirs4all_pipeline(list(nirs4all_emsc()),
    nirs4all_n4m_method("bagging_pls", params = list(
      n_estimators = 7L, seed = 13L))),
  spa = nirs4all_pipeline(list(nirs4all_spa(5L, 2L)),
    nirs4all_n4m_method("bagging_pls", params = list(
      n_estimators = 7L, seed = 13L))),
  selector = nirs4all_pipeline(list(nirs4all_n4m_selector(
    "wvc_select", 2L, list(top_k = 5L))),
    nirs4all_n4m_method("random_subspace_pls", params = list(
      n_estimators = 7L, features_per_subspace = 5L, seed = 13L))),
  branch = nirs4all_pipeline(list(nirs4all_concat(list(
    baseline = list(nirs4all_snv()), native = list(nirs4all_msc())))),
    nirs4all_n4m_method("ridge")))
for (profile in profiles) check(profile)

original <- check(nirs4all_pipeline(learner = nirs4all_n4m_method(
  "bagging_pls", params = list(n_estimators = 7L, seed = 13L))))
document <- original$document
manifest <- original$manifest
emit <- function(doc) as.character(jsonlite::toJSON(doc, auto_unbox = TRUE,
  null = "null", digits = 17L))
rehash <- function(doc, state) {
  doc$manifest_json <- emit(state)
  doc$manifest_sha256 <- digest::digest(doc$manifest_json,
    algo = "sha256", serialize = FALSE)
  doc
}
bad <- document
bad$manifest_sha256 <- paste0("0", substring(bad$manifest_sha256, 2L))
stopifnot(inherits(try(nirs4all_import_trained_pipeline(emit(bad)),
  silent = TRUE), "try-error"))
bad <- document
bad$model$sha256 <- paste0("0", substring(bad$model$sha256, 2L))
stopifnot(inherits(try(nirs4all_import_trained_pipeline(emit(bad)),
  silent = TRUE), "try-error"))
for (alter in list(
  function(state) { state$input_n_features <- 11L; state },
  function(state) { state$fit_recipe_assertion$recipe_class <- "n4m.Ridge"; state },
  function(state) { state$fit_recipe_assertion$kind <- "trained_ensemble"; state },
  function(state) { state$preprocessing_owner <- "embedded_methods"; state })) {
  bad <- rehash(document, alter(manifest))
  stopifnot(inherits(try(nirs4all_import_trained_pipeline(emit(bad)),
    silent = TRUE), "try-error"))
}
bad <- document
bad$schema <- "nirs4all.n4m.trained_pipeline.v1"
stopifnot(inherits(try(nirs4all_import_trained_pipeline(emit(bad)),
  silent = TRUE), "try-error"))
pls <- nirs4all_fit(nirs4all_pipeline(learner = nirs4all_pls(2L)), X, y)
bad <- document
bytes <- nirs4all_export_native_model(pls)
bad$model$payload <- jsonlite::base64_enc(bytes)
bad$model$sha256 <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
stopifnot(inherits(try(nirs4all_import_trained_pipeline(emit(bad)),
  silent = TRUE), "try-error"))
