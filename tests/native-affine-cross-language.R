library(nirs4all)

X <- outer(seq_len(21L), seq_len(8L),
           function(i, j) sin(i * j / 9) + cos(i + j / 7) + i * j / 100)
colnames(X) <- paste0("wl", seq_len(ncol(X)))
X_test <- X[c(2L, 8L, 17L), , drop = FALSE] + 0.031
y <- 1.3 + 0.7 * X[, 2L] - 0.4 * X[, 6L]
methods <- c("ridge", "ridge_pls", "robust_pls", "cppls",
             "sparse_simpls", "ecr", "continuum_regression", "mir_pls")
for (method in methods) {
  learner <- nirs4all_n4m_method(method, 2L)
  pipeline <- nirs4all_pipeline(learner = learner)
  fitted <- nirs4all_fit(pipeline, X, y)
  expected <- predict(fitted, X_test)
  stopifnot(!is.null(fitted$state$native_model),
    max(abs(expected - as.numeric(n4m::n4m_predict(
      fitted$state$native_model, X_test)))) < 1e-12)
  bytes <- nirs4all_export_native_model(fitted)
  descriptor <- n4m::n4m_model_descriptor(bytes)
  stopifnot(identical(descriptor$algorithm, 11L),
            identical(descriptor$n_targets, 1L),
            identical(descriptor$n_features, ncol(X)))
  restored <- nirs4all_import_native_model(bytes, pipeline, colnames(X))
  stopifnot(max(abs(predict(restored, X_test) - expected)) < 1e-10,
            identical(nirs4all_export_native_model(restored), bytes))
  path <- tempfile(fileext = ".rds")
  nirs4all_save(fitted, path)
  stopifnot(max(abs(predict(nirs4all_load(path), X_test) - expected)) < 1e-10)
  unlink(path)
  bad <- try(nirs4all_import_native_model(bytes[-length(bytes)], pipeline),
             silent = TRUE)
  stopifnot(inherits(bad, "try-error"))
  bad <- try(predict(restored, X_test[, rev(seq_len(ncol(X_test)))]), silent = TRUE)
  stopifnot(inherits(bad, "try-error"))
}

ridge <- nirs4all_fit(nirs4all_pipeline(learner = nirs4all_n4m_method("ridge")), X, y)
ridge_bytes <- nirs4all_export_native_model(ridge)
pls_recipe <- nirs4all_pipeline(learner = nirs4all_pls(2L))
stopifnot(inherits(try(nirs4all_import_native_model(ridge_bytes, pls_recipe),
                       silent = TRUE), "try-error"))
with_step <- nirs4all_fit(nirs4all_pipeline(
  list(nirs4all_snv()), nirs4all_n4m_method("ridge")), X, y)
stopifnot(inherits(try(nirs4all_export_native_model(with_step), silent = TRUE),
                   "try-error"))
step_path <- tempfile(fileext = ".rds")
nirs4all_save(with_step, step_path)
stopifnot(max(abs(predict(nirs4all_load(step_path), X_test) -
                  predict(with_step, X_test))) < 1e-10)
unlink(step_path)

python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON", "")
if (nzchar(python)) {
  helper <- if (file.exists("helpers/native_n4mm_peer.py"))
    "helpers/native_n4mm_peer.py" else "tests/helpers/native_n4mm_peer.py"
  work <- tempfile("nirs4all-affine-peer-")
  dir.create(work)
  model_file <- file.path(work, "model.n4mm")
  request_file <- file.path(work, "request.json")
  result_file <- file.path(work, "result.json")
  ridge_intercept <- ridge$state$intercept
  if (is.null(ridge_intercept))
    ridge_intercept <- ridge$state$y_mean -
      drop(ridge$state$x_mean %*% ridge$state$coefficients)
  request <- list(
    X = lapply(seq_len(nrow(X)), function(i) unname(as.numeric(X[i, ]))),
    predict_X = lapply(seq_len(nrow(X_test)), function(i)
      unname(as.numeric(X_test[i, ]))),
    coefficients = lapply(seq_len(ncol(X)), function(i)
      as.numeric(ridge$state$coefficients[i, 1L])),
    intercept = unname(as.numeric(ridge_intercept)))
  writeLines(as.character(jsonlite::toJSON(request, auto_unbox = FALSE,
                                            digits = NA)), request_file)
  peer <- function(mode) {
    output <- suppressWarnings(system2(python,
      c(shQuote(helper), mode, shQuote(model_file), shQuote(request_file),
        shQuote(result_file)), stdout = TRUE, stderr = TRUE))
    status <- attr(output, "status")
    if (!is.null(status) && status != 0L)
      stop("Python affine peer failed: ", paste(output, collapse = "\n"))
    jsonlite::fromJSON(result_file)$predictions
  }
  writeBin(ridge_bytes, model_file)
  stopifnot(max(abs(peer("predict_affine") - predict(ridge, X_test))) < 1e-10)
  unlink(c(model_file, result_file))
  python_predictions <- peer("fit_affine")
  python_bytes <- readBin(model_file, "raw", n = file.info(model_file)$size)
  imported <- nirs4all_import_native_model(python_bytes,
    nirs4all_pipeline(learner = nirs4all_n4m_method("ridge")), colnames(X))
  stopifnot(max(abs(predict(imported, X_test) - python_predictions)) < 1e-10,
            max(abs(predict(imported, X_test) - predict(ridge, X_test))) < 1e-10)
  unlink(work, recursive = TRUE)
}
