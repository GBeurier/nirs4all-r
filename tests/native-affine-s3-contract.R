library(nirs4all)

required <- c("n4m_affine_fit", "n4m_affine_supported_methods")
if (all(required %in% getNamespaceExports("n4m"))) {
  X <- outer(seq_len(21L), seq_len(12L), function(i, j)
    sin(i * j / 9) + cos(i + j / 7) + i * j / 100)
  colnames(X) <- paste0("wl", seq_len(ncol(X)))
  y <- 1.3 + 0.7 * X[, 2L] - 0.4 * X[, 6L]
  held <- X[c(2L, 8L, 17L), , drop = FALSE] + 0.031
  groups <- stats::setNames(rep(0:2, each = 4L), colnames(X))
  expected <- c(1.2560303930849952, 1.8411427818857744,
                0.8454051444648407)
  stopifnot("group_sparse_pls" %in%
    n4m::n4m_affine_supported_methods())
  pipeline <- nirs4all_pipeline(learner =
    nirs4all_group_sparse_pls(2L, groups, 0.05))
  fitted <- nirs4all_fit(pipeline, X, y)
  stopifnot(max(abs(predict(fitted, held) - expected)) < 1e-10)
  bytes <- nirs4all_export_native_model(fitted)
  restored <- nirs4all_import_native_model(bytes, pipeline, colnames(X))
  stopifnot(max(abs(predict(restored, held) - expected)) < 1e-10,
    identical(nirs4all_export_native_model(restored), bytes))

  fused <- nirs4all_fit(nirs4all_pipeline(learner =
    nirs4all_n4m_method("fused_sparse_pls", 2L)), X, y)
  fused_expected <- c(1.3373539467794611, 1.9377148952044152,
                      0.7544322586932715)
  stopifnot("fused_sparse_pls" %in%
      n4m::n4m_affine_supported_methods(),
    max(abs(predict(fused, held) - fused_expected)) < 1e-10)

  # Ridge now carries the native affine marker, including its direct
  # intercept schema, and remains portable through the product controller.
  ridge <- nirs4all_fit(nirs4all_pipeline(learner =
    nirs4all_n4m_method("ridge", 2L)), X, y)
  raw_ridge <- n4m::n4m_method("ridge", X, y, 2L)
  ridge_expected <- as.numeric(sweep(held, 2L,
    as.numeric(raw_ridge$x_mean), "-") %*%
    raw_ridge$coefficients + as.numeric(raw_ridge$y_mean))
  ridge_bytes <- nirs4all_export_native_model(ridge)
  ridge_roundtrip <- nirs4all_import_native_model(ridge_bytes,
    nirs4all_pipeline(learner = nirs4all_n4m_method("ridge", 2L)),
    colnames(X))
  stopifnot("ridge" %in% n4m::n4m_affine_supported_methods(),
    max(abs(predict(ridge, held) - ridge_expected)) < 1e-10,
    max(abs(predict(ridge_roundtrip, held) - ridge_expected)) < 1e-10)

  python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON", "")
  if (nzchar(python)) {
    helper <- if (file.exists("helpers/native_n4mm_peer.py"))
      "helpers/native_n4mm_peer.py" else "tests/helpers/native_n4mm_peer.py"
    stopifnot(file.exists(helper), file.exists(python))
    rows <- function(values) lapply(seq_len(nrow(values)), function(i)
      unname(as.numeric(values[i, ])))
    work <- tempfile("nirs4all-native-affine-s3-")
    dir.create(work)
    model_file <- file.path(work, "model.n4mm")
    request <- file.path(work, "request.json")
    response <- file.path(work, "response.json")
    writeBin(bytes, model_file)
    writeLines(as.character(jsonlite::toJSON(list(
      X = rows(X), predict_X = rows(held)),
      auto_unbox = TRUE, digits = 17L)), request)
    output <- suppressWarnings(system2(python,
      c(shQuote(helper), "predict_affine", shQuote(model_file),
        shQuote(request), shQuote(response)), stdout = TRUE, stderr = TRUE))
    status <- attr(output, "status")
    if (!is.null(status) && status != 0L)
      stop("Python native affine peer failed: ",
        paste(output, collapse = "\n"))
    stopifnot(max(abs(as.numeric(jsonlite::fromJSON(response)$predictions) -
      expected)) < 1e-10)
    unlink(work, recursive = TRUE)
  }
  message("n4m R S3 GroupSparse and Ridge native model contracts passed")
}
