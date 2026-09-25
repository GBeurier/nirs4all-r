library(nirs4all)

X <- outer(seq_len(16L), seq_len(6L), function(i, j) sin(i * j / 9))
y <- 1 + X[, 2L] - 0.4 * X[, 5L]
work <- tempfile("nirs4all-fresh-process-")
dir.create(work)
on.exit(unlink(work, recursive = TRUE), add = TRUE)
matrix_path <- file.path(work, "matrix.rds")
saveRDS(X, matrix_path)
rscript <- file.path(R.home("bin"), "Rscript")
expression <- paste(
  "args <- commandArgs(TRUE); library(nirs4all);",
  "X <- readRDS(args[[2]]); fitted <- nirs4all_load(args[[1]]);",
  "saveRDS(predict(fitted, X), args[[3]])"
)

check_fresh_process <- function(learner, label, tolerance) {
  fitted <- nirs4all_fit(nirs4all_pipeline(learner = learner), X, y)
  expected <- predict(fitted, X)
  bundle_path <- file.path(work, paste0(label, "-bundle.rds"))
  predictions_path <- file.path(work, paste0(label, "-predictions.rds"))
  nirs4all_save(fitted, bundle_path)
  output <- suppressWarnings(system2(rscript,
    c("-e", shQuote(expression), shQuote(bundle_path), shQuote(matrix_path),
      shQuote(predictions_path)), stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L)
    stop(label, " fresh-process replay failed: ", paste(output, collapse = "\n"))
  actual <- readRDS(predictions_path)
  stopifnot(isTRUE(all.equal(actual, expected, tolerance = tolerance)))
}

check_fresh_process(nirs4all_pls(n_components = 2L), "n4m", 1e-12)
if (requireNamespace("mlr3", quietly = TRUE) &&
    requireNamespace("rpart", quietly = TRUE))
  check_fresh_process(nirs4all_mlr3(mlr3::lrn("regr.rpart", minsplit = 3L,
                                             cp = 0)), "mlr3", 1e-12)
if (requireNamespace("torch", quietly = TRUE) && torch::torch_is_installed())
  check_fresh_process(nirs4all_torch_mlp(hidden = 8L, epochs = 10L,
                                        seed = 10L), "torch", 1e-6)
