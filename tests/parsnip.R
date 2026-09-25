if (requireNamespace("parsnip", quietly = TRUE)) {
  library(nirs4all)
  X <- outer(seq_len(18L), seq_len(4L),
             function(i, j) sin(i * j / 7) + i * j / 50)
  y <- 2 + 0.7 * X[, 2L] - 0.3 * X[, 4L]
  spec <- parsnip::set_engine(parsnip::linear_reg(), "lm")
  pipeline <- nirs4all_pipeline(learner = nirs4all_parsnip(spec))
  fitted <- nirs4all_fit(pipeline, X[1:12, , drop = FALSE], y[1:12])
  reference_x <- as.data.frame(X[1:12, , drop = FALSE])
  names(reference_x) <- paste0("x", seq_len(ncol(X)))
  reference <- parsnip::fit_xy(spec, x = reference_x, y = y[1:12])
  test_x <- as.data.frame(X[13:18, , drop = FALSE])
  names(test_x) <- names(reference_x)
  expected <- stats::predict(reference, new_data = test_x, type = "numeric")$.pred
  stopifnot(max(abs(predict(fitted, X[13:18, , drop = FALSE]) - expected)) < 1e-12)
  model_file <- tempfile(fileext = ".rds")
  nirs4all_save(fitted, model_file)
  stopifnot(max(abs(predict(nirs4all_load(model_file), X[13:18, , drop = FALSE]) -
                    expected)) < 1e-12)
  unlink(model_file)
  if (requireNamespace("ranger", quietly = TRUE)) {
    forest <- parsnip::set_engine(
      parsnip::rand_forest(trees = 20L, mode = "regression"),
      "ranger", seed = 5L, num.threads = 1L)
    forest_fit <- nirs4all_fit(nirs4all_pipeline(
      learner = nirs4all_parsnip(forest)), X[1:12, , drop = FALSE], y[1:12])
    forest_reference <- parsnip::fit_xy(forest, x = reference_x, y = y[1:12])
    forest_expected <- stats::predict(forest_reference, new_data = test_x,
                                      type = "numeric")$.pred
    stopifnot(max(abs(predict(forest_fit, X[13:18, , drop = FALSE]) -
                      forest_expected)) < 1e-12)
  }
  classification <- parsnip::set_engine(
    parsnip::rand_forest(mode = "classification"), "ranger")
  stopifnot(inherits(try(nirs4all_parsnip(classification), silent = TRUE),
                     "try-error"))
}
