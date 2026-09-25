if (requireNamespace("torch", quietly = TRUE) && torch::torch_is_installed()) {
  library(nirs4all)
  X <- outer(seq_len(16L), seq_len(6L),
             function(i, j) sin(i * j / 9) + i * j / 50)
  y <- 1 + 0.6 * X[, 2L] - 0.2 * X[, 5L]
  builder <- local({
    widths <- c(7L, 4L)
    function(n_features) torch::nn_sequential(
      torch::nn_linear(n_features, widths[[1L]]), torch::nn_tanh(),
      torch::nn_linear(widths[[1L]], widths[[2L]]), torch::nn_relu(),
      torch::nn_linear(widths[[2L]], 1L))
  })
  controller <- nirs4all_torch_module(builder, name = "two_hidden",
    epochs = 12L, learning_rate = 0.01, seed = 19L)
  pipeline <- nirs4all_pipeline(learner = controller)
  fitted <- nirs4all_fit(pipeline, X[1:12, , drop = FALSE], y[1:12])
  independent <- nirs4all_fit(pipeline, X[1:12, , drop = FALSE], y[1:12])
  actual <- predict(fitted, X[13:16, , drop = FALSE])
  stopifnot(length(actual) == 4L, all(is.finite(actual)),
            max(abs(actual - predict(independent,
                                     X[13:16, , drop = FALSE]))) < 1e-6,
            !identical(fitted$state$module, independent$state$module))
  path <- tempfile(fileext = ".rds")
  nirs4all_save(fitted, path)
  stopifnot(max(abs(predict(nirs4all_load(path), X[13:16, , drop = FALSE]) -
                    actual)) < 1e-6)
  unlink(path)

  wrong_type <- nirs4all_torch_module(function(n_features) n_features,
                                     epochs = 1L)
  stopifnot(inherits(try(nirs4all_fit(nirs4all_pipeline(learner = wrong_type),
                                    X, y), silent = TRUE), "try-error"))
  wrong_shape <- nirs4all_torch_module(function(n_features)
    torch::nn_linear(n_features, 2L), epochs = 1L)
  stopifnot(inherits(try(nirs4all_fit(nirs4all_pipeline(learner = wrong_shape),
                                    X, y), silent = TRUE), "try-error"))
  stopifnot(inherits(try(nirs4all_torch_module(builder, name = "bad name"),
                         silent = TRUE), "try-error"))
}
