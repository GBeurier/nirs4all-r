#' Optional R torch multilayer-perceptron regression controller
#'
#' Fits a one-hidden-layer neural network with full-batch Adam and mean
#' squared error on CPU. The trained module is an R-torch artifact, not a
#' cross-language N4MM or ONNX model. Training uses an explicit torch seed.
#'
#' @param hidden Positive hidden-layer width.
#' @param epochs Positive number of full-batch optimization epochs.
#' @param learning_rate Positive Adam learning rate.
#' @param seed Non-negative torch seed.
#' @export
nirs4all_torch_mlp <- function(hidden = 32L, epochs = 100L,
                              learning_rate = 0.001, seed = 1L) {
  if (!requireNamespace("torch", quietly = TRUE) || !torch::torch_is_installed())
    stop("Install the optional 'torch' R package and its CPU runtime first",
         call. = FALSE)
  whole_positive <- function(x) is.numeric(x) && length(x) == 1L &&
    is.finite(x) && x >= 1 && x <= .Machine$integer.max && x == floor(x)
  if (!whole_positive(hidden)) stop("hidden must be a positive integer", call. = FALSE)
  if (!whole_positive(epochs)) stop("epochs must be a positive integer", call. = FALSE)
  if (!is.numeric(learning_rate) || length(learning_rate) != 1L ||
      !is.finite(learning_rate) || learning_rate <= 0)
    stop("learning_rate must be positive and finite", call. = FALSE)
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
      seed < 0 || seed > .Machine$integer.max || seed != floor(seed))
    stop("seed must be a non-negative integer", call. = FALSE)
  controller <- nirs4all_controller(
    fit = function(X, y) {
      x_center <- colMeans(X)
      x_scale <- apply(X, 2L, stats::sd)
      x_scale[!is.finite(x_scale) | x_scale <= .Machine$double.eps] <- 1
      y_center <- mean(y)
      y_scale <- stats::sd(y)
      if (!is.finite(y_scale) || y_scale <= .Machine$double.eps) y_scale <- 1
      normalized_x <- sweep(sweep(X, 2L, x_center, "-"), 2L, x_scale, "/")
      normalized_y <- (y - y_center) / y_scale
      torch::torch_manual_seed(as.integer(seed))
      module <- torch::nn_sequential(
        torch::nn_linear(ncol(X), as.integer(hidden)),
        torch::nn_relu(),
        torch::nn_linear(as.integer(hidden), 1L))
      optimizer <- torch::optim_adam(module$parameters, lr = learning_rate)
      input <- torch::torch_tensor(normalized_x, dtype = torch::torch_float())
      target <- torch::torch_tensor(matrix(normalized_y, ncol = 1L),
                                    dtype = torch::torch_float())
      module$train()
      for (epoch in seq_len(as.integer(epochs))) {
        optimizer$zero_grad()
        loss <- torch::nnf_mse_loss(module(input), target)
        if (!is.finite(as.numeric(loss$item())))
          stop("torch training loss became non-finite", call. = FALSE)
        loss$backward()
        optimizer$step()
      }
      module$eval()
      list(module = module, x_center = x_center, x_scale = x_scale,
           y_center = y_center, y_scale = y_scale)
    },
    predict = function(state, X) {
      normalized_x <- sweep(sweep(X, 2L, state$x_center, "-"),
                            2L, state$x_scale, "/")
      state$module$eval()
      normalized_predictions <- torch::with_no_grad({
        as.numeric(as.array(state$module(torch::torch_tensor(normalized_x,
                                               dtype = torch::torch_float()))))
      })
      normalized_predictions * state$y_scale + state$y_center
    }, name = "torch:mlp.regression")
  controller$format <- "torch-r"
  controller
}
