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
  if (!is.numeric(hidden) || length(hidden) != 1L ||
      !is.finite(hidden) || hidden < 1L ||
      hidden > .Machine$integer.max || hidden != floor(hidden))
    stop("hidden must be a positive integer", call. = FALSE)
  builder <- function(n_features) torch::nn_sequential(
    torch::nn_linear(n_features, as.integer(hidden)),
    torch::nn_relu(),
    torch::nn_linear(as.integer(hidden), 1L))
  nirs4all_torch_cpu_regressor(builder, "mlp.regression", epochs,
    learning_rate, seed, list(learner = "torch_mlp", hidden = as.integer(hidden),
      epochs = as.integer(epochs), learning_rate = learning_rate,
      seed = as.integer(seed)))
}

#' Optional R torch module regression controller
#'
#' The builder receives the number of input features and must return a fresh
#' CPU-compatible `torch` `nn_module` whose forward pass maps an `N x p` float
#' tensor to an `N x 1` float tensor. Each DAG-ML fold invokes the builder anew,
#' with independent normalization and optimizer state. Only trusted,
#' self-contained R builder functions should be used: they are serialized into
#' the R-specific process-adapter specification and are not portable Python
#' weights, ONNX, or N4MM artifacts.
#' @param builder Function accepting one integer `n_features` and returning an
#'   untrained `torch` `nn_module`.
#' @param name Stable model label recorded in the controller specification.
#' @param epochs Positive number of full-batch Adam epochs.
#' @param learning_rate Positive Adam learning rate.
#' @param seed Non-negative torch seed.
#' @export
nirs4all_torch_module <- function(builder, name = "custom", epochs = 100L,
                                  learning_rate = 0.001, seed = 1L) {
  if (!is.function(builder)) stop("builder must be a function", call. = FALSE)
  if (!is.character(name) || length(name) != 1L || is.na(name) ||
      !grepl("^[A-Za-z][A-Za-z0-9_.-]*$", name))
    stop("name must be a stable non-empty identifier", call. = FALSE)
  nirs4all_torch_cpu_regressor(builder, name, epochs, learning_rate, seed,
    list(learner = "torch_module", name = name,
         epochs = epochs, learning_rate = learning_rate,
         seed = seed), model_spec = builder)
}

nirs4all_torch_cpu_regressor <- function(builder, name, epochs,
                                         learning_rate, seed, spec,
                                         model_spec = NULL) {
  if (!requireNamespace("torch", quietly = TRUE) || !torch::torch_is_installed())
    stop("Install the optional 'torch' R package and its CPU runtime first",
         call. = FALSE)
  whole_positive <- function(x) is.numeric(x) && length(x) == 1L &&
    is.finite(x) && x >= 1 && x <= .Machine$integer.max && x == floor(x)
  if (!whole_positive(epochs)) stop("epochs must be a positive integer", call. = FALSE)
  if (!is.numeric(learning_rate) || length(learning_rate) != 1L ||
      !is.finite(learning_rate) || learning_rate <= 0)
    stop("learning_rate must be positive and finite", call. = FALSE)
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
      seed < 0 || seed > .Machine$integer.max || seed != floor(seed))
    stop("seed must be a non-negative integer", call. = FALSE)
  spec$epochs <- as.integer(epochs)
  spec$seed <- as.integer(seed)
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
      module <- builder(as.integer(ncol(X)))
      if (!inherits(module, "nn_module"))
        stop("torch builder must return an nn_module", call. = FALSE)
      module$to(device = torch::torch_device("cpu"))
      optimizer <- torch::optim_adam(module$parameters, lr = learning_rate)
      input <- torch::torch_tensor(normalized_x, dtype = torch::torch_float())
      target <- torch::torch_tensor(matrix(normalized_y, ncol = 1L),
                                    dtype = torch::torch_float())
      module$train()
      for (epoch in seq_len(as.integer(epochs))) {
        optimizer$zero_grad()
        output <- module(input)
        if (!inherits(output, "torch_tensor") ||
            !identical(as.integer(output$size()), c(nrow(X), 1L)))
          stop("torch module must emit one value per training row", call. = FALSE)
        loss <- torch::nnf_mse_loss(output, target)
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
        output <- state$module(torch::torch_tensor(normalized_x,
                                                  dtype = torch::torch_float()))
        if (!inherits(output, "torch_tensor") ||
            !identical(as.integer(output$size()), c(nrow(X), 1L)))
          stop("torch module must emit one value per prediction row", call. = FALSE)
        as.numeric(as.array(output))
      })
      normalized_predictions * state$y_scale + state$y_center
    }, name = paste0("torch:", name))
  controller$format <- "torch-r"
  controller$spec <- spec
  if (!is.null(model_spec)) controller$model_spec <- model_spec
  controller
}
