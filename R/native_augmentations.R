.nirs4all_native_augmentation_counts <- c(
  gaussian_noise = 1L, multiplicative_noise = 1L, spike_noise = 4L,
  hetero_noise = 2L, linear_drift = 4L, path_length = 2L,
  band_perturb = 7L, band_mask = 5L, channel_dropout = 2L,
  gauss_jitter = 3L, unsharp_mask = 4L, local_clip = 3L,
  rotate_translate = 2L, random_x_op = 3L, scatter_sim_msc = 4L,
  dead_band = 6L, batch_effect = 4L, spline_smoothing = 0L,
  spline_x_perturb = 4L, spline_y_perturb = 2L,
  spline_x_simplify = 2L, spline_curve_simplify = 2L)

#' Specify one seeded native training-only augmentation
#'
#' n4m applies the X-only numerical kernel; the R controller verifies that
#' row/feature identity survives. The positional `params` follow the native
#' n4m augmentation contract. Y-changing and wavelength-axis-dependent kinds
#' are unavailable because they lack a qualified paired-data contract.
#'
#' @param kind One of the 22 X-only kinds supported by
#'   `n4m::n4m_augmentation_apply()`.
#' @param params Finite positional parameter vector for this kind.
#' @param seed Exact nonnegative integer seed up to `2^53-1`.
#' @return A `nirs4all_native_augmentation` specification.
#' @export
nirs4all_native_augmentation <- function(kind, params = numeric(), seed = 0) {
  if (!is.character(kind) || length(kind) != 1L || is.na(kind) ||
      !(kind %in% names(.nirs4all_native_augmentation_counts)))
    stop("unsupported native X-only augmentation kind", call. = FALSE)
  if (!is.numeric(params) ||
      length(params) != .nirs4all_native_augmentation_counts[[kind]] ||
      anyNA(params) || any(!is.finite(params)))
    stop("invalid native augmentation parameter vector", call. = FALSE)
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
      !is.finite(seed) || seed < 0 || seed > 2^53 - 1 ||
      seed != floor(seed))
    stop("native augmentation seed must be an exact nonnegative integer",
         call. = FALSE)
  structure(list(kind = kind, params = unname(as.double(params)),
                 seed = as.double(seed)),
            class = "nirs4all_native_augmentation")
}

# Closed Level-1 wire node. The positional values and explicit seed are shared
# with n4m's C ABI; neither R defaults nor an R RNG state cross this boundary.
nirs4all_portable_augmentation_node <- function(spec) {
  verified <- nirs4all_native_augmentation(spec$kind, spec$params, spec$seed)
  list(train_augmentation = list(class = "n4m.NativeXAugmentation",
    params = list(kind = verified$kind, values = as.list(verified$params),
                  seed = verified$seed)))
}

nirs4all_portable_augmentation_spec <- function(node) {
  if (!is.list(node) || !identical(names(node), "train_augmentation"))
    stop("train augmentation must be a closed pipeline node", call. = FALSE)
  value <- node$train_augmentation
  if (!is.list(value) || anyDuplicated(names(value)) ||
      !setequal(names(value), c("class", "params")) ||
      !identical(value$class, "n4m.NativeXAugmentation"))
    stop("unsupported train augmentation alias or field", call. = FALSE)
  params <- value$params
  if (!is.list(params) || anyDuplicated(names(params)) ||
      !setequal(names(params), c("kind", "values", "seed")))
    stop("train augmentation requires kind, values and seed", call. = FALSE)
  values <- params$values
  if (!is.list(values) || !is.null(names(values)) ||
      !all(vapply(values, function(item)
        is.numeric(item) && !is.logical(item) && length(item) == 1L &&
          !is.na(item) && is.finite(item), logical(1))))
    stop("augmentation values must be a positional numeric array", call. = FALSE)
  values <- if (length(values)) unlist(values, use.names = FALSE) else numeric()
  if (!is.numeric(params$seed) || is.logical(params$seed))
    stop("augmentation seed must be an exact numeric integer", call. = FALSE)
  nirs4all_native_augmentation(params$kind, values, params$seed)
}

nirs4all_augment_training <- function(X, augmentations) {
  if (!length(augmentations)) return(X)
  if (!("n4m_augmentation_apply" %in% getNamespaceExports("n4m")))
    stop("n4m native augmentation ABI is unavailable", call. = FALSE)
  for (spec in augmentations) {
    if (!inherits(spec, "nirs4all_native_augmentation"))
      stop("invalid training augmentation specification", call. = FALSE)
    # Revalidate mutable R lists rather than trusting the constructor class.
    verified <- nirs4all_native_augmentation(spec$kind, spec$params, spec$seed)
    next_X <- n4m::n4m_augmentation_apply(verified$kind, X,
                                           verified$params, verified$seed)
    if (!is.matrix(next_X) || !is.numeric(next_X) ||
        !identical(dim(next_X), dim(X)) || anyNA(next_X) ||
        any(!is.finite(next_X)))
      stop("native augmentation changed training shape or finiteness",
           call. = FALSE)
    dimnames(next_X) <- dimnames(X)
    X <- next_X
  }
  X
}
