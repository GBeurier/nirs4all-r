library(nirs4all)

X <- outer(seq_len(30L), seq_len(8L),
           function(i, j) sin(i * j / 11) + cos((i + j) / 7) + i * j / 100)
rownames(X) <- sprintf("sample:%04d", seq_len(nrow(X)))
colnames(X) <- sprintf("band:%03d", seq_len(ncol(X)))
y <- 1.2 + .7 * X[, 2L] - .3 * X[, 6L]
names(y) <- rownames(X)
original_X <- X + 0
original_y <- y + 0

cases <- list(
  gaussian_noise = c(.03), multiplicative_noise = c(.03),
  spike_noise = c(1, 2, .1, .2), hetero_noise = c(.01, .02),
  linear_drift = c(.01, .02, .001, .002), path_length = c(.05, .5),
  band_perturb = c(1, 2, 4, .9, 1.1, -.01, .01),
  band_mask = c(1, 1, 2, 4, 0), channel_dropout = c(.1, 0),
  gauss_jitter = c(.5, 1, 5), unsharp_mask = c(.1, .2, 1, 5),
  local_clip = c(1, 2, 4), rotate_translate = c(.05, .05),
  random_x_op = c(0, .9, 1.1),
  scatter_sim_msc = c(.9, 1.1, -.01, .01),
  dead_band = c(1, 2, 4, .01, .5, 0),
  batch_effect = c(.01, .01, .01, 0), spline_smoothing = numeric(),
  spline_x_perturb = c(3, .2, -.1, .1),
  spline_y_perturb = c(4, .1), spline_x_simplify = c(8, 1),
  spline_curve_simplify = c(8, 1)
)
for (kind in names(cases)) {
  spec <- nirs4all_native_augmentation(kind, cases[[kind]], seed = 42)
  first <- nirs4all:::nirs4all_augment_training(X, list(spec))
  second <- nirs4all:::nirs4all_augment_training(X, list(spec))
  stopifnot(identical(first, second), identical(dim(first), dim(X)),
            identical(dimnames(first), dimnames(X)), all(is.finite(first)))
  for (format in c("json", "yaml")) {
    recipe <- nirs4all_export_pipeline(nirs4all_pipeline(
      learner = nirs4all_pls(2L), augmentations = list(spec)), format)
    imported <- nirs4all_pipeline_from_portable(recipe)
    stopifnot(identical(imported$augmentations, list(spec)))
  }
}
stopifnot(identical(X, original_X), identical(y, original_y),
          inherits(try(nirs4all_native_augmentation("mixup", .5),
                       silent = TRUE), "try-error"),
          inherits(try(nirs4all_native_augmentation("gaussian_noise"),
                       silent = TRUE), "try-error"),
          inherits(try(nirs4all_native_augmentation("gaussian_noise", .03,
                                                    seed = 2^53),
                       silent = TRUE), "try-error"))

gaussian <- nirs4all_native_augmentation("gaussian_noise", .03, seed = 42)
augmented <- nirs4all:::nirs4all_augment_training(X, list(gaussian))
python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON", "")
if (nzchar(python)) {
  helper <- if (file.exists("helpers/native_augmentation_peer.py"))
    "helpers/native_augmentation_peer.py" else
    "tests/helpers/native_augmentation_peer.py"
  output <- suppressWarnings(system2(python, shQuote(helper),
                                      stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L)
    stop("Python augmentation peer failed: ", paste(output, collapse = "\n"))
  peer <- jsonlite::fromJSON(paste(output, collapse = "\n"))
  stopifnot(identical(dim(peer), dim(augmented)),
            max(abs(peer - augmented)) < 1e-12)
}

pipeline <- nirs4all_pipeline(learner = nirs4all_pls(2L),
                             augmentations = list(gaussian))
fitted <- nirs4all_fit(pipeline, X, y)
manual <- nirs4all_fit(nirs4all_pipeline(learner = nirs4all_pls(2L)),
                      augmented, y)
legacy <- nirs4all_fit(nirs4all_pipeline(learner = nirs4all_pls(2L)), X, y)
legacy$augmentations <- NULL
legacy_retrained <- nirs4all_retrain(legacy, X, y)
stopifnot(max(abs(predict(legacy_retrained, X) - predict(legacy, X))) < 1e-12)
heldout <- X[c(2L, 12L, 25L), , drop = FALSE] + .031
stopifnot(max(abs(predict(fitted, heldout) - predict(manual, heldout))) < 1e-12,
          identical(fitted$augmentations, pipeline$augmentations),
          identical(X, original_X), identical(y, original_y))
refitted <- nirs4all_retrain(fitted, X + .02, y)
manual_refit <- nirs4all_fit(nirs4all_pipeline(learner = nirs4all_pls(2L)),
  nirs4all:::nirs4all_augment_training(X + .02, list(gaussian)), y)
stopifnot(max(abs(predict(refitted, heldout) -
                  predict(manual_refit, heldout))) < 1e-12)
path <- tempfile(fileext = ".rds")
nirs4all_save(fitted, path)
loaded <- nirs4all_load(path)
stopifnot(identical(loaded$augmentations, list(gaussian)),
          max(abs(predict(loaded, heldout) - predict(fitted, heldout))) < 1e-12)
unlink(path)
for (format in c("json", "yaml")) {
  recipe <- nirs4all_export_pipeline(pipeline, format)
  imported <- nirs4all_pipeline_from_portable(recipe)
  stopifnot(identical(imported$augmentations, list(gaussian)),
            max(abs(predict(nirs4all_fit(imported, X, y), heldout) -
                    predict(fitted, heldout))) < 1e-12)
}
stopifnot(inherits(try(nirs4all_export_trained_pipeline(fitted), silent = TRUE),
                   "try-error"))
message("22 native X-only augmentations and train-only fit/retrain/predict passed")
