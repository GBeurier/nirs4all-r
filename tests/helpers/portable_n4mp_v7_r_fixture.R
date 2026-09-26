# Deterministic R-trained v7 envelope and independent train/predict/retrain
# oracle for Python. Usage: Rscript ... envelope.json oracle.json
library(nirs4all)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("expected envelope and oracle paths")
X <- outer(seq_len(24L), seq_len(17L), function(s, b)
  sin(s * b / 9) + cos(s + b / 7) + s * b / 100)
colnames(X) <- paste0("wl", seq_len(ncol(X)))
y <- 1.3 + .7 * X[, 2L] - .4 * X[, 6L]
held <- X[c(2L, 8L, 17L), , drop = FALSE] + .031
augmentations <- list(
  nirs4all_native_augmentation("gaussian_noise", .03, seed = 42),
  nirs4all_native_augmentation("multiplicative_noise", .02, seed = 7))
pipeline <- nirs4all_pipeline(list(nirs4all_snv(), nirs4all_msc(),
  nirs4all_savgol(7L, 2L, 1L)),
  nirs4all_n4m_method("ridge_pls", 2L), augmentations = augmentations)
fitted <- nirs4all_fit(pipeline, X, y, preprocessing = "native_n4mp")
retrain_X <- X + .02
retrain_y <- y + .01
retrained <- nirs4all_retrain(fitted, retrain_X, retrain_y)
rows <- function(value) lapply(seq_len(nrow(value)), function(index)
  unname(as.list(as.numeric(value[index, ]))))
nirs4all_export_trained_pipeline(fitted, args[[1L]])
oracle <- list(train = rows(X), heldout = rows(held),
  feature_names = unname(as.list(colnames(X))), y = unname(as.list(y)),
  augmented_train = rows(nirs4all:::nirs4all_augment_training(X,
    augmentations)),
  predictions = unname(as.list(as.numeric(predict(fitted, held)))),
  retrain_train = rows(retrain_X), retrain_y = unname(as.list(retrain_y)),
  retrain_predictions = unname(as.list(as.numeric(predict(retrained, held)))))
writeLines(as.character(jsonlite::toJSON(oracle, auto_unbox = TRUE,
  digits = 17L)), args[[2L]], useBytes = TRUE)
