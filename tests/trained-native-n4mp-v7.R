library(nirs4all)

X <- outer(seq_len(24L), seq_len(17L), function(s, b)
  sin(s * b / 9) + cos(s + b / 7) + s * b / 100)
colnames(X) <- paste0("wl", seq_len(ncol(X)))
y <- 1.3 + .7 * X[, 2L] - .4 * X[, 6L]
held <- X[c(2L, 8L, 17L), , drop = FALSE] + .031
original_X <- X + 0
original_y <- y + 0
augmentations <- list(
  nirs4all_native_augmentation("gaussian_noise", .03, seed = 42),
  nirs4all_native_augmentation("multiplicative_noise", .02, seed = 7))
pipeline <- nirs4all_pipeline(list(nirs4all_snv(), nirs4all_msc(),
  nirs4all_savgol(7L, 2L, 1L)),
  nirs4all_n4m_method("ridge_pls", 2L), augmentations = augmentations)
fitted <- nirs4all_fit(pipeline, X, y, preprocessing = "native_n4mp")
expected <- predict(fitted, held)
augmented_X <- nirs4all:::nirs4all_augment_training(X, augmentations)
manual <- nirs4all_fit(nirs4all_pipeline(pipeline$steps, pipeline$learner),
  augmented_X, y, preprocessing = "native_n4mp")
stopifnot(max(abs(expected - predict(manual, held))) < 1e-12,
          identical(X, original_X), identical(y, original_y))

document_text <- nirs4all_export_trained_pipeline(fitted)
document <- jsonlite::fromJSON(document_text, simplifyVector = FALSE)
stopifnot(identical(document$schema, "nirs4all.n4m.trained_pipeline.v7"),
          identical(document$preprocessing$encoding, "base64-n4mp"),
          identical(document$model$encoding, "base64-n4mm"))
manifest <- jsonlite::fromJSON(document$manifest_json, simplifyVector = FALSE)
stopifnot(identical(manifest$recipe$pipeline[[1L]]$train_augmentation$class,
                    "n4m.NativeXAugmentation"),
          identical(manifest$recipe$pipeline[[1L]]$train_augmentation$params$seed,
                    42L),
          identical(nirs4all_parse_execution_plan(manifest$recipe)$augmentations,
                    augmentations))
imported <- nirs4all_import_trained_pipeline(document_text)
stopifnot(identical(imported$augmentations, augmentations),
          max(abs(predict(imported, held) - expected)) < 1e-12,
          max(abs(predict(nirs4all_retrain(imported, X, y), held) -
                  expected)) < 1e-8)
fresh_X <- X + .02
fresh_y <- y + .01
retrained <- nirs4all_retrain(imported, fresh_X, fresh_y)
fresh_manual <- nirs4all_fit(pipeline, fresh_X, fresh_y,
                            preprocessing = "native_n4mp")
stopifnot(max(abs(predict(retrained, held) -
                  predict(fresh_manual, held))) < 1e-12)
replay <- nirs4all_export_trained_pipeline(imported)
stopifnot(identical(jsonlite::fromJSON(replay, simplifyVector = FALSE)$schema,
                    document$schema),
          max(abs(predict(nirs4all_import_trained_pipeline(replay), held) -
                  expected)) < 1e-12)
reject <- function(expr) stopifnot(inherits(try(expr, silent = TRUE), "try-error"))
reject(predict(imported, held[, rev(seq_len(ncol(held))), drop = FALSE]))
reject(predict(imported, unname(held)))
reject(nirs4all_retrain(imported, unname(X), y))

emit <- function(value) as.character(jsonlite::toJSON(value, auto_unbox = TRUE,
  null = "null", digits = 17L))
with_manifest <- function(mutator, schema = document$schema) {
  doc <- document
  doc$schema <- schema
  changed <- mutator(jsonlite::fromJSON(doc$manifest_json,
                                        simplifyVector = FALSE))
  doc$manifest_json <- emit(changed)
  doc$manifest_sha256 <- digest::digest(doc$manifest_json, algo = "sha256",
                                        serialize = FALSE)
  emit(doc)
}
reject(nirs4all_import_trained_pipeline(with_manifest(identity,
  "nirs4all.n4m.trained_pipeline.v6")))
bad <- document
bad$manifest_sha256 <- paste0("0", substring(bad$manifest_sha256, 2L))
reject(nirs4all_import_trained_pipeline(emit(bad)))
bad <- document
bad$preprocessing$sha256 <- paste0("0", substring(bad$preprocessing$sha256, 2L))
reject(nirs4all_import_trained_pipeline(emit(bad)))
bad <- document
bad$model$sha256 <- paste0("0", substring(bad$model$sha256, 2L))
reject(nirs4all_import_trained_pipeline(emit(bad)))
bad <- document
reordered <- n4m::n4m_preprocess_fit(X, list(
  n4m::n4m_preprocess_step("msc"), n4m::n4m_preprocess_step("snv"),
  n4m::n4m_preprocess_step("savgol_derivative", c(7, 2, 1, 1))))
raw <- n4m::n4m_preprocess_export(reordered)
bad$preprocessing$payload <- jsonlite::base64_enc(raw)
bad$preprocessing$sha256 <- digest::digest(raw, algo = "sha256",
                                         serialize = FALSE)
reject(nirs4all_import_trained_pipeline(emit(bad)))
reject(nirs4all_import_trained_pipeline(with_manifest(function(x) {
  x$recipe$pipeline[[1L]]$train_augmentation$params$seed <- 2^53
  x
})))
reject(nirs4all_import_trained_pipeline(with_manifest(function(x) {
  x$recipe$pipeline[[1L]]$train_augmentation$params$values <- list()
  x
})))
reject(nirs4all_import_trained_pipeline(with_manifest(function(x) {
  x$recipe$pipeline[[1L]]$train_augmentation$params$kind <- "mixup"
  x
})))
reject(nirs4all_import_trained_pipeline(with_manifest(function(x) {
  x$recipe$pipeline[c(1L, 3L)] <- x$recipe$pipeline[c(3L, 1L)]
  x
})))
reject(nirs4all_import_trained_pipeline(with_manifest(function(x) {
  x$recipe$pipeline <- x$recipe$pipeline[-c(1L, 2L)]
  x
})))
reject(nirs4all_import_trained_pipeline(with_manifest(function(x) {
  x$recipe$extra <- TRUE
  x
})))
renamed <- nirs4all_import_trained_pipeline(with_manifest(function(x) {
  x$feature_names <- rev(x$feature_names)
  x
}))
reject(predict(renamed, held))
reject(nirs4all_import_trained_pipeline(with_manifest(function(x) {
  x$recipe$pipeline[[3L]]$params$ddof <- 1L
  x
})))
bad <- document
bad$unexpected <- TRUE
reject(nirs4all_import_trained_pipeline(emit(bad)))
bad <- c(document, list(schema = document$schema))
reject(nirs4all_import_trained_pipeline(emit(bad)))
bad <- document
bad$preprocessing <- c(bad$preprocessing, list(kind = "n4m_preprocessing"))
reject(nirs4all_import_trained_pipeline(emit(bad)))

# The older trained schemas cannot smuggle a newly accepted Level-1 node.
legacy <- nirs4all_fit(nirs4all_pipeline(pipeline$steps, pipeline$learner),
  X, y)
old <- jsonlite::fromJSON(nirs4all_export_trained_pipeline(legacy),
                          simplifyVector = FALSE)
old_manifest <- jsonlite::fromJSON(old$manifest_json, simplifyVector = FALSE)
old_manifest$recipe$pipeline <- c(manifest$recipe$pipeline[1:2],
                                  old_manifest$recipe$pipeline)
old$manifest_json <- emit(old_manifest)
old$manifest_sha256 <- digest::digest(old$manifest_json, algo = "sha256",
                                      serialize = FALSE)
reject(nirs4all_import_trained_pipeline(emit(old)))
reject(nirs4all_export_trained_pipeline(nirs4all_fit(pipeline, X, y)))
message("v7 native N4MP+N4MM augmentation predict/retrain/tamper passed")
