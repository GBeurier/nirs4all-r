library(nirs4all)

X <- outer(seq_len(30L), seq_len(12L), function(i, j)
  sin(i * j / 9) + cos((i + j) / 7) + i * j / 100)
rownames(X) <- sprintf("id-%03d", seq_len(nrow(X)))
colnames(X) <- sprintf("band-%02d", seq_len(ncol(X)))
y <- 1.3 + .7 * X[, 2L] - .4 * X[, 6L]
names(y) <- rownames(X)
original_X <- X + 0
original_y <- y + 0
augmentations <- list(
  nirs4all_native_augmentation("gaussian_noise", .03, seed = 42),
  nirs4all_native_augmentation("spline_smoothing", numeric(), seed = 7))
pipeline <- nirs4all_pipeline(list(nirs4all_snv()), nirs4all_pls(2L),
                             augmentations = augmentations)
heldout <- X[c(2L, 8L, 17L), , drop = FALSE] + .031
expected <- predict(nirs4all_fit(pipeline, X, y), heldout)

for (format in c("json", "yaml")) {
  recipe <- nirs4all_export_pipeline(pipeline, format)
  definition <- nirs4all_load_pipeline(recipe)
  stopifnot(identical(names(definition$pipeline[[1L]]), "train_augmentation"),
            identical(definition$pipeline[[1L]]$train_augmentation$class,
                      "n4m.NativeXAugmentation"),
            identical(definition$pipeline[[2L]]$train_augmentation$params$values,
                      list()))
  plan <- nirs4all_parse_execution_plan(definition)
  stopifnot(identical(plan$augmentations, augmentations),
            length(plan$preprocessing) == 1L)
  imported <- nirs4all_pipeline_from_portable(recipe)
  stopifnot(identical(imported$augmentations, augmentations),
            max(abs(predict(nirs4all_fit(imported, X, y), heldout) - expected)) <
              1e-12)
  refitted <- nirs4all_retrain(nirs4all_fit(imported, X, y), X + .02, y)
  manual_refit <- nirs4all_fit(pipeline, X + .02, y)
  stopifnot(max(abs(predict(refitted, heldout) -
                    predict(manual_refit, heldout))) < 1e-12)
}
stopifnot(identical(X, original_X), identical(y, original_y))
precise <- nirs4all_native_augmentation("gaussian_noise",
  .12345678901234568, seed = 2^53 - 1)
for (format in c("json", "yaml")) {
  precise_recipe <- nirs4all_export_pipeline(nirs4all_pipeline(
    learner = nirs4all_pls(2L), augmentations = list(precise)), format)
  stopifnot(identical(nirs4all_pipeline_from_portable(precise_recipe)$augmentations,
                      list(precise)))
}

# The splitter acts on raw X. Only the training partition receives augmentation.
recipe <- nirs4all_load_pipeline(nirs4all_export_pipeline(pipeline))
recipe$pipeline <- c(list(list(class = "n4m.KennardStone",
                               params = list(test_size = .3))), recipe$pipeline)
split <- n4m::kennard_stone_split(X, test_size = .3, zero_based = TRUE)
train <- split$train + 1L
valid <- split$test + 1L
manual <- nirs4all_fit(pipeline, X[train, , drop = FALSE], y[train])
actual <- nirs4all_run_portable_pipeline(recipe, list(X = X, y = y))
stopifnot(identical(actual$split$trainIndices, as.integer(split$train)),
          identical(actual$split$testIndices, as.integer(split$test)),
          max(abs(actual$selected$predictions -
                  predict(manual, X[valid, , drop = FALSE]))) < 1e-12)

reject <- function(definition) stopifnot(inherits(
  try(nirs4all_parse_execution_plan(definition), silent = TRUE), "try-error"))
base <- nirs4all_load_pipeline(nirs4all_export_pipeline(pipeline))
bad <- base
bad$pipeline[[1L]]$extra <- TRUE
reject(bad)
bad <- base
bad$pipeline[[1L]]$train_augmentation$class <- "n4m.Mixup"
reject(bad)
bad <- base
bad$pipeline[[1L]]$train_augmentation$params$unknown <- 1
reject(bad)
bad <- base
bad$pipeline[[1L]]$train_augmentation$params$seed <- NULL
reject(bad)
bad <- base
bad$pipeline[[1L]]$train_augmentation$params$seed <- 2^53
reject(bad)
bad <- base
bad$pipeline[[1L]]$train_augmentation$params$values <- list(rate = .03)
reject(bad)
bad <- base
bad$pipeline[[1L]]$train_augmentation$params$values <- .03
reject(bad)
bad_yaml <- sub("values:\n      - 0.03", "values: 0.03",
                nirs4all_export_pipeline(pipeline, "yaml"), fixed = TRUE)
reject(bad_yaml)
bad <- base
bad$pipeline[[1L]]$train_augmentation$params$kind <- "mixup"
reject(bad)
bad <- base
bad$pipeline <- bad$pipeline[c(3L, 1L, 2L, 4L)]
reject(bad)
bad <- recipe
bad$pipeline <- bad$pipeline[c(2L, 1L, 3L, 4L, 5L)]
reject(bad)
bad <- base
bad$pipeline[[1L]]$train_augmentation$params$values <- list(Inf)
reject(bad)
bad <- base
bad$pipeline[[1L]]$train_augmentation$params$values <- list(TRUE)
reject(bad)
bad <- base
bad$pipeline[[1L]]$train_augmentation$params$seed <- TRUE
reject(bad)
bad <- base
bad$pipeline[[1L]]$train_augmentation$params$values <- list(.03, .04)
reject(bad)
bad <- base
bad$pipeline[[1L]]$train_augmentation$params$kind <- "gaussian_noise"
bad$pipeline[[1L]]$train_augmentation$params$values <- list()
reject(bad)
bad <- base
bad$pipeline[[1L]]$train_augmentation$params$kind <- "band_perturb"
bad$pipeline[[1L]]$train_augmentation$params$values <- list(.03)
reject(bad)

# A branch or local-model recipe cannot reinterpret a train-only node.
bad <- base
bad$pipeline <- list(list(branch = list(left = list(base$pipeline[[1L]]),
                                             right = list(list(class = "n4m.SNV")))),
                     list(merge = "features"),
                     tail(base$pipeline, 1L)[[1L]])
reject(bad)
bad <- base
bad$pipeline <- c(base$pipeline[1:2], list(
  list(branch = list(left = list(list(class = "n4m.SNV")),
                     right = list(list(class = "n4m.SNV")))),
  list(merge = "features")), tail(base$pipeline, 1L))
reject(bad)
branch_pipeline <- nirs4all_pipeline(list(nirs4all_concat(list(
  left = list(nirs4all_snv()), right = list(nirs4all_snv())))),
  nirs4all_pls(2L), augmentations = augmentations)
stopifnot(inherits(try(nirs4all_export_pipeline(branch_pipeline),
                       silent = TRUE), "try-error"))
selector <- nirs4all_n4m_selector("wvc_threshold_select", 4L, list())
selector_pipeline <- nirs4all_pipeline(list(selector), nirs4all_pls(2L),
                                      augmentations = augmentations)
stopifnot(inherits(try(nirs4all_export_pipeline(selector_pipeline),
                       silent = TRUE), "try-error"))
selector_recipe <- nirs4all_load_pipeline(nirs4all_export_pipeline(
  nirs4all_pipeline(list(selector), nirs4all_pls(2L))))
selector_recipe$pipeline <- c(base$pipeline[1:2], selector_recipe$pipeline)
reject(selector_recipe)
local <- base
local$pipeline[[length(local$pipeline)]] <- list(model = list(
  class = "r.ranger.regression", params = list(num_trees = 10L)))
stopifnot(inherits(try(nirs4all_r_pipeline_from_recipe(local), silent = TRUE),
                   "try-error"),
          inherits(try(nirs4all_export_pipeline(pipeline, scope = "r_native"),
                       silent = TRUE), "try-error"),
          inherits(try(nirs4all_export_trained_pipeline(
            nirs4all_fit(pipeline, X, y)), silent = TRUE), "try-error"))
message("closed Level-1 train augmentation JSON/YAML phase checks passed")
