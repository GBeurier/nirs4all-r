# Owner-executed standalone regression train augmentation through the real Core
# operation `portable-pipeline-run` (profile `portable_feature_regression.v1`,
# owner profile `feature_graph.v1`). Linux/R only.
#
# Prerequisites (otherwise an explicit NOT RUN line, or a failure when
# NIRS4ALL_REQUIRE_CORE_PORTABLE=1):
#   NIRS4ALL_CORE_CLI  nirs4all-core-archive providing the graph augmentation route
#   N4M_LIB_PATH       absolute path of the Methods libn4m used by the owner
#
# One augmenter, `band_perturb`, with the seven-position non-default values of
# native-augmentations.R and a seed above 2^31: its positions and the exact seed
# must reach the owner untouched. References are the current local R
# fit/predict of the same pipeline (`nirs4all_native_augmentation` applied to the
# training rows only, MSC learned from the augmented rows, predictions on raw X).
# Owner-vs-R equality is same-kernel interoperability, not an independent oracle.
library(nirs4all)

cli <- Sys.getenv("NIRS4ALL_CORE_CLI")
library_path <- Sys.getenv("N4M_LIB_PATH")
if (!nzchar(cli) || !file.exists(cli) || !nzchar(library_path) || !file.exists(library_path)) {
  if (identical(Sys.getenv("NIRS4ALL_REQUIRE_CORE_PORTABLE"), "1"))
    stop("portable-owner-augmentation.R requires NIRS4ALL_CORE_CLI and N4M_LIB_PATH")
  cat("NOT RUN: portable-owner-augmentation.R (set NIRS4ALL_CORE_CLI and N4M_LIB_PATH)\n")
  quit(save = "no", status = 0L)
}
run <- function(definition, dataset)
  nirs4all_run_portable_pipeline(definition, dataset, core_cli = cli,
                                 methods_library = library_path)

X <- outer(seq_len(30L), seq_len(12L), function(i, j)
  sin(i * j / 9) + cos((i + j) / 7) + i * j / 100)
y <- 1.3 + .7 * X[, 2L] - .4 * X[, 6L]
data <- list(X = X, y = y)
values <- c(1, 2, 4, .9, 1.1, -.01, .01)      # n_bands, bw_lo/hi, gain_lo/hi, offset_lo/hi
seed <- 4294967299                              # above 2^31 and 2^32, exact in a double
spec <- function(seed)
  nirs4all_native_augmentation("band_perturb", values, seed = seed)
make <- function(...) nirs4all_pipeline(list(nirs4all_msc()), nirs4all_pls(2L), ...)
augmented <- make(augmentations = list(spec(seed)))
truncated <- make(augmentations = list(spec(3)))   # seed - 2^32: what an int32 cast would give
plain <- make()

recipe <- jsonlite::fromJSON(nirs4all_export_pipeline(augmented), simplifyVector = FALSE)
holdout <- recipe
holdout$pipeline <- c(list(list(class = "n4m.KennardStone",
                                params = list(test_size = 0.25))), recipe$pipeline)

# Declared dispatch: standalone regression augmentation is owner-executed.
plan <- nirs4all_parse_execution_plan(recipe, core_cli = cli)
stopifnot(identical(plan$profile$route, "owner"), !length(plan$profile$residual),
          identical(plan$profile$name, "portable_feature_regression.v1"),
          identical(plan$admission, "owner"),
          identical(plan$owner$profile, "portable_feature_regression.v1"),
          identical(plan$owner$owner_profile, "feature_graph.v1"))

# The test can tell an exact seed from an int32-truncated one, and the augmentation
# from none: otherwise the comparisons below would prove nothing.
everything <- function(pipeline) predict(nirs4all_fit(pipeline, X, y), X)
reference <- everything(augmented)
stopifnot(max(abs(reference - everything(truncated))) > 1e-6,
          max(abs(reference - everything(plain))) > 1e-6)

check_common <- function(result, bank) {
  stopifnot(identical(result$profile$route, "owner"),
            identical(result$profile$name, "portable_feature_regression.v1"),
            identical(result$profile$owner_profile, "feature_graph.v1"),
            identical(result$owner$profile, bank),
            !"selected_package" %in% names(result$owner$evidence),
            length(result$variants) == 1L, identical(result$selected$n_components, 2L),
            length(result$selected$feature_blocks) == 0L,   # no branch, no join widths
            identical(result$evaluation$independent_test, FALSE))
}

# 1. Training scope: augmented fit rows, raw-row scoring (predictions on raw X after
# the preprocessing learned from the augmented rows), as the local runner does.
trained <- run(recipe, data)
check_common(trained, "training_graph_bank.v1")
stopifnot(identical(trained$mode, "training"), identical(trained$split$kind, "all"),
          identical(trained$evaluation$scope, "training"),
          max(abs(trained$targets - y)) < 1e-12,
          max(abs(trained$selected$predictions - reference)) < 1e-10,
          abs(trained$selected$rmse - sqrt(mean((trained$selected$predictions - y)^2))) < 1e-10)

# 2. Raw-X Kennard-Stone holdout: owner split order, augmentation of the training
# rows only, validation rows raw.
split <- n4m::kennard_stone_split(X, test_size = 0.25, zero_based = TRUE)
train <- split$train + 1L
test <- split$test + 1L
held <- run(holdout, data)
check_common(held, "single_resampled_holdout_graph_bank.v1")
stopifnot(identical(held$mode, "single_holdout"), identical(held$split$kind, "KennardStone"),
          identical(held$split$trainIndices, as.integer(split$train)),
          identical(held$split$testIndices, as.integer(split$test)),
          identical(held$evaluation$scope, "selection_validation"),
          max(abs(held$targets - y[test])) < 1e-12,
          max(abs(held$selected$predictions -
                  predict(nirs4all_fit(augmented, X[train, , drop = FALSE], y[train]),
                          X[test, , drop = FALSE]))) < 1e-10,
          abs(held$selected$rmse - sqrt(mean((held$selected$predictions - y[test])^2))) < 1e-10)
cat("portable-owner-augmentation: ok (same-kernel interoperability, not an independent oracle)\n")
