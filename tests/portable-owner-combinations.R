# Owner-executed combined graph profiles through the real Core operation
# `portable-pipeline-run`: a classifier with a feature branch or training
# augmentation (`portable_feature_classification.v1`), and a feature branch with
# MBPLS block_sizes or GroupSparsePLS group_assignment
# (`portable_feature_regression.v1`). Owner profile `feature_graph.v1`. Linux/R only.
# Scope is limited to the named Linux/native profiles, not universal support.
#
# Prerequisites (otherwise an explicit NOT RUN line, or a failure when
# NIRS4ALL_REQUIRE_CORE_PORTABLE=1):
#   NIRS4ALL_CORE_CLI  nirs4all-core-archive providing the combined graph routes
#   N4M_LIB_PATH       absolute path of the Methods libn4m used by the owner
#
# Four cases, each in training scope and in a raw-X Kennard-Stone holdout, one fixed
# component count (2), no search. Every valid manual `nirs4all_fit` baseline is
# executed BEFORE the first owner call. Owner-vs-R equality below is SAME-KERNEL
# interoperability, not an independent oracle, for all four combinations
# (GroupSparsePLS is guarded same-kernel; only the standalone frozen MBPLS
# reference in the existing tests is independent). Branch features join 6 + 6 = 12
# columns; MBPLS uses explicit blocks 4/4/4 and GroupSparsePLS an explicit
# positional length-12 group vector over the JOINED ordered axis.
library(nirs4all)

cli <- Sys.getenv("NIRS4ALL_CORE_CLI")
library_path <- Sys.getenv("N4M_LIB_PATH")
if (!nzchar(cli) || !file.exists(cli) || !nzchar(library_path) || !file.exists(library_path)) {
  if (identical(Sys.getenv("NIRS4ALL_REQUIRE_CORE_PORTABLE"), "1"))
    stop("portable-owner-combinations.R requires NIRS4ALL_CORE_CLI and N4M_LIB_PATH")
  cat("NOT RUN: portable-owner-combinations.R (set NIRS4ALL_CORE_CLI and N4M_LIB_PATH)\n")
  quit(save = "no", status = 0L)
}
run <- function(definition, dataset)
  nirs4all_run_portable_pipeline(definition, dataset, core_cli = cli,
                                 methods_library = library_path)

# Deterministic public data: 36 rows x 6 columns, 3 balanced classes (12 each).
n <- 36L
cls <- rep(1:3, length.out = n)
X <- outer(seq_len(n), seq_len(6L), function(i, j) sin(i * j / 11) + cos(i / 5 + j / 3) / 2) +
  0.6 * outer(c(0, 1, -1)[cls], cos(seq_len(6L) / 2))
levels_unsorted <- c("gamma", "alpha", "beta")          # not alphabetical
labels <- factor(levels_unsorted[cls], levels = levels_unsorted)
y_regression <- 1 + X[, 2L] - 0.5 * X[, 5L] + 0.1 * sin(seq_len(n))

split <- n4m::kennard_stone_split(X, test_size = 0.25, zero_based = TRUE)
train <- split$train + 1L
test <- split$test + 1L
stopifnot(length(test) == 9L, setequal(levels(droplevels(labels[train])), levels_unsorted))

branches <- function() nirs4all_concat(list(
  derivative = list(nirs4all_snv()), scatter = list(nirs4all_msc())))
values <- c(1, 2, 4, .9, 1.1, -.01, .01)                  # accepted band_perturb fixture
seed <- 4294967299                                        # above 2^31 and 2^32
augmentation <- nirs4all_native_augmentation("band_perturb", values, seed = seed)
groups <- rep(1:4, each = 3L)                              # positional, length 12

cases <- list(
  branch_classifier = list(classification = TRUE, branch = TRUE,
    pipeline = nirs4all_pipeline(list(branches()), nirs4all_sparse_pls_da(2L, 0.05))),
  augmentation_classifier = list(classification = TRUE, branch = FALSE,
    pipeline = nirs4all_pipeline(list(nirs4all_msc()), nirs4all_sparse_pls_da(2L, 0.05),
                                 augmentations = list(augmentation))),
  branch_mbpls = list(classification = FALSE, branch = TRUE,
    pipeline = nirs4all_pipeline(list(branches()), nirs4all_n4m_method("mb_pls", 2L,
      list(block_sizes = c(4L, 4L, 4L))))),
  branch_group_sparse = list(classification = FALSE, branch = TRUE,
    pipeline = nirs4all_pipeline(list(branches()),
      nirs4all_group_sparse_pls(2L, groups, 0.2))))

# Manual baselines FIRST (both scopes, every case), before any owner call. Training
# scope: all rows fitted, scored on raw X; holdout: fit rows only, raw validation X.
baseline <- lapply(cases, function(case) {
  y <- if (case$classification) labels else y_regression
  fit <- function(rows) nirs4all_fit(case$pipeline, X[rows, , drop = FALSE], y[rows])
  shape <- function(value) if (case$classification) as.character(value) else as.numeric(value)
  list(training = shape(predict(fit(seq_len(n)), X)),
       holdout = shape(predict(fit(train), X[test, , drop = FALSE])))
})

recipe_of <- function(case, holdout) {
  recipe <- jsonlite::fromJSON(nirs4all_export_pipeline(case$pipeline), simplifyVector = FALSE)
  if (holdout) recipe$pipeline <- c(list(list(class = "n4m.KennardStone",
    params = list(test_size = 0.25))), recipe$pipeline)
  recipe
}

classification_profile <- "portable_feature_classification.v1"
regression_profile <- "portable_feature_regression.v1"
for (name in names(cases)) {
  case <- cases[[name]]
  profile <- if (case$classification) classification_profile else regression_profile
  y <- if (case$classification) labels else y_regression
  data <- list(X = X, y = y)
  banks <- if (case$classification)
    c(training = "training_graph_classification_bank.v1",
      holdout = "single_resampled_holdout_graph_classification_bank.v1") else
    c(training = "training_graph_bank.v1",
      holdout = "single_resampled_holdout_graph_bank.v1")

  # Declared dispatch and the no-fit plan: owner admission, explicit profile.
  plan <- nirs4all_parse_execution_plan(recipe_of(case, TRUE), core_cli = cli)
  stopifnot(identical(plan$profile$name, profile), identical(plan$admission, "owner"),
            identical(plan$owner$profile, profile),
            identical(plan$owner$owner_profile, "feature_graph.v1"))

  for (scope in c("training", "holdout")) {
    held <- identical(scope, "holdout")
    result <- run(recipe_of(case, held), data)
    rows <- if (held) test else seq_len(n)
    expected <- baseline[[name]][[scope]]
    stopifnot(identical(result$profile$name, profile),
              identical(result$profile$owner_profile, "feature_graph.v1"),
              identical(result$owner$profile, unname(banks[[scope]])),
              !"selected_package" %in% names(result$owner$evidence),
              identical(result$mode, if (held) "single_holdout" else "training"),
              identical(result$split$kind, if (held) "KennardStone" else "all"),
              identical(result$evaluation, list(
                scope = if (held) "selection_validation" else "training",
                independent_test = FALSE)),
              length(result$variants) == 1L, identical(result$selected$n_components, 2L))
    if (held)   # raw-X Kennard-Stone order, owner split
      stopifnot(identical(result$split$trainIndices, as.integer(split$train)),
                identical(result$split$testIndices, as.integer(split$test)))

    if (case$classification) {
      stopifnot(identical(result$class_values, levels_unsorted),
                identical(result$targets, as.character(y[rows])),
                identical(result$selected$predictions, expected),   # same-kernel, labels
                is.null(result$selected$rmse),
                abs(result$selected$accuracy - mean(expected == as.character(y[rows]))) < 1e-10,
                abs(result$selected$accuracy -
                    mean(result$selected$predictions == result$targets)) < 1e-10,
                is.list(result$unseen_validation_classes),
                identical(result$unseen_validation_classes,
                          result$owner$evidence$unseen_validation_classes))
    } else {
      stopifnot(max(abs(result$targets - y[rows])) < 1e-12,
                max(abs(result$selected$predictions - expected)) < 1e-10,
                abs(result$selected$rmse -
                    sqrt(mean((result$selected$predictions - y[rows])^2))) < 1e-10,
                is.null(result$selected$accuracy))
    }

    # feature_blocks: the owner's join widths in declared branch order for branch
    # cases (6 + 6); no join, nothing to report for augmentation alone.
    check_blocks <- function(blocks) {
      stopifnot(length(blocks) == 1L,
                identical(vapply(blocks[[1L]], `[[`, "", "branch_id"),
                          c("derivative", "scatter")),
                identical(vapply(blocks[[1L]], `[[`, integer(1), "column_count"),
                          c(6L, 6L)))
    }
    if (case$branch) {
      check_blocks(result$selected$feature_blocks)
      check_blocks(result$variants[[1L]]$feature_blocks)
      check_blocks(result$owner$evidence$variants[[1L]]$feature_blocks)
    } else {
      stopifnot(!length(result$selected$feature_blocks),
                !length(result$variants[[1L]]$feature_blocks))
    }
  }
}
cat("portable-owner-combinations: ok (staged private; same-kernel interoperability, not an independent oracle)\n")
