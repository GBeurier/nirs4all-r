# Owner-executed SparsePLS-DA classification (`portable_linear_classification.v1`)
# through the real Core operation `portable-pipeline-run`. Linux/R only.
#
# Prerequisites (otherwise an explicit NOT RUN line, or a failure when
# NIRS4ALL_REQUIRE_CORE_PORTABLE=1):
#   NIRS4ALL_CORE_CLI  nirs4all-core-archive providing the classification profile
#   N4M_LIB_PATH       absolute path of the Methods libn4m used by the owner
#
# References are the current local R fit/predict of the same named profile
# (`n4m.SparsePLSDA`, as in n4m-classification.R): owner-vs-R equality is
# same-kernel transport parity, not an independent reference (the independent
# Python reference remains trained-sparse-plsda-cross-language.R). The owner's
# accuracy and winner are never recomputed by the host beyond consistency checks
# on the owner's own numbers.
library(nirs4all)

cli <- Sys.getenv("NIRS4ALL_CORE_CLI")
library_path <- Sys.getenv("N4M_LIB_PATH")
if (!nzchar(cli) || !file.exists(cli) || !nzchar(library_path) || !file.exists(library_path)) {
  if (identical(Sys.getenv("NIRS4ALL_REQUIRE_CORE_PORTABLE"), "1"))
    stop("portable-owner-classification.R requires NIRS4ALL_CORE_CLI and N4M_LIB_PATH")
  cat("NOT RUN: portable-owner-classification.R (set NIRS4ALL_CORE_CLI and N4M_LIB_PATH)\n")
  quit(save = "no", status = 0L)
}
run <- function(definition, dataset)
  nirs4all_run_portable_pipeline(definition, dataset, core_cli = cli,
                                 methods_library = library_path)

rows <- as.vector(rbind(1:12, 51:62, 101:112))
X <- as.matrix(iris[rows, 1:4])
levels_unsorted <- c("virginica", "setosa", "versicolor")   # not alphabetical
y <- factor(as.character(iris$Species[rows]), levels = levels_unsorted)
portable <- list(name = "native_sparse_pls_da", pipeline = list(
  list(class = "n4m.SNV"),
  list(model = list(class = "n4m.SparsePLSDA", params = list(sparsity_lambda = 0.05)),
       `_range_` = list(1L, 2L, 1L), param = "n_components")))
holdout <- portable
holdout$pipeline <- c(list(list(class = "n4m.KennardStone",
                                params = list(test_size = 0.25))), portable$pipeline)
candidate <- function(n) nirs4all_pipeline(list(nirs4all_snv()),
                                           nirs4all_sparse_pls_da(n, 0.05))

# 1. Kennard-Stone holdout: owner split, scope, ordered candidates, ids decoded
# through R's own (unsorted) level order.
split <- n4m::kennard_stone_split(X, test_size = 0.25, zero_based = TRUE)
train <- split$train + 1L
held <- split$test + 1L
result <- run(holdout, list(X = X, y = y))
stopifnot(identical(result$mode, "single_holdout"),
          identical(result$profile$name, "portable_linear_classification.v1"),
          identical(result$owner$profile, "single_resampled_holdout_bank.v2"),
          identical(result$split$kind, "KennardStone"),
          identical(result$split$trainIndices, as.integer(split$train)),
          identical(result$split$testIndices, as.integer(split$test)),
          identical(result$evaluation, list(scope = "selection_validation",
                                            independent_test = FALSE)),
          identical(result$class_values, levels_unsorted),
          identical(result$targets, as.character(y[held])),
          identical(vapply(result$variants, `[[`, integer(1), "n_components"), 1:2))
for (index in seq_along(result$variants)) {
  reference <- as.character(predict(nirs4all_fit(candidate(index),
    X[train, , drop = FALSE], y[train]), X[held, , drop = FALSE]))
  stopifnot(identical(result$variants[[index]]$predictions, reference),
            identical(result$variants[[index]]$accuracy,
                      mean(result$variants[[index]]$predictions == result$targets)))
}
accuracy <- vapply(result$variants, `[[`, numeric(1), "accuracy")
stopifnot(identical(result$selected$ordinal, result$variants[[which.max(accuracy)]]$ordinal))
# The unseen-class report is the owner's, in both places, in the documented shape.
stopifnot(is.list(result$unseen_validation_classes),
          identical(result$unseen_validation_classes,
                    result$owner$evidence$unseen_validation_classes),
          all(vapply(result$unseen_validation_classes, function(entry)
            identical(names(entry), c("class_id", "count", "sample_ids")), logical(1))))

# 2. No splitter: training scope on all rows, report still present.
trained <- run(portable, list(X = X, y = y))
stopifnot(identical(trained$mode, "training"), identical(trained$split$kind, "all"),
          identical(trained$owner$profile, "training_bank.v2"),
          identical(trained$evaluation, list(scope = "training", independent_test = FALSE)),
          identical(trained$targets, as.character(y)),
          is.list(trained$unseen_validation_classes))

# 3. Numeric class codes keep their type: integer codes and double codes.
integer_codes <- as.integer(y) - 1L
by_integer <- run(portable, list(X = X, y = integer_codes))
stopifnot(is.integer(by_integer$targets), identical(by_integer$targets, integer_codes),
          all(vapply(by_integer$variants, function(variant)
            is.integer(variant$predictions) &&
              all(variant$predictions %in% unique(integer_codes)), logical(1))))
double_codes <- c(10.5, 20.5, 30.5)[as.integer(y)]
by_double <- run(portable, list(X = X, y = double_codes))
stopifnot(is.double(by_double$targets), identical(by_double$targets, double_codes))

# 4. Formats-backed categorical dataset: declared IDs, axis, metadata retained.
ids <- sprintf("sample-%02d", seq_len(nrow(X)))
metadata <- lapply(seq_along(ids), function(i) list(batch = paste0("b", i %% 3L)))
formats <- structure(list(X = `rownames<-`(X, ids),
  y = `names<-`(as.character(y), ids), sample_ids = ids,
  wavelengths = seq(1100, by = 2.5, length.out = ncol(X)), axis_kind = "wavelength",
  axis_unit = "nm", signal_type = "reflectance", target_name = "species",
  metadata = metadata, formats = NULL, provenance = NULL), class = "nirs4all_dataset")
declared <- run(holdout, formats)
stopifnot(identical(declared$identities$kind, "declared"),
          identical(unlist(declared$identities$sample_ids), ids),
          identical(unlist(declared$identities$training_sample_ids), ids[train]),
          identical(declared$class_values, sort(unique(as.character(y)))),
          identical(declared$split$testIndices, result$split$testIndices),
          identical(declared$identities$metadata, jsonlite::fromJSON(jsonlite::toJSON(
            metadata, auto_unbox = TRUE), simplifyVector = FALSE)))

# 5. Flags of the original document reach the owner as distinct admitted plans.
plan <- nirs4all_parse_execution_plan(holdout, core_cli = cli)
stopifnot(identical(plan$admission, "owner"),
          identical(plan$owner$profile, "portable_linear_classification.v1"),
          identical(vapply(plan$owner$candidates, function(item)
            as.integer(item$n_components), integer(1)), 1:2),
          all(vapply(plan$owner$candidates, function(item)
            identical(item$sparsity_lambda, 0.05), logical(1))))
cat("portable-owner-classification: ok\n")
