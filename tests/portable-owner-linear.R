# Linear regression profile of nirs4all_run_portable_pipeline() through the real
# Core owner operation `portable-pipeline-run` (DAG single-holdout bank / training
# sibling over Methods kernels). Linux/R only; Octave and MATLAB are separate
# bindings and are not qualified here.
#
# Prerequisites (otherwise an explicit NOT RUN line, or a failure when
# NIRS4ALL_REQUIRE_CORE_PORTABLE=1):
#   NIRS4ALL_CORE_CLI  nirs4all-core-archive providing portable-pipeline-plan/run
#   N4M_LIB_PATH       absolute path of the Methods libn4m used by the owner
# Optional: NIRS4ALL_PORTABLE_IO_RECORD, an IO dataset JSON file with one finite
# single source, to exercise the record-backed form.
#
# References are the current local R fit/predict (the same n4m kernels the older
# portable tests use); the owner result is compared to them at the established
# 1e-10 tolerance. The owner's own numbers are never recomputed by the host.
library(nirs4all)

refused <- function(expr, pattern) {
  error <- tryCatch({ force(expr); NULL }, error = identity)
  stopifnot(inherits(error, "error"), grepl(pattern, conditionMessage(error), fixed = TRUE))
}

X <- outer(seq_len(30L), seq_len(14L), function(i, j)
  sin(i * j / 11) + cos(i / 5 + j / 9) + i * j / 200)
y <- 1 + 0.7 * X[, 2L] - 0.4 * X[, 9L]
sg <- list(window_length = 7L, polyorder = 2L, deriv = 1L, mode = "nearest", cval = 0)
pls_flags <- list(algo = "pls_simpls", center_x = TRUE, scale_x = FALSE,
                  center_y = TRUE, scale_y = TRUE)
recipe <- function(snv = list(ddof = 1L, with_mean = TRUE, with_std = FALSE),
                   flags = pls_flags, splitter = TRUE) {
  steps <- list()
  if (splitter) steps[[1L]] <- list(class = "n4m.KennardStone", params = list(test_size = 0.3))
  steps[[length(steps) + 1L]] <- list(class = "n4m.SNV", params = snv)
  steps[[length(steps) + 1L]] <- list(class = "n4m.SavitzkyGolay", params = sg)
  steps[[length(steps) + 1L]] <- list(model = list(class = "n4m.PLS",
    params = c(list(n_components = 2L), flags)), `_range_` = c(1L, 4L, 1L),
    param = "n_components")
  list(name = "linear_holdout", pipeline = steps)
}
manual <- function(components, snv = list(ddof = 1L, with_mean = TRUE, with_std = FALSE),
                   flags = pls_flags)
  nirs4all_pipeline(list(do.call(nirs4all_snv, unname(snv)),
    nirs4all_savgol(sg$window_length, sg$polyorder, sg$deriv, delta = 1,
                    mode = sg$mode, cval = sg$cval)),
    do.call(nirs4all_pls, c(list(n_components = components), flags)))

# Source-level preflight refusals are preserved and need no native library.
refused(nirs4all_parse_execution_plan(list(pipeline = list(
  list(class = "n4m.KennardStone", params = list(test_size = 0.3, n_train = 20L)),
  list(model = list(class = "n4m.PLS"))))), "Kennard-Stone")
refused(nirs4all_parse_execution_plan(list(pipeline = list(
  list(class = "n4m.SavitzkyGolay", params = list(delta = 2)),
  list(model = list(class = "n4m.PLS"))))), "delta=1")

cli <- Sys.getenv("NIRS4ALL_CORE_CLI")
library_path <- Sys.getenv("N4M_LIB_PATH")
if (!nzchar(cli) || !file.exists(cli) || !nzchar(library_path) || !file.exists(library_path)) {
  if (identical(Sys.getenv("NIRS4ALL_REQUIRE_CORE_PORTABLE"), "1"))
    stop("portable-owner-linear.R requires NIRS4ALL_CORE_CLI and N4M_LIB_PATH")
  cat("NOT RUN: portable-owner-linear.R (set NIRS4ALL_CORE_CLI and N4M_LIB_PATH)\n")
  quit(save = "no", status = 0L)
}
run <- function(definition, dataset)
  nirs4all_run_portable_pipeline(definition, dataset, core_cli = cli,
                                 methods_library = library_path)

# 1. Kennard-Stone holdout: owner split order, scope, flags, inclusive candidates.
split <- n4m::kennard_stone_split(X, test_size = 0.3, zero_based = TRUE)
train <- split$train + 1L
test <- split$test + 1L
result <- run(recipe(), list(X = X, y = y))
stopifnot(identical(result$mode, "single_holdout"),
          identical(result$split$kind, "KennardStone"),
          identical(result$split$trainIndices, as.integer(split$train)),
          identical(result$split$testIndices, as.integer(split$test)),
          identical(result$evaluation, list(scope = "selection_validation",
                                            independent_test = FALSE)),
          identical(result$profile$route, "owner"),
          identical(result$owner$profile, "single_resampled_holdout_bank.v1"),
          identical(vapply(result$variants, `[[`, integer(1), "n_components"), 1:4),
          max(abs(result$targets - y[test])) < 1e-12)
for (variant in result$variants) {
  reference <- predict(nirs4all_fit(manual(variant$n_components), X[train, , drop = FALSE],
                                    y[train]), X[test, , drop = FALSE])
  stopifnot(max(abs(variant$predictions - reference)) < 1e-10,
            abs(variant$rmse - sqrt(mean((variant$predictions - result$targets)^2))) < 1e-10)
}
rmse <- vapply(result$variants, `[[`, numeric(1), "rmse")
stopifnot(identical(result$selected$n_components, result$variants[[which.min(rmse)]]$n_components),
          identical(result$selected$ordinal, result$variants[[which.min(rmse)]]$ordinal))

# 2. Supplied SNV and PLS controls reach the owner: distinct admitted plans and results.
plan <- nirs4all_parse_execution_plan(recipe(), core_cli = cli)
stopifnot(identical(plan$admission, "owner"), identical(plan$owner$mode, "single_holdout"),
          identical(as.integer(unlist(plan$owner$nComponents)), 1:4),
          identical(vapply(plan$owner$candidates, function(item)
            as.integer(item$n_components), integer(1)), 1:4))
steps <- plan$owner$recipe
snv <- Filter(function(step) grepl("snv", step$methodId, fixed = TRUE), steps)[[1L]]
stopifnot(identical(snv$params$with_std, FALSE), identical(snv$params$ddof, 1L),
          identical(steps[[length(steps)]]$params$scale_x, FALSE))
default <- run(recipe(snv = list(), flags = list()), list(X = X, y = y))
stopifnot(max(abs(default$variants[[2L]]$predictions - result$variants[[2L]]$predictions)) > 1e-6)

# 3. No splitter: training mode, all rows, no OOF or independent-test claim.
trained <- run(recipe(splitter = FALSE), list(X = X, y = y))
stopifnot(identical(trained$mode, "training"), identical(trained$split$kind, "all"),
          identical(trained$split$trainIndices, 0:(nrow(X) - 1L)),
          identical(trained$split$testIndices, 0:(nrow(X) - 1L)),
          identical(trained$evaluation, list(scope = "training", independent_test = FALSE)),
          identical(trained$owner$profile, "training_bank.v1"))
reference <- predict(nirs4all_fit(manual(2L), X, y), X)
stopifnot(max(abs(trained$variants[[2L]]$predictions - reference)) < 1e-10)

# 4. Identity scope: positional for plain matrices, declared for formats-backed data.
stopifnot(identical(result$identities$kind, "positional"),
          identical(unlist(result$identities$sample_ids)[1L], "position_0"))
ids <- sprintf("sample-%02d", seq_len(nrow(X)))
formats <- structure(list(X = `rownames<-`(X, ids), y = `names<-`(y, ids), sample_ids = ids,
  wavelengths = seq(1100, by = 2.5, length.out = ncol(X)), axis_kind = "wavelength",
  axis_unit = "nm", signal_type = "reflectance", target_name = "y", metadata = NULL,
  formats = NULL, provenance = NULL), class = "nirs4all_dataset")
declared <- run(recipe(), formats)
stopifnot(identical(declared$identities$kind, "declared"),
          identical(unlist(declared$identities$training_sample_ids), ids[train]),
          identical(unlist(declared$identities$evaluation_sample_ids), ids[test]),
          identical(declared$split$trainIndices, result$split$trainIndices),
          max(abs(declared$selected$predictions - result$selected$predictions)) < 1e-10)

# 5. Record-backed dataset: owner projection, or an owner refusal by name.
record_file <- Sys.getenv("NIRS4ALL_PORTABLE_IO_RECORD")
if (nzchar(record_file) && file.exists(record_file)) {
  backed <- nirs4all_dataset(record_file, core_cli = cli)
  projected <- run(recipe(), backed)
  stopifnot(identical(projected$identities$kind, "declared"),
            length(projected$identities$sample_ids) == projected$rows)
} else cat("NOT RUN: record-backed case (set NIRS4ALL_PORTABLE_IO_RECORD)\n")
cat("portable-owner-linear: ok\n")
