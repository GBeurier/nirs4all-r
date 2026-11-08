# Declared-profile dispatch of nirs4all_run_portable_pipeline().
#
# Linear regression (including opls_nipals), SparsePLS-DA classification and
# regression feature-branch recipes go to the Core owner operation; training
# augmentations, models with extra fit inputs and a feature branch combined with
# a classifier or with either of those are the named temporary residual B01
# profiles and keep the unchanged local R execution. The route is chosen from the parsed recipe BEFORE execution, never
# by catching an owner error. Every residual entry is a required later
# migration, not a parity claim. No Core CLI is needed here: a failing stub
# proves that residual recipes never reach the owner.
library(nirs4all)

residual_table <- nirs4all:::.nirs4all_portable_residual_b01
stopifnot(identical(sort(names(residual_table)),
  c("extra_fit_inputs", "feature_branch", "train_augmentation")),
  all(vapply(residual_table, function(entry)
    is.character(entry$reason) && nzchar(entry$reason) &&
      is.character(entry$next_seam) && nzchar(entry$next_seam), logical(1))))

route <- function(pipeline) nirs4all_parse_execution_plan(
  nirs4all_export_pipeline(pipeline))$profile

# Linear regression profiles: owner route.
for (pipeline in list(
    nirs4all_pipeline(list(nirs4all_snv(), nirs4all_savgol(5L)), nirs4all_pls(2L)),
    nirs4all_pipeline(list(nirs4all_msc(), nirs4all_detrend(1L)), nirs4all_pls(3L)),
    nirs4all_pipeline(learner = nirs4all_n4m_method("ridge")),
    nirs4all_pipeline(learner = nirs4all_n4m_method("cppls")))) {
  profile <- route(pipeline)
  stopifnot(identical(profile$route, "owner"),
            identical(profile$name, "portable_linear_regression.v1"),
            !length(profile$residual))
}

# Every PLS algorithm, including opls_nipals (generic role models.pls.opls), is on
# the owner route; the declared algo is never renamed to another solver.
pls_recipe <- function(algo) list(pipeline = list(list(class = "n4m.SNV"),
  list(model = list(class = "n4m.PLS", params = list(n_components = 2L, algo = algo)))))
for (algo in c("pls_simpls", "pls_nipals", "pls_kernel_algorithm", "pcr_svd", "opls_nipals")) {
  plan <- nirs4all_parse_execution_plan(pls_recipe(algo))
  stopifnot(identical(plan$profile$route, "owner"), !length(plan$profile$residual),
            identical(plan$learner$algo, algo))
}

# SparsePLS-DA classification: owner route with its own versioned profile.
classifier <- nirs4all_parse_execution_plan(nirs4all_export_pipeline(
  nirs4all_pipeline(list(nirs4all_snv()), nirs4all_sparse_pls_da(2L, 0.05))))
stopifnot(identical(classifier$profile$route, "owner"),
          identical(classifier$profile$name, "portable_linear_classification.v1"),
          !length(classifier$profile$residual))

# A regression feature branch alone: owner route, its own versioned profile.
branch_alone <- nirs4all_pipeline(list(nirs4all_concat(list(
  a = list(nirs4all_snv()), b = list(nirs4all_msc())))), nirs4all_pls(2L))
feature <- route(branch_alone)
stopifnot(identical(feature$route, "owner"),
          identical(feature$name, "portable_feature_regression.v1"),
          !length(feature$residual))

# Residual profiles, each with its exact declared key, source reason and seam.
cases <- list(
  train_augmentation = nirs4all_pipeline(learner = nirs4all_pls(2L), augmentations =
    list(nirs4all_native_augmentation("gaussian_noise", 0.05, seed = 3))),
  extra_fit_inputs = nirs4all_pipeline(learner = nirs4all_n4m_method("mb_pls", 2L,
    list(block_sizes = c(4L, 4L, 4L)))))
for (key in names(cases)) {
  profile <- route(cases[[key]])
  stopifnot(identical(profile$route, "residual_b01"),
            identical(unname(profile$residual), key),
            identical(unname(profile$reasons), residual_table[[key]]$reason),
            identical(unname(profile$next_seams), residual_table[[key]]$next_seam))
}
group <- nirs4all_pipeline(learner = nirs4all_group_sparse_pls(2L, c(1L, 1L, 2L, 2L), 0.2))
stopifnot(identical(unname(route(group)$residual), "extra_fit_inputs"))

# A branch combined with a classifier or another residual profile stays declared
# residual through `feature_branch` (the combined owner profile is not qualified).
combined <- nirs4all_pipeline(list(nirs4all_concat(list(
  a = list(nirs4all_snv()), b = list(nirs4all_msc())))), nirs4all_sparse_pls_da(2L))
combined_profile <- route(combined)
stopifnot(identical(combined_profile$route, "residual_b01"),
          identical(unname(combined_profile$residual), "feature_branch"),
          identical(unname(combined_profile$reasons), residual_table$feature_branch$reason),
          identical(unname(combined_profile$next_seams), residual_table$feature_branch$next_seam))
branch_mbpls <- nirs4all_pipeline(list(nirs4all_concat(list(
  a = list(nirs4all_snv()), b = list(nirs4all_msc())))),
  nirs4all_n4m_method("mb_pls", 2L, list(block_sizes = c(4L, 4L, 4L))))
stopifnot(identical(unname(route(branch_mbpls)$residual),
                    c("feature_branch", "extra_fit_inputs")))
combined_augmented <- nirs4all_pipeline(learner = nirs4all_sparse_pls_da(2L),
  augmentations = list(nirs4all_native_augmentation("gaussian_noise", 0.05, seed = 3)))
stopifnot(identical(unname(route(combined_augmented)$residual), "train_augmentation"))

# Residual execution is the unchanged local R runner and never calls the owner.
capture <- tempfile("residual-stub-")
dir.create(capture)
if (.Platform$OS.type != "windows") {
  stub <- file.path(capture, "nirs4all-core-archive")
  writeLines(c("#!/bin/sh", "echo called > \"$(dirname \"$0\")/called\"", "exit 9"), stub)
  Sys.chmod(stub, "0755")
  X <- outer(seq_len(21L), seq_len(12L), function(s, b)
    sin(s * b / 9) + cos(s + b / 7) + s * b / 100)
  y <- 1.3 + 0.7 * X[, 2L] - 0.4 * X[, 6L]
  mbpls <- nirs4all_pipeline(learner = nirs4all_n4m_method("mb_pls", 2L,
    list(block_sizes = c(4L, 4L, 4L))))
  result <- nirs4all_run_portable_pipeline(nirs4all_export_pipeline(mbpls),
    list(X = X, y = y), core_cli = stub)
  stopifnot(!file.exists(file.path(capture, "called")),
            identical(result$profile$route, "residual_b01"),
            identical(unname(result$profile$residual), "extra_fit_inputs"),
            identical(result$evaluation, list(scope = "training", independent_test = FALSE)),
            max(abs(result$selected$predictions -
                    predict(nirs4all_fit(mbpls, X, y), X))) < 1e-10)
  # The linear profile has no such reroute: an owner failure is an error.
  linear <- nirs4all_export_pipeline(nirs4all_pipeline(list(nirs4all_snv()), nirs4all_pls(2L)))
  library_file <- file.path(capture, "libn4m.so")
  writeLines("stub", library_file)
  error <- tryCatch(nirs4all_run_portable_pipeline(linear, list(X = X, y = y),
    core_cli = stub, methods_library = library_file), error = identity)
  stopifnot(inherits(error, "nirs4all_portable_owner_refusal"),
            file.exists(file.path(capture, "called")))
}

# A record-backed dataset cannot feed a residual profile; no X/y is invented.
backed <- structure(list(record = list(schema = "nirs4all.dataset.v1", dataset = list()),
                         core_cli = "unused"), class = "nirs4all_dataset")
error <- tryCatch(nirs4all_run_portable_pipeline(nirs4all_export_pipeline(combined),
  backed), error = identity)
stopifnot(inherits(error, "error"), grepl("record-backed", conditionMessage(error), fixed = TRUE))
cat("portable-residual-dispatch: ok\n")
