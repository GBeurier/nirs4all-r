# Declared-profile dispatch of nirs4all_run_portable_pipeline().
#
# Linear regression (including opls_nipals), SparsePLS-DA classification,
# regression feature-branch recipes, standalone regression training augmentation /
# MBPLS block_sizes / GroupSparsePLS group_assignment and the combinations
# classifier + feature branch, classifier + training augmentation and feature
# branch + MBPLS / GroupSparsePLS all derive their owner profile name from the
# parsed recipe BEFORE execution, never by catching an owner error; there is no
# local runner. Routing only: no fit parity is shown here. No Core CLI is needed:
# a failing stub proves an owner failure is an error, not a local run.
library(nirs4all)

route <- function(pipeline) nirs4all_parse_execution_plan(
  nirs4all_export_pipeline(pipeline))$profile

# Linear regression profiles.
for (pipeline in list(
    nirs4all_pipeline(list(nirs4all_snv(), nirs4all_savgol(5L)), nirs4all_pls(2L)),
    nirs4all_pipeline(list(nirs4all_msc(), nirs4all_detrend(1L)), nirs4all_pls(3L)),
    nirs4all_pipeline(learner = nirs4all_n4m_method("ridge")),
    nirs4all_pipeline(learner = nirs4all_n4m_method("cppls")))) {
  profile <- route(pipeline)
  stopifnot(identical(profile$name, "portable_linear_regression.v1"))
}

# Every PLS algorithm, including opls_nipals (generic role models.pls.opls) keeps
# its declared algo is never renamed to another solver.
pls_recipe <- function(algo) list(pipeline = list(list(class = "n4m.SNV"),
  list(model = list(class = "n4m.PLS", params = list(n_components = 2L, algo = algo)))))
for (algo in c("pls_simpls", "pls_nipals", "pls_kernel_algorithm", "pcr_svd", "opls_nipals")) {
  plan <- nirs4all_parse_execution_plan(pls_recipe(algo))
  stopifnot(identical(plan$learner$algo, algo))
}

# SparsePLS-DA classification: its own versioned profile.
classifier <- nirs4all_parse_execution_plan(nirs4all_export_pipeline(
  nirs4all_pipeline(list(nirs4all_snv()), nirs4all_sparse_pls_da(2L, 0.05))))
stopifnot(identical(classifier$profile$name, "portable_linear_classification.v1"),
          identical(classifier$admission, "source_preflight"), is.null(classifier$owner),
          is.null(classifier$profile$route), is.null(classifier$profile$residual))

# A regression feature branch alone: its own versioned profile.
branch_alone <- nirs4all_pipeline(list(nirs4all_concat(list(
  a = list(nirs4all_snv()), b = list(nirs4all_msc())))), nirs4all_pls(2L))
feature <- route(branch_alone)
stopifnot(identical(feature$name, "portable_feature_regression.v1"))

# Standalone regression training augmentation and extra fit inputs: the existing
# graph profile (the owner types values and supplies the feature
# axis inputs; R only forwards the original document).
standalone <- list(
  augmentation = nirs4all_pipeline(learner = nirs4all_pls(2L), augmentations =
    list(nirs4all_native_augmentation("gaussian_noise", 0.05, seed = 3))),
  mbpls = nirs4all_pipeline(learner = nirs4all_n4m_method("mb_pls", 2L,
    list(block_sizes = c(4L, 4L, 4L)))),
  group_sparse = nirs4all_pipeline(learner = nirs4all_group_sparse_pls(2L,
    c(1L, 1L, 2L, 2L), 0.2)),
  both = nirs4all_pipeline(learner = nirs4all_n4m_method("mb_pls", 2L,
    list(block_sizes = c(4L, 4L, 4L))), augmentations =
    list(nirs4all_native_augmentation("gaussian_noise", 0.05, seed = 3))))
for (name in names(standalone)) {
  profile <- route(standalone[[name]])
  stopifnot(identical(profile$name, "portable_feature_regression.v1"))
}

# Combined owner profiles: classifier with a branch or augmentation use the
# graph classification profile, branch with MBPLS / GroupSparsePLS the existing
# graph regression profile. The original document is sent unchanged (no flattening).
branch_steps <- list(nirs4all_concat(list(a = list(nirs4all_snv()), b = list(nirs4all_msc()))))
seeded <- list(nirs4all_native_augmentation("gaussian_noise", 0.05, seed = 3))
classification_graph <- "portable_feature_classification.v1"
regression_graph <- "portable_feature_regression.v1"
combined <- list(
  branch_classifier = list(nirs4all_pipeline(branch_steps, nirs4all_sparse_pls_da(2L)),
                           classification_graph),
  augmentation_classifier = list(nirs4all_pipeline(learner = nirs4all_sparse_pls_da(2L),
    augmentations = seeded), classification_graph),
  branch_mbpls = list(nirs4all_pipeline(branch_steps, nirs4all_n4m_method("mb_pls", 2L,
    list(block_sizes = c(4L, 4L, 4L)))), regression_graph),
  branch_group_sparse = list(nirs4all_pipeline(branch_steps,
    nirs4all_group_sparse_pls(2L, c(1L, 1L, 2L, 2L), 0.2)), regression_graph))
for (name in names(combined)) {
  profile <- route(combined[[name]][[1L]])
  stopifnot(identical(profile$name, combined[[name]][[2L]]))
}
# Every classification variant keeps the classification codec path; the linear and
# graph classification profiles are the only classification profiles.
stopifnot(identical(nirs4all:::.nirs4all_portable_classification_profiles,
  c("portable_linear_classification.v1", classification_graph)))

# A combination no owner profile admits (augmentation with a branch) is refused by
# the portable grammar before any native call; it is not run locally.
grammar_refusal <- tryCatch(nirs4all_parse_execution_plan(list(pipeline = list(
  list(train_augmentation = list(class = "n4m.NativeXAugmentation",
    params = list(kind = "gaussian_noise", values = list(0.05), seed = 3))),
  list(branch = list(a = list(list(class = "n4m.SNV")), b = list(list(class = "n4m.MSC")))),
  list(merge = "features"), list(model = list(class = "n4m.PLS"))))), error = identity)
stopifnot(inherits(grammar_refusal, "error"))

# An owner failure is an error, never a local rerun, for the linear profile and for
# each combined profile: the failing stub is called and its refusal propagates.
capture <- tempfile("residual-stub-")
dir.create(capture)
if (.Platform$OS.type != "windows") {
  stub <- file.path(capture, "nirs4all-core-archive")
  writeLines(c("#!/bin/sh", "echo called > \"$(dirname \"$0\")/called\"", "exit 9"), stub)
  Sys.chmod(stub, "0755")
  library_file <- file.path(capture, "libn4m.so")
  writeLines("stub", library_file)
  X <- outer(seq_len(21L), seq_len(12L), function(s, b)
    sin(s * b / 9) + cos(s + b / 7) + s * b / 100)
  y <- 1.3 + 0.7 * X[, 2L] - 0.4 * X[, 6L]
  labels <- factor(rep(c("c", "a", "b"), 7L), levels = c("c", "a", "b"))
  runs <- list(
    linear = list(nirs4all_pipeline(list(nirs4all_snv()), nirs4all_pls(2L)), y),
    branch_classifier = list(combined$branch_classifier[[1L]], labels),
    augmentation_classifier = list(combined$augmentation_classifier[[1L]], labels),
    branch_mbpls = list(nirs4all_pipeline(branch_steps, nirs4all_n4m_method("mb_pls", 2L,
      list(block_sizes = c(12L, 12L)))), y))
  for (name in names(runs)) {
    unlink(file.path(capture, "called"))
    error <- tryCatch(nirs4all_run_portable_pipeline(nirs4all_export_pipeline(runs[[name]][[1L]]),
      list(X = X, y = runs[[name]][[2L]]), core_cli = stub, methods_library = library_file),
      error = identity)
    stopifnot(inherits(error, "nirs4all_portable_owner_refusal"),
              file.exists(file.path(capture, "called")))
  }
}

# A record-backed classification dataset is forwarded to the owner (which refuses
# it by name); R neither parses the record nor invents X/y.
if (.Platform$OS.type != "windows") {
  backed <- structure(list(record = list(schema = "nirs4all.dataset.v1", dataset = list()),
                           core_cli = stub), class = "nirs4all_dataset")
  unlink(file.path(capture, "called"))
  error <- tryCatch(nirs4all_run_portable_pipeline(
    nirs4all_export_pipeline(combined$branch_classifier[[1L]]), backed,
    methods_library = library_file), error = identity)
  stopifnot(inherits(error, "nirs4all_portable_owner_refusal"),
            file.exists(file.path(capture, "called")))
}
cat("portable-residual-dispatch: ok\n")
