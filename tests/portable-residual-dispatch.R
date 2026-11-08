# Declared-profile dispatch of nirs4all_run_portable_pipeline().
#
# Linear regression recipes go to the Core owner operation; classification,
# feature branches, training augmentations and models with extra fit inputs are
# the named temporary residual B01 profiles and keep the unchanged local R
# execution. The route is chosen from the parsed recipe BEFORE execution, never
# by catching an owner error. Every residual entry is a required later
# migration, not a parity claim. No Core CLI is needed here: a failing stub
# proves that residual recipes never reach the owner.
library(nirs4all)

residual_table <- nirs4all:::.nirs4all_portable_residual_b01
stopifnot(identical(sort(names(residual_table)),
  c("classification", "extra_fit_inputs", "feature_branch", "opls_solver",
    "train_augmentation")),
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

# Every other PLS solver stays on the owner route: only the declared opls_nipals
# algorithm is residual, and it is never renamed to a PLS NIPALS solver.
pls_recipe <- function(algo) list(pipeline = list(list(class = "n4m.SNV"),
  list(model = list(class = "n4m.PLS", params = list(n_components = 2L, algo = algo)))))
for (algo in c("pls_simpls", "pls_nipals", "pls_kernel_algorithm", "pcr_svd"))
  stopifnot(identical(nirs4all_parse_execution_plan(pls_recipe(algo))$profile$route, "owner"))
opls <- nirs4all_parse_execution_plan(pls_recipe("opls_nipals"))
stopifnot(identical(opls$profile$route, "residual_b01"),
          identical(unname(opls$profile$residual), "opls_solver"),
          identical(opls$learner$algo, "opls_nipals"),
          identical(unname(opls$profile$reasons), residual_table$opls_solver$reason))

# Residual profiles, each with its exact declared key, source reason and seam.
cases <- list(
  classification = nirs4all_pipeline(list(nirs4all_snv()), nirs4all_sparse_pls_da(2L)),
  feature_branch = nirs4all_pipeline(list(nirs4all_concat(list(
    a = list(nirs4all_snv()), b = list(nirs4all_msc())))), nirs4all_pls(2L)),
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

# A recipe may be residual for several declared reasons at once.
combined <- nirs4all_pipeline(list(nirs4all_concat(list(
  a = list(nirs4all_snv()), b = list(nirs4all_msc())))), nirs4all_sparse_pls_da(2L))
stopifnot(identical(unname(route(combined)$residual), c("classification", "feature_branch")))

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
  # opls_nipals executes through the unchanged local R path and never reaches the owner.
  unlink(file.path(capture, "called"))
  opls_pipeline <- nirs4all_pipeline(list(nirs4all_snv()),
                                     nirs4all_pls(2L, algo = "opls_nipals"))
  opls_result <- nirs4all_run_portable_pipeline(pls_recipe("opls_nipals"),
    list(X = X, y = y), core_cli = stub)
  stopifnot(!file.exists(file.path(capture, "called")),
            identical(unname(opls_result$profile$residual), "opls_solver"),
            max(abs(opls_result$selected$predictions -
                    predict(nirs4all_fit(opls_pipeline, X, y), X))) < 1e-10)
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
error <- tryCatch(nirs4all_run_portable_pipeline(nirs4all_export_pipeline(cases$classification),
  backed), error = identity)
stopifnot(inherits(error, "error"), grepl("record-backed", conditionMessage(error), fixed = TRUE))
cat("portable-residual-dispatch: ok\n")
