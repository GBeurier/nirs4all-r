# Real-CLI check that the PLS algorithm `opls_nipals` is owner-executed
# (Methods role `models.pls.opls`) by nirs4all_run_portable_pipeline(), in the
# two existing scopes: training (no splitter) and an ordered Kennard-Stone
# holdout. Linux/R only.
#
# Prerequisites (otherwise an explicit NOT RUN line, or a failure when
# NIRS4ALL_REQUIRE_CORE_PORTABLE=1):
#   NIRS4ALL_CORE_CLI  nirs4all-core-archive providing portable-pipeline-run
#   N4M_LIB_PATH       absolute path of the Methods libn4m used by the owner
#
# The reference is the current local R fit/predict of the same MSC + OPLS
# pipeline with identical explicit flags, fitted on the training rows only. This
# is same-kernel R/native interoperability, NOT an independent mathematical
# oracle. One fixed component count, no range or search; the run offers no extra
# winner refit and this test makes no deployment claim.
library(nirs4all)

cli <- Sys.getenv("NIRS4ALL_CORE_CLI")
library_path <- Sys.getenv("N4M_LIB_PATH")
if (!nzchar(cli) || !file.exists(cli) || !nzchar(library_path) || !file.exists(library_path)) {
  if (identical(Sys.getenv("NIRS4ALL_REQUIRE_CORE_PORTABLE"), "1"))
    stop("portable-owner-opls.R requires NIRS4ALL_CORE_CLI and N4M_LIB_PATH")
  cat("NOT RUN: portable-owner-opls.R (set NIRS4ALL_CORE_CLI and N4M_LIB_PATH)\n")
  quit(save = "no", status = 0L)
}
run <- function(definition, dataset)
  nirs4all_run_portable_pipeline(definition, dataset, core_cli = cli,
                                 methods_library = library_path)

# Small deterministic public synthetic data.
n <- 24L
X <- outer(seq_len(n), seq_len(10L), function(i, j)
  sin(i * j / 9) + cos(i / 4 + j / 6) + i * j / 150)
y <- 1.1 + 0.8 * X[, 2L] - 0.5 * X[, 7L] + 0.05 * sin(seq_len(n))
data <- list(X = X, y = y)

# The full portable original document: MSC then PLS with algo opls_nipals and
# explicit, non-default scaling flags (the manual reference uses the same ones).
flags <- list(center_x = TRUE, scale_x = FALSE, center_y = TRUE, scale_y = FALSE)
model <- list(class = "n4m.PLS",
              params = c(list(n_components = 2L, algo = "opls_nipals"), flags))
steps <- list(list(class = "n4m.MSC"), list(model = model))
training_recipe <- list(name = "opls_training", pipeline = steps)
holdout_recipe <- list(name = "opls_holdout", pipeline = c(list(list(
  class = "n4m.KennardStone", params = list(test_size = 0.25))), steps))
pipeline <- nirs4all_pipeline(list(nirs4all_msc()), do.call(nirs4all_pls,
  c(list(n_components = 2L, algo = "opls_nipals"), flags)))

# Declared dispatch: owner route, no residual key, the algorithm is not renamed.
plan <- nirs4all_parse_execution_plan(training_recipe)
stopifnot(identical(plan$profile$route, "owner"), !length(plan$profile$residual),
          identical(plan$profile$name, "portable_linear_regression.v1"),
          identical(plan$learner$algo, "opls_nipals"))

# The owner's effective terminal recipe step, from its evidence: the selected
# entry in training mode, the selected variant's identity in holdout mode.
terminal_step <- function(result) {
  evidence <- result$owner$evidence
  recipe <- if (identical(result$mode, "training")) evidence$selected$recipe else {
    ordinals <- vapply(evidence$variants, function(variant) variant$identity$ordinal, "")
    evidence$variants[[match(result$selected$ordinal, ordinals)]]$identity$recipe
  }
  stopifnot(is.list(recipe), length(recipe) == 2L)
  list(first = recipe[[1L]], last = recipe[[2L]])
}
check_identity <- function(result) {
  step <- terminal_step(result)
  params <- step$last$params
  stopifnot(identical(step$first$methodId, "preprocessing.scatter.msc"),
            identical(step$last$methodId, "models.pls.opls"),
            identical(as.integer(params$n_components), 2L),
            identical(params$center_x, TRUE), identical(params$scale_x, FALSE),
            identical(params$center_y, TRUE), identical(params$scale_y, FALSE),
            is.null(params$algo), is.null(params$solver))   # never PLS NIPALS or a solver
}
check_scores <- function(result, predictions, targets) {
  stopifnot(identical(result$profile$route, "owner"),
            identical(result$profile$name, "portable_linear_regression.v1"),
            !length(result$profile$residual),
            length(result$variants) == 1L,
            identical(result$selected$n_components, 2L),
            identical(result$selected$ordinal, result$variants[[1L]]$ordinal),
            identical(result$evaluation$independent_test, FALSE),
            max(abs(result$targets - targets)) < 1e-12,
            max(abs(result$selected$predictions - predictions)) < 1e-10,
            abs(result$selected$rmse - sqrt(mean((result$selected$predictions - targets)^2))) < 1e-10)
  check_identity(result)
}

# 1. Training scope: all rows fitted and evaluated (resubstitution, not a test).
trained <- run(training_recipe, data)
stopifnot(identical(trained$mode, "training"), identical(trained$split$kind, "all"),
          identical(trained$split$trainIndices, 0:(n - 1L)),
          identical(trained$split$testIndices, 0:(n - 1L)),
          identical(trained$evaluation$scope, "training"))
check_scores(trained, predict(nirs4all_fit(pipeline, X, y), X), y)

# 2. Raw-X Kennard-Stone holdout: owner split order, MSC and OPLS fitted on the
# training rows only, evaluated on the validation rows.
split <- n4m::kennard_stone_split(X, test_size = 0.25, zero_based = TRUE)
train <- split$train + 1L
test <- split$test + 1L
held <- run(holdout_recipe, data)
stopifnot(identical(held$mode, "single_holdout"), identical(held$split$kind, "KennardStone"),
          identical(held$split$trainIndices, as.integer(split$train)),
          identical(held$split$testIndices, as.integer(split$test)),
          identical(held$evaluation$scope, "selection_validation"))
check_scores(held, predict(nirs4all_fit(pipeline, X[train, , drop = FALSE], y[train]),
                           X[test, , drop = FALSE]), y[test])
cat("portable-owner-opls: ok (same-kernel interoperability, not an independent oracle)\n")
