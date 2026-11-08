# Owner-executed regression feature branch (`portable_feature_regression.v1`)
# through the real Core operation `portable-pipeline-run`. Linux/R only.
#
# Prerequisites (otherwise an explicit NOT RUN line, or a failure when
# NIRS4ALL_REQUIRE_CORE_PORTABLE=1):
#   NIRS4ALL_CORE_CLI  nirs4all-core-archive providing the feature graph profile
#   N4M_LIB_PATH       absolute path of the Methods libn4m used by the owner
#
# The recipes and the manual references are those of portable-branch-merge.R and
# n4m-selectors.R (`nirs4all_concat` composition fitted locally in R). Owner-vs-R
# equality is same-kernel transport parity, not an independent reference; the
# independent Python peers stay in the unchanged existing tests. The owner's
# split, scores and winner are not recomputed by the host.
library(nirs4all)

cli <- Sys.getenv("NIRS4ALL_CORE_CLI")
library_path <- Sys.getenv("N4M_LIB_PATH")
if (!nzchar(cli) || !file.exists(cli) || !nzchar(library_path) || !file.exists(library_path)) {
  if (identical(Sys.getenv("NIRS4ALL_REQUIRE_CORE_PORTABLE"), "1"))
    stop("portable-owner-branch.R requires NIRS4ALL_CORE_CLI and N4M_LIB_PATH")
  cat("NOT RUN: portable-owner-branch.R (set NIRS4ALL_CORE_CLI and N4M_LIB_PATH)\n")
  quit(save = "no", status = 0L)
}
run <- function(definition, dataset)
  nirs4all_run_portable_pipeline(definition, dataset, core_cli = cli,
                                 methods_library = library_path)

X <- outer(seq_len(12L), seq_len(8L), function(i, j) sin(i * j / 7) + i * j / 50)
y <- 2 + X[, 2L] - 0.3 * X[, 5L]
recipe <- list(pipeline = list(
  list(branch = list(
    derivative = list(list(class = "n4m.SNV"),
      list(class = "n4m.SavitzkyGolay", params = list(window_length = 5L, polyorder = 2L))),
    scatter = list(list(class = "n4m.MSC"),
      list(class = "n4m.Detrend", params = list(polyorder = 1L))))),
  list(merge = "features"),
  list(model = list(class = "n4m.PLS", params = list(n_components = 2L)))))
manual <- nirs4all_pipeline(list(nirs4all_concat(list(
  derivative = list(nirs4all_snv(), nirs4all_savgol(5L)),
  scatter = list(nirs4all_msc(), nirs4all_detrend(1L))))), nirs4all_pls(2L))

# Promised owner evidence of this known one-join profile (branch-next PUBLIC_API):
# the graph bank profile, no published deployment package, and the join widths at
# every documented path (`variants[].feature_blocks`, `selected$feature_blocks`,
# `owner$evidence$variants[].feature_blocks`): an object keyed by the join node id
# holding ordered {branch_id, column_count}, here derivative then scatter, 8 and 8.
check_graph_evidence <- function(result, bank_profile) {
  evidence <- result$owner$evidence
  declared <- function(blocks) {
    join <- blocks[[1L]]
    is.list(blocks) && length(blocks) == 1L && !is.null(names(blocks)) &&
      nzchar(names(blocks)) &&
      identical(vapply(join, `[[`, "", "branch_id"), c("derivative", "scatter")) &&
      identical(vapply(join, `[[`, integer(1), "column_count"), c(8L, 8L))
  }
  stopifnot(identical(result$owner$profile, bank_profile),
            !"selected_package" %in% names(evidence),
            length(result$variants) == length(evidence$variants),
            declared(result$selected$feature_blocks))
  for (index in seq_along(result$variants)) {
    stopifnot(declared(result$variants[[index]]$feature_blocks),
              declared(evidence$variants[[index]]$feature_blocks),
              identical(result$variants[[index]]$feature_blocks,
                        evidence$variants[[index]]$feature_blocks))
  }
  chosen <- vapply(result$variants, `[[`, "", "ordinal") == result$selected$ordinal
  stopifnot(sum(chosen) == 1L,
            identical(result$selected$feature_blocks,
                      result$variants[[which(chosen)]]$feature_blocks))
}

# 1. No splitter: training mode over all rows (as portable-branch-merge.R).
trained <- run(recipe, list(X = X, y = y))
stopifnot(identical(trained$mode, "training"),
          identical(trained$profile$name, "portable_feature_regression.v1"),
          identical(trained$profile$owner_profile, "feature_graph.v1"),
          identical(trained$evaluation, list(scope = "training", independent_test = FALSE)),
          max(abs(trained$selected$predictions - predict(nirs4all_fit(manual, X, y), X))) < 1e-12)
check_graph_evidence(trained, "training_graph_bank.v1")

# 2. Ordered Kennard-Stone holdout on raw X: owner split order and fit rows.
held_out <- recipe
held_out$pipeline <- c(list(list(class = "n4m.KennardStone",
  params = list(test_size = 0.25))), held_out$pipeline)
split <- n4m::kennard_stone_split(X, test_size = 0.25, zero_based = TRUE)
train <- split$train + 1L
test <- split$test + 1L
selection <- run(held_out, list(X = X, y = y))
stopifnot(identical(selection$mode, "single_holdout"),
          identical(selection$split$kind, "KennardStone"),
          identical(selection$split$trainIndices, as.integer(split$train)),
          identical(selection$split$testIndices, as.integer(split$test)),
          identical(selection$evaluation, list(scope = "selection_validation",
                                               independent_test = FALSE)),
          identical(selection$owner$profile, "single_resampled_holdout_graph_bank.v1"),
          max(abs(selection$targets - y[test])) < 1e-12,
          max(abs(selection$selected$predictions -
                  predict(nirs4all_fit(manual, X[train, , drop = FALSE], y[train]),
                          X[test, , drop = FALSE]))) < 1e-10)
check_graph_evidence(selection, "single_resampled_holdout_graph_bank.v1")

# 3. Branches fit on training rows only: changing validation targets or rows does
# not change anything the owner learned from the training rows.
changed_y <- y
changed_y[test] <- changed_y[test] + 1e6
leak <- run(held_out, list(X = X, y = changed_y))
stopifnot(identical(leak$split$trainIndices, selection$split$trainIndices),
          max(abs(leak$selected$predictions - selection$selected$predictions)) < 1e-10)

# 4. A selector inside a branch (SPA) keeps R's ascending column projection and
# declared branch order: the manual composition is the existing local reference.
spa_recipe <- list(pipeline = c(list(list(class = "n4m.KennardStone",
    params = list(test_size = 0.25))),
  list(list(branch = list(
    selected = list(list(class = "n4m.SPA", params = list(top_k = 4L, n_components = 2L))),
    original = list(list(class = "n4m.SNV")))),
  list(merge = "features"),
  list(model = list(class = "n4m.PLS", params = list(n_components = 2L))))))
spa_manual <- nirs4all_pipeline(list(nirs4all_concat(list(
  selected = list(nirs4all_spa(top_k = 4L, n_components = 2L)),
  original = list(nirs4all_snv())))), nirs4all_pls(2L))
spa_result <- run(spa_recipe, list(X = X, y = y))
stopifnot(identical(spa_result$split$trainIndices, as.integer(split$train)),
          max(abs(spa_result$selected$predictions -
                  predict(nirs4all_fit(spa_manual, X[train, , drop = FALSE], y[train]),
                          X[test, , drop = FALSE]))) < 1e-10)
# The owner's join widths are a promised, mandatory result of this known profile:
# declared (non-alphabetical) order selected then original, widths 4 and 8.
spa_blocks <- spa_result$selected$feature_blocks
stopifnot(!is.null(spa_blocks),
          identical(vapply(spa_blocks[[1L]], `[[`, "", "branch_id"),
                    c("selected", "original")),
          identical(vapply(spa_blocks[[1L]], `[[`, integer(1), "column_count"), c(4L, 8L)))

# 5. Anonymous array form: default branch names, same predictions as the object form.
anonymous <- recipe
anonymous$pipeline[[1L]]$branch <- unname(recipe$pipeline[[1L]]$branch)
by_array <- run(anonymous, list(X = X, y = y))
stopifnot(max(abs(by_array$selected$predictions - trained$selected$predictions)) < 1e-12)

# 6. The plan operation admits the same definition without fitting.
plan <- nirs4all_parse_execution_plan(held_out, core_cli = cli)
stopifnot(identical(plan$admission, "owner"),
          identical(plan$owner$profile, "portable_feature_regression.v1"),
          identical(plan$owner$owner_profile, "feature_graph.v1"))
cat("portable-owner-branch: ok\n")
