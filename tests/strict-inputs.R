# Every product entry point that feeds n4m refuses targets, labels and group
# IDs whose length differs from the sample count: R would otherwise recycle
# them and n4m could no longer see the lost sample identity (audit F01).
library(nirs4all)

X <- outer(seq_len(24L), seq_len(10L), function(i, j) sin(i * j / 7) + i / 24 + j / 10)
n <- nrow(X)
y <- 1 + 0.6 * X[, 2L] - 0.3 * X[, 7L]
labels <- factor(rep(c("a", "b"), length.out = n))
groups <- sprintf("g%02d", rep(seq_len(8L), each = 3L))
# A divisor of n, a single value, and one value short.
wrong <- list(divisor = function(v) v[seq_len(n / 2)], scalar = function(v) v[1L],
              short = function(v) v[-1L])
refused <- function(expr, pattern, label) {
  error <- tryCatch({
    force(expr)
    NULL
  }, error = identity)
  if (!inherits(error, "error") || !grepl(pattern, conditionMessage(error), fixed = TRUE))
    stop(sprintf("%s: expected an error matching '%s', got: %s", label, pattern,
                 if (is.null(error)) "no error" else conditionMessage(error)))
}

pls <- nirs4all_pipeline(list(nirs4all_snv()), nirs4all_pls(n_components = 2L))
spa <- nirs4all_pipeline(list(nirs4all_spa(n_components = 2L, top_k = 4L)),
                         nirs4all_lm())
plsda <- nirs4all_pipeline(list(nirs4all_snv()), nirs4all_sparse_pls_da(n_components = 2L))
fitted <- nirs4all_fit(pls, X, y)
roles <- list(pipeline = list("n4m:preprocessing.scatter.snv",
  list(class = "n4m:models.pls.pls_regression", params = list(n_components = 2L))))
kfold <- nirs4all_native_splitter("spxy_group_fold", n_splits = 3L)
portable <- system.file("extdata", "portable_snv_pls.json", package = "nirs4all",
                        mustWork = TRUE)

for (case in names(wrong)) {
  cut <- wrong[[case]]
  refused(nirs4all_fit(pls, X, cut(y)), "one finite numeric value per row", case)
  refused(nirs4all_fit(spa, X, cut(y)), "one finite numeric value per row", case)
  refused(nirs4all_fit(plsda, X, cut(labels)), "at least two observed classes", case)
  refused(nirs4all_retrain(fitted, X, cut(y)), "one finite numeric value per row", case)
  refused(nirs4all_fit_role_recipe(roles, X, cut(y)), "values; X has 24 rows", case)
  refused(nirs4all_native_split(nirs4all_native_splitter("spxy", test_size = 0.25), X, cut(y)),
          "aligned to sample_ids", case)
  refused(nirs4all_native_split(kfold, X, y, group_ids = cut(groups)),
          "aligned to sample_ids", case)
  refused(nirs4all_run_portable_pipeline(portable, list(X = X, y = cut(y))),
          "row-aligned", case)
  if (requireNamespace("dagml", quietly = TRUE)) {
    refused(nirs4all_dag_cv_refit_predict(pls, X, cut(y), folds = 3L, cli = "unused"),
            "one finite numeric value per row", case)
    refused(nirs4all_dag_cv_refit_predict(pls, X, y, folds = 3L, cli = "unused",
                                          group_ids = cut(groups)),
            "aligned to sample_ids", case)
  }
}
# A target matrix is not a vector of samples.
refused(nirs4all_fit(pls, X, cbind(y, y)), "one finite numeric value per row", "matrix")
