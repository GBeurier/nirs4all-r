# Frozen held-out Python Methods predictions, libn4m 2.6.0. The full n4m
# wrappers are used for RidgePLS/RobustPLS/CPPLS/ECR/Continuum; pls4all.sklearn
# supplies Ridge (scale_x=FALSE), SparseSIMPLS and MIRPLS. These are native
# coefficient predictions, not training-set fitted values.
library(nirs4all)

X <- outer(seq_len(21L), seq_len(8L), function(i, j)
  sin(i * j / 9) + cos(i + j / 7) + i * j / 100)
X_test <- X[c(2L, 8L, 17L), , drop = FALSE] + 0.031
y <- 1.3 + 0.7 * X[, 2L] - 0.4 * X[, 6L]
expected <- list(
  ridge = c(1.119585507454503, 2.2247502591042467, 0.8880639379404299),
  ridge_pls = c(1.0183856539647425, 2.164229001896515, 0.9228238984475714),
  robust_pls = c(1.2000108169670543, 1.5460522175157725, 0.7548630520848121),
  cppls = c(0.9908421322688001, 2.2185030364862115, 0.9364520576202219),
  sparse_simpls = c(0.9908421322687999, 2.2185030364862115, 0.9364520576202217),
  ecr = c(1.1953068770625324, 2.0055006340124004, 0.4704252881332379),
  continuum_regression = c(1.0601251478760563, 2.285195247718287, 0.919738050799327),
  mir_pls = c(1.184521178890891, 2.2497913086895096, 0.26128963080778034))
classes <- c(
  ridge = "n4m.Ridge", ridge_pls = "n4m.RidgePLS",
  robust_pls = "n4m.RobustPLS", cppls = "n4m.CPPLS",
  sparse_simpls = "n4m.SparseSIMPLS", ecr = "n4m.ECR",
  continuum_regression = "n4m.ContinuumRegression",
  mir_pls = "n4m.MIRPLS")

for (method in names(expected)) {
  original <- nirs4all_pipeline(learner = nirs4all_n4m_method(method))
  for (format in c("json", "yaml")) {
    recipe <- nirs4all_export_pipeline(original, format)
    stopifnot(identical(tail(nirs4all_portable_class_names(
      nirs4all_load_pipeline(recipe)), 1L), unname(classes[[method]])))
    imported <- nirs4all_pipeline_from_portable(recipe)
    stopifnot(identical(imported$learner$spec, original$learner$spec))
    prediction <- predict(nirs4all_fit(imported, X, y), X_test)
    stopifnot(max(abs(prediction - expected[[method]])) < 1e-10)
    result <- nirs4all_run_portable_pipeline(recipe, list(X = X, y = y))
    stopifnot(length(result$variants) == 1L,
              length(result$selected$predictions) == nrow(X))
  }
}

ridge <- list(pipeline = list(list(model = list(
  class = "n4m.Ridge", params = list(alpha = 0.5)))))
stopifnot(identical(nirs4all_pipeline_from_portable(ridge)$learner$spec$params,
                    list(ridge_lambda = 0.5)))
bad <- list(pipeline = list(list(model = list(
  class = "n4m.Ridge", params = list(ridge_lambda = 0.5)))))
stopifnot(inherits(try(nirs4all_pipeline_from_portable(bad), silent = TRUE),
                   "try-error"))
