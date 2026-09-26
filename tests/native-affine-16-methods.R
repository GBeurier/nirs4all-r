library(nirs4all)

X <- outer(seq_len(21L), seq_len(12L), function(i, j)
  sin(i * j / 9) + cos(i + j / 7) + i * j / 100)
colnames(X) <- paste0("wl", seq_len(ncol(X)))
y <- 1.3 + 0.7 * X[, 2L] - 0.4 * X[, 6L]
held <- X[c(2L, 8L, 17L), , drop = FALSE] + 0.031
special <- list(
  n_pls = list(mode_j = 3L, mode_k = 4L),
  mb_pls = list(block_sizes = c(6L, 6L)),
  di_pls = list(X_target = X + 0.02),
  group_sparse_pls = list(group_assignment =
    stats::setNames(rep(0:2, each = 4L), colnames(X))))
qualified <- n4m::n4m_affine_supported_methods()
stopifnot(length(qualified) == 16L)
for (method in qualified) {
  params <- special[[method]]
  if (is.null(params)) params <- list()
  pipeline <- nirs4all_pipeline(learner =
    nirs4all_n4m_method(method, 2L, params))
  fitted <- nirs4all_fit(pipeline, X, y)
  predicted <- predict(fitted, held)
  bytes <- nirs4all_export_native_model(fitted)
  restored <- nirs4all_import_native_model(bytes, pipeline, colnames(X))
  stopifnot(length(predicted) == nrow(held),
    all(is.finite(predicted)), is.raw(bytes), length(bytes) > 0L,
    max(abs(predicted - predict(restored, held))) < 1e-10)
}
message("all 16 qualified n4m controllers predict via portable native N4MM")
