library(nirs4all)

X <- matrix(seq_len(120) / 37, nrow = 12L, ncol = 10L)
X <- X + outer(seq_len(12L), seq_len(10L), function(i, j) sin(i * j / 7))
y <- 2 + 0.7 * X[, 2L] - 0.3 * X[, 7L]

pipeline <- nirs4all_pipeline(list(nirs4all_snv(), nirs4all_savgol(5L)),
                             nirs4all_pls(n_components = 2L))
fitted <- nirs4all_fit(pipeline, X, y)
expected <- n4m::n4m_predict(fitted$state,
  n4m::savgol_transform(n4m::snv_transform(X), 5L, 2L, mode = "interp"))
stopifnot(isTRUE(all.equal(nirs4all_predict(fitted, X), as.numeric(expected),
                           tolerance = 1e-12)))
path <- tempfile(fileext = ".rds")
nirs4all_save(fitted, path)
reloaded <- nirs4all_load(path)
stopifnot(isTRUE(all.equal(predict(reloaded, X), predict(fitted, X),
                           tolerance = 1e-12)))
unlink(path)

preprocess_cases <- list(
  snv_flags = list(step = nirs4all_snv(with_mean = FALSE),
                   reference = n4m::snv_transform(X, with_mean = FALSE)),
  local_snv = list(step = nirs4all_local_snv(window = 5L),
                   reference = n4m::local_snv_transform(X, window = 5L)),
  robust_snv = list(step = nirs4all_robust_snv(),
                    reference = n4m::robust_snv_transform(X)),
  area = list(step = nirs4all_area_normalization("trapz"),
              reference = n4m::area_normalization_transform(X, "trapz")),
  detrend = list(step = nirs4all_detrend(2L),
                 reference = n4m::detrend_transform(X, 2L)))
for (case in preprocess_cases) {
  candidate <- nirs4all_pipeline(list(case$step), nirs4all_pls(2L))
  fit <- nirs4all_fit(candidate, X, y)
  direct <- n4m::n4m_fit(case$reference, y, algo = "pls_simpls",
                         n_components = 2L)
  stopifnot(max(abs(predict(fit, X) -
                    as.numeric(n4m::n4m_predict(direct, case$reference)))) < 1e-10)
  saved <- tempfile(fileext = ".rds")
  nirs4all_save(fit, saved)
  stopifnot(max(abs(predict(nirs4all_load(saved), X) - predict(fit, X))) < 1e-10)
  unlink(saved)
}

lm_pipeline <- nirs4all_pipeline(learner = nirs4all_lm())
lm_fit <- nirs4all_fit(lm_pipeline, X[, c(2L, 7L), drop = FALSE], y)
stopifnot(max(abs(nirs4all_predict(lm_fit, X[, c(2L, 7L), drop = FALSE]) - y)) < 1e-10)

for (method in c("ridge", "ridge_pls", "robust_pls", "cppls",
                 "sparse_simpls", "ecr", "continuum_regression", "mir_pls")) {
  params <- if (identical(method, "ridge")) list(ridge_lambda = 0.5) else list()
  learner <- nirs4all_n4m_method(method, n_components = 2L, params = params)
  method_fit <- nirs4all_fit(nirs4all_pipeline(learner = learner), X, y)
  reference <- n4m::n4m_method(method, X, y, 2L, params = params)
  stopifnot(max(abs(predict(method_fit, X) -
                    as.numeric(reference$predictions))) < 1e-10)
  method_path <- tempfile(fileext = ".rds")
  nirs4all_save(method_fit, method_path)
  stopifnot(isTRUE(all.equal(predict(nirs4all_load(method_path), X),
                             predict(method_fit, X), tolerance = 1e-12)))
  unlink(method_path)
}
bad <- tryCatch(nirs4all_n4m_method("kernel_pls"), error = identity)
stopifnot(inherits(bad, "error"), grepl("unsupported", conditionMessage(bad)))
bad <- tryCatch(nirs4all_n4m_method("ridge", params = list(lamda = 1)),
                error = identity)
stopifnot(inherits(bad, "error"), grepl("params", conditionMessage(bad)))

bad <- tryCatch(nirs4all_predict(fitted, X[, -1L, drop = FALSE]), error = identity)
stopifnot(inherits(bad, "error"), grepl("expected", conditionMessage(bad)))
named_X <- X
colnames(named_X) <- paste0("wl", seq_len(ncol(X)))
named_fit <- nirs4all_fit(pipeline, named_X, y)
bad <- tryCatch(predict(named_fit, named_X[, rev(seq_len(ncol(X))), drop = FALSE]),
                error = identity)
stopifnot(inherits(bad, "error"), grepl("feature names", conditionMessage(bad)))
rownames(named_X) <- paste0("sample", seq_len(nrow(named_X)))
names(y) <- rev(rownames(named_X))
bad <- tryCatch(nirs4all_fit(pipeline, named_X, y), error = identity)
stopifnot(inherits(bad, "error"), grepl("sample names", conditionMessage(bad)))
names(y) <- NULL
bad <- tryCatch(nirs4all_fit(pipeline, X, y[-1L]), error = identity)
stopifnot(inherits(bad, "error"), grepl("one finite", conditionMessage(bad)))
bad <- tryCatch(nirs4all_snv("1"), error = identity)
stopifnot(inherits(bad, "error"), grepl("ddof", conditionMessage(bad)))
bad <- tryCatch(nirs4all_snv(2147483648), error = identity)
stopifnot(inherits(bad, "error"), grepl("ddof", conditionMessage(bad)))
stopifnot(inherits(try(nirs4all_local_snv(4L), silent = TRUE), "try-error"),
          inherits(try(nirs4all_robust_snv(k = 0), silent = TRUE), "try-error"),
          inherits(try(nirs4all_area_normalization("bad"), silent = TRUE), "try-error"),
          inherits(try(nirs4all_detrend(-1L), silent = TRUE), "try-error"))
bad <- tryCatch(nirs4all_pls(2147483648), error = identity)
stopifnot(inherits(bad, "error"), grepl("n_components", conditionMessage(bad)))
bad <- tryCatch(nirs4all_fit(nirs4all_pipeline(learner = nirs4all_lm()),
                            cbind(X[, 1L], X[, 1L]), y), error = identity)
stopifnot(inherits(bad, "error"), grepl("rank-deficient", conditionMessage(bad)))

if (requireNamespace("ranger", quietly = TRUE)) {
  forest_pipeline <- nirs4all_pipeline(learner = nirs4all_ranger(num.trees = 25L,
                                                               seed = 123L,
                                                               num.threads = 1L))
  forest_a <- nirs4all_fit(forest_pipeline, X, y)
  forest_b <- nirs4all_fit(forest_pipeline, X, y)
  pa <- predict(forest_a, X)
  pb <- predict(forest_b, X)
  stopifnot(length(pa) == nrow(X), all(is.finite(pa)), identical(pa, pb))
  forest_path <- tempfile(fileext = ".rds")
  nirs4all_save(forest_a, forest_path)
  stopifnot(identical(predict(nirs4all_load(forest_path), X), pa))
  unlink(forest_path)
}

if (requireNamespace("glmnet", quietly = TRUE)) {
  elastic <- nirs4all_pipeline(learner = nirs4all_glmnet(lambda = 0.01,
                                                        alpha = 0.5))
  elastic_fit <- nirs4all_fit(elastic, X, y)
  actual <- predict(elastic_fit, X)
  reference <- as.numeric(stats::predict(elastic_fit$state$model, newx = X,
                                          s = 0.01))
  stopifnot(isTRUE(all.equal(actual, reference, tolerance = 1e-12)),
            isTRUE(any(abs(elastic_fit$state$model$lambda - 0.01) < 1e-12)))
  elastic_path <- tempfile(fileext = ".rds")
  nirs4all_save(elastic_fit, elastic_path)
  stopifnot(isTRUE(all.equal(predict(nirs4all_load(elastic_path), X), actual,
                             tolerance = 1e-12)))
  unlink(elastic_path)
  bad <- tryCatch(nirs4all_glmnet(lambda = 0), error = identity)
  stopifnot(inherits(bad, "error"), grepl("lambda", conditionMessage(bad)))
}

if (requireNamespace("torch", quietly = TRUE) && torch::torch_is_installed()) {
  mlp <- nirs4all_pipeline(learner = nirs4all_torch_mlp(hidden = 8L,
                                                      epochs = 40L,
                                                      learning_rate = 0.01,
                                                      seed = 42L))
  mlp_a <- nirs4all_fit(mlp, X, y)
  mlp_b <- nirs4all_fit(mlp, X, y)
  pa <- predict(mlp_a, X)
  pb <- predict(mlp_b, X)
  stopifnot(length(pa) == nrow(X), all(is.finite(pa)),
            isTRUE(all.equal(pa, pb, tolerance = 1e-6)),
            sqrt(mean((pa - y)^2)) < 0.5 * stats::sd(y))
  mlp_path <- tempfile(fileext = ".rds")
  nirs4all_save(mlp_a, mlp_path)
  stopifnot(isTRUE(all.equal(predict(nirs4all_load(mlp_path), X), pa,
                             tolerance = 1e-6)))
  unlink(mlp_path)
}
