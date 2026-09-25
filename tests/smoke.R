library(nirs4all)

X <- matrix(seq_len(120) / 37, nrow = 12L, ncol = 10L)
X <- X + outer(seq_len(12L), seq_len(10L), function(i, j) sin(i * j / 7))
y <- 2 + 0.7 * X[, 2L] - 0.3 * X[, 7L]

pipeline <- nirs4all_pipeline(list(nirs4all_snv(), nirs4all_savgol(5L)),
                             nirs4all_pls(n_components = 2L))
fitted <- nirs4all_fit(pipeline, X, y)
transformed <- n4m::savgol_transform(n4m::snv_transform(X),
                                     5L, 2L, mode = "interp")
plain_model <- n4m::n4m_fit(transformed, y, algo = "pls_simpls",
                            n_components = 2L)
expected <- n4m::n4m_predict(plain_model, transformed)
stopifnot(identical(fitted$preprocessing_owner, "embedded_methods"))
stopifnot(isTRUE(all.equal(nirs4all_predict(fitted, X), as.numeric(expected),
                           tolerance = 1e-12)))
path <- tempfile(fileext = ".rds")
nirs4all_save(fitted, path)
reloaded <- nirs4all_load(path)
stopifnot(isTRUE(all.equal(predict(reloaded, X), predict(fitted, X),
                           tolerance = 1e-12)))
unlink(path)

native_bytes <- nirs4all_export_native_model(fitted)
native_info <- n4m::n4m_model_pipeline_info(native_bytes)
stopifnot(identical(native_info$semantic_profile, 1L),
          identical(native_info$window_length, 5L),
          identical(native_info$polyorder, 2L),
          identical(native_info$raw_n_features, ncol(X)))
native_import <- nirs4all_import_native_model(native_bytes, pipeline)
stopifnot(max(abs(predict(native_import, X) - predict(fitted, X))) < 1e-12)
recipe <- nirs4all_export_pipeline(pipeline, "yaml")
native_import_recipe <- nirs4all_import_native_model(native_bytes, recipe)
stopifnot(max(abs(predict(native_import_recipe, X) - predict(fitted, X))) < 1e-12)
wrong_recipe <- nirs4all_pipeline(list(nirs4all_snv(), nirs4all_savgol(7L)),
                                  nirs4all_pls(2L))
stopifnot(inherits(try(nirs4all_import_native_model(native_bytes, wrong_recipe),
                     silent = TRUE), "try-error"))
wrong_components <- n4m::n4m_fit(X, y, "pls_simpls", 1L,
  embedded_snv_savgol = c(5, 2))
stopifnot(inherits(try(nirs4all_import_native_model(
  n4m::n4m_model_export(wrong_components), pipeline), silent = TRUE),
  "try-error"))
stopifnot(inherits(try(nirs4all_import_native_model(
  native_bytes, pipeline, feature_names = letters[1:9]), silent = TRUE),
  "try-error"))
external_fit <- nirs4all_fit(
  nirs4all_pipeline(list(nirs4all_snv(with_mean = FALSE)), nirs4all_pls(2L)),
  X, y)
stopifnot(inherits(try(nirs4all_export_native_model(external_fit), silent = TRUE),
                   "try-error"))

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

msc_train <- X[1:8, , drop = FALSE]
msc_test <- X[9:12, , drop = FALSE]
msc_pipeline <- nirs4all_pipeline(list(nirs4all_msc()), nirs4all_pls(2L))
msc_fitted <- nirs4all_fit(msc_pipeline, msc_train, y[1:8])
msc_reference <- n4m::msc_fit(msc_train)
stopifnot(length(msc_fitted$step_states[[1L]]) == ncol(X),
          isTRUE(all.equal(msc_fitted$step_states[[1L]], msc_reference,
                           tolerance = 1e-12)))
msc_direct <- n4m::n4m_fit(n4m::msc_transform(msc_train, msc_reference), y[1:8],
                           algo = "pls_simpls", n_components = 2L)
msc_expected <- as.numeric(n4m::n4m_predict(
  msc_direct, n4m::msc_transform(msc_test, msc_reference)))
stopifnot(max(abs(predict(msc_fitted, msc_test) - msc_expected)) < 1e-10)
msc_path <- tempfile(fileext = ".rds")
nirs4all_save(msc_fitted, msc_path)
stopifnot(max(abs(predict(nirs4all_load(msc_path), msc_test) - msc_expected)) < 1e-10)
unlink(msc_path)

emsc_pipeline <- nirs4all_pipeline(list(nirs4all_emsc(2L)), nirs4all_pls(2L))
emsc_fitted <- nirs4all_fit(emsc_pipeline, msc_train, y[1:8])
emsc_reference <- n4m::emsc_fit(msc_train, 2L)
stopifnot(isTRUE(all.equal(emsc_fitted$step_states[[1L]], emsc_reference,
                           tolerance = 1e-12)))
emsc_direct <- n4m::n4m_fit(n4m::emsc_transform(msc_train, emsc_reference, 2L),
                            y[1:8], algo = "pls_simpls", n_components = 2L)
emsc_expected <- as.numeric(n4m::n4m_predict(
  emsc_direct, n4m::emsc_transform(msc_test, emsc_reference, 2L)))
stopifnot(max(abs(predict(emsc_fitted, msc_test) - emsc_expected)) < 1e-10)
emsc_path <- tempfile(fileext = ".rds")
nirs4all_save(emsc_fitted, emsc_path)
stopifnot(max(abs(predict(nirs4all_load(emsc_path), msc_test) - emsc_expected)) < 1e-10)
unlink(emsc_path)

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
