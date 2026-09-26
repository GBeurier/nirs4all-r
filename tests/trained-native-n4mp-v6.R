library(nirs4all)

X <- outer(seq_len(24L), seq_len(17L), function(s, b)
  sin(s * b / 9) + cos(s + b / 7) + s * b / 100)
colnames(X) <- paste0("wl", seq_len(ncol(X)))
y <- 1.3 + 0.7 * X[, 2L] - 0.4 * X[, 6L]
held <- X[c(2L, 8L, 17L), , drop = FALSE] + 0.031
emit <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE,
  null = "null", digits = 17L))
reject <- function(x) stopifnot(inherits(try(x, silent = TRUE), "try-error"))

# Qualification is against the existing R n4m step path on both training
# and held-out rows, before enabling a semantic mapping in v6.
profiles <- list(
  list(nirs4all_snv()),
  list(nirs4all_msc()),
  list(nirs4all_detrend(2L)),
  list(nirs4all_savgol(5L, 2L)),
  list(nirs4all_savgol(7L, 2L, 1L)),
  list(nirs4all_snv(), nirs4all_msc(),
       nirs4all_savgol(7L, 2L, 1L)))
for (steps in profiles) {
  old <- nirs4all:::nirs4all_fit_transform(X, steps)
  stopifnot(identical(colnames(old$X), colnames(X)))
  native <- n4m::n4m_preprocess_fit(X,
    nirs4all:::nirs4all_n4mp_steps(steps))
  stopifnot(max(abs(old$X - n4m::n4m_preprocess_transform(native, X))) < 1e-10,
    max(abs(nirs4all:::nirs4all_transform(held, steps, old$states) -
      n4m::n4m_preprocess_transform(native, held))) < 1e-10)
}

# Sweep the bounded polynomial and SG parameter space over a wider spectral
# matrix, checking held-out values rather than only the fixture defaults.
wide <- outer(seq_len(32L), seq_len(31L), function(s, b)
  sin(s * b / 11) + cos(b / 3 + s / 7) + s * b / 200)
colnames(wide) <- paste0("band", seq_len(ncol(wide)))
wide_held <- wide[c(3L, 10L, 29L), , drop = FALSE] + 0.023
audit_step <- function(step) {
  old <- nirs4all:::nirs4all_fit_transform(wide, list(step))
  native <- n4m::n4m_preprocess_fit(wide,
    nirs4all:::nirs4all_n4mp_steps(list(step)))
  stopifnot(identical(colnames(old$X), colnames(wide)),
    max(abs(old$X - n4m::n4m_preprocess_transform(native, wide))) < 1e-10,
    max(abs(nirs4all:::nirs4all_transform(wide_held, list(step), old$states) -
      n4m::n4m_preprocess_transform(native, wide_held))) < 1e-10)
}
for (degree in 0:5) audit_step(nirs4all_detrend(degree))
for (window in c(3L, 5L, 7L, 11L, 15L))
  for (poly in 0:min(5L, window - 1L))
    for (deriv in 0:min(2L, poly))
      audit_step(nirs4all_savgol(window, poly, deriv))

# Named per-feature groups must remain aligned after every width-preserving
# step. Both the legacy and N4MP routes now retain input feature identity.
groups <- stats::setNames(rep(0:2, length.out = ncol(X)), colnames(X))
group_steps <- list(nirs4all_snv(), nirs4all_msc(),
                    nirs4all_savgol(5L, 2L))
group_pipeline <- nirs4all_pipeline(group_steps,
  nirs4all_n4m_method("group_sparse_pls", 2L,
    list(group_assignment = groups)))
legacy_groups <- nirs4all_fit(group_pipeline, X, y)
native_groups <- nirs4all_fit(group_pipeline, X, y,
                              preprocessing = "native_n4mp")
stopifnot(max(abs(predict(legacy_groups, held) -
                  predict(native_groups, held))) < 1e-10)
bad_groups <- groups[rev(seq_along(groups))]
bad_pipeline <- nirs4all_pipeline(group_steps,
  nirs4all_n4m_method("group_sparse_pls", 2L,
    list(group_assignment = bad_groups)))
reject(nirs4all_fit(bad_pipeline, X, y))
reject(nirs4all_fit(bad_pipeline, X, y,
                    preprocessing = "native_n4mp"))

# Generic N4MP EMSC has an intercept and a normalized polynomial axis;
# the legacy n4m EMSC uses powers of integer wavelength without intercept.
old_emsc <- nirs4all:::nirs4all_fit_transform(X, list(nirs4all_emsc(2L)))
native_emsc <- n4m::n4m_preprocess_fit(X,
  list(n4m::n4m_preprocess_step("emsc", 2)))
stopifnot(max(abs(old_emsc$X -
  n4m::n4m_preprocess_transform(native_emsc, X))) > 1e-3)
reject(nirs4all:::nirs4all_n4mp_steps(list(nirs4all_emsc(2L))))

for (learner in list(nirs4all_pls(n_components = 2L),
                     nirs4all_n4m_method("ridge_pls", 2L))) {
  pipeline <- nirs4all_pipeline(list(nirs4all_snv(), nirs4all_msc(),
    nirs4all_savgol(7L, 2L, 1L)), learner)
  fitted <- nirs4all_fit(pipeline, X, y, preprocessing = "native_n4mp")
  legacy <- nirs4all_fit(pipeline, X, y)
  expected <- predict(fitted, held)
  stopifnot(max(abs(expected - predict(legacy, held))) < 1e-8)
  text <- nirs4all_export_trained_pipeline(fitted)
  doc <- jsonlite::fromJSON(text, simplifyVector = FALSE)
  stopifnot(identical(doc$schema, "nirs4all.n4m.trained_pipeline.v6"),
    identical(doc$preprocessing$encoding, "base64-n4mp"),
    identical(doc$model$encoding, "base64-n4mm"))
  imported <- nirs4all_import_trained_pipeline(text)
  stopifnot(max(abs(predict(imported, held) - expected)) < 1e-10,
    max(abs(predict(nirs4all_retrain(imported, X, y), held) - expected)) < 1e-8)
  path <- tempfile(fileext = ".rds")
  nirs4all_save(imported, path)
  stopifnot(max(abs(predict(nirs4all_load(path), held) - expected)) < 1e-10)
  unlink(path)
  reject(predict(imported, held[, rev(seq_len(ncol(held))), drop = FALSE]))
  reject(predict(imported, unname(held)))
  reject(predict(imported, held[, -1L, drop = FALSE]))

  bad <- doc
  bad$preprocessing$sha256 <- paste0("0", substring(bad$preprocessing$sha256, 2L))
  reject(nirs4all_import_trained_pipeline(emit(bad)))
  bad <- doc
  bad$model$sha256 <- paste0("0", substring(bad$model$sha256, 2L))
  reject(nirs4all_import_trained_pipeline(emit(bad)))
  bad <- doc
  bad$manifest_sha256 <- paste0("0", substring(bad$manifest_sha256, 2L))
  reject(nirs4all_import_trained_pipeline(emit(bad)))
  bad <- doc
  manifest <- jsonlite::fromJSON(bad$manifest_json, simplifyVector = FALSE)
  manifest$recipe$pipeline[[1L]]$params$ddof <- 1L
  bad$manifest_json <- emit(manifest)
  bad$manifest_sha256 <- digest::digest(bad$manifest_json,
    algo = "sha256", serialize = FALSE)
  reject(nirs4all_import_trained_pipeline(emit(bad)))
  bad <- doc
  manifest <- jsonlite::fromJSON(bad$manifest_json, simplifyVector = FALSE)
  manifest$input_n_features <- 18L
  bad$manifest_json <- emit(manifest)
  bad$manifest_sha256 <- digest::digest(bad$manifest_json,
    algo = "sha256", serialize = FALSE)
  reject(nirs4all_import_trained_pipeline(emit(bad)))
  bad <- doc
  manifest <- jsonlite::fromJSON(bad$manifest_json, simplifyVector = FALSE)
  manifest$preprocessing_owner <- "external"
  bad$manifest_json <- emit(manifest)
  bad$manifest_sha256 <- digest::digest(bad$manifest_json,
    algo = "sha256", serialize = FALSE)
  reject(nirs4all_import_trained_pipeline(emit(bad)))
  bad <- doc
  manifest <- jsonlite::fromJSON(bad$manifest_json, simplifyVector = FALSE)
  manifest$step_states <- list(list(reference = 1))
  bad$manifest_json <- emit(manifest)
  bad$manifest_sha256 <- digest::digest(bad$manifest_json,
    algo = "sha256", serialize = FALSE)
  reject(nirs4all_import_trained_pipeline(emit(bad)))
  bad <- doc
  manifest <- jsonlite::fromJSON(bad$manifest_json, simplifyVector = FALSE)
  manifest$recipe$pipeline[c(1L, 2L)] <-
    manifest$recipe$pipeline[c(2L, 1L)]
  bad$manifest_json <- emit(manifest)
  bad$manifest_sha256 <- digest::digest(bad$manifest_json,
    algo = "sha256", serialize = FALSE)
  reject(nirs4all_import_trained_pipeline(emit(bad)))
  bad <- doc
  bad$unexpected <- TRUE
  reject(nirs4all_import_trained_pipeline(emit(bad)))
  bad <- doc
  reordered <- n4m::n4m_preprocess_fit(X, list(
    n4m::n4m_preprocess_step("msc"), n4m::n4m_preprocess_step("snv"),
    n4m::n4m_preprocess_step("savgol_derivative", c(7, 2, 1, 1))))
  raw <- n4m::n4m_preprocess_export(reordered)
  bad$preprocessing$payload <- jsonlite::base64_enc(raw)
  bad$preprocessing$sha256 <- digest::digest(raw, algo = "sha256",
    serialize = FALSE)
  reject(nirs4all_import_trained_pipeline(emit(bad)))
  bad <- doc
  raw <- jsonlite::base64_dec(bad$preprocessing$payload)
  raw[[1L]] <- as.raw(0L)
  bad$preprocessing$payload <- jsonlite::base64_enc(raw)
  bad$preprocessing$sha256 <- digest::digest(raw, algo = "sha256",
    serialize = FALSE)
  reject(nirs4all_import_trained_pipeline(emit(bad)))
}

for (step in list(nirs4all_emsc(), nirs4all_local_snv(),
                  nirs4all_robust_snv(), nirs4all_area_normalization(),
                  nirs4all_snv(ddof = 1L),
                  nirs4all_savgol(5L, 2L, mode = "mirror"),
                  nirs4all_savgol(5L, 2L, delta = 2),
                  nirs4all_concat(list(a = list(nirs4all_snv()),
                                        b = list(nirs4all_msc())))))
  reject(nirs4all_fit(nirs4all_pipeline(list(step),
    nirs4all_pls(n_components = 2L)), X, y,
    preprocessing = "native_n4mp"))

python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON")
helper <- if (file.exists("helpers/trained_n4mp_v6_peer.py"))
  "helpers/trained_n4mp_v6_peer.py" else
  "tests/helpers/trained_n4mp_v6_peer.py"
if (nzchar(python) && nzchar(Sys.getenv("N4M_LIB_PATH"))) {
  stopifnot(file.exists(python), file.exists(helper))
  envelope <- tempfile(fileext = ".json")
  oracle <- tempfile(fileext = ".json")
  nirs4all_export_trained_pipeline(fitted, envelope)
  rows <- function(value) lapply(seq_len(nrow(value)), function(i)
    unname(as.list(as.numeric(value[i, ]))))
  writeLines(emit(list(train = rows(X), heldout = rows(held),
    feature_names = unname(as.list(colnames(X))),
    y = unname(as.list(y)),
    predictions = unname(as.list(as.numeric(predict(fitted, held)))))),
    oracle, useBytes = TRUE)
  result <- suppressWarnings(system2(python,
    c(shQuote(helper), shQuote(envelope), shQuote(oracle)),
    stdout = TRUE, stderr = TRUE))
  status <- attr(result, "status")
  if (!is.null(status) && status != 0L)
    stop("Python N4MP/N4MM peer failed: ", paste(result, collapse = "\n"))
  unlink(c(envelope, oracle))
}
