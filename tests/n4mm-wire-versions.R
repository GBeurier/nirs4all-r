library(nirs4all)

fixture <- jsonlite::fromJSON(system.file("extdata", "n4mm_legacy_v1_v2.json",
                                        package = "nirs4all", mustWork = TRUE))
X <- fixture$X
y <- fixture$y
recipes <- list(
  plain = nirs4all_pipeline(learner = nirs4all_pls(2L)),
  embedded = nirs4all_pipeline(list(nirs4all_snv(), nirs4all_savgol(5L)),
                              nirs4all_pls(2L)))
must_fail <- function(expression) {
  stopifnot(inherits(tryCatch(force(expression), error = identity), "error"))
}
for (name in names(recipes)) {
  recipe <- recipes[[name]]
  legacy <- fixture[[name]]
  bytes <- jsonlite::base64_dec(legacy$payload)
  stopifnot(identical(n4m::n4m_model_descriptor(bytes)$format_version,
                      as.integer(legacy$format_version)))
  restored <- nirs4all_import_native_model(bytes, recipe)
  # Re-export preserves the wire family and predictions; writer ABI provenance
  # may change when a legacy payload is imported by a newer native library.
  reexported <- nirs4all_export_native_model(restored)
  stopifnot(max(abs(predict(restored, X) - legacy$predictions)) < 1e-12,
            identical(n4m::n4m_model_descriptor(reexported)$format_version,
                      as.integer(legacy$format_version)))
  # Legacy payloads remain readable inside trained envelopes as well.
  transferred <- nirs4all_import_trained_pipeline(
    nirs4all_export_trained_pipeline(restored))
  stopifnot(max(abs(predict(transferred, X) - legacy$predictions)) < 1e-12)

  fitted <- nirs4all_fit(recipe, X, y)
  current <- nirs4all_export_native_model(fitted)
  if (utils::packageVersion("n4m") >= "1.3.7")
    stopifnot(identical(n4m::n4m_model_descriptor(current)$format_version, 3L))
  native <- nirs4all_import_native_model(current, recipe)
  trained <- nirs4all_import_trained_pipeline(
    nirs4all_export_trained_pipeline(fitted))
  stopifnot(max(abs(predict(native, X) - predict(fitted, X))) < 1e-12,
            max(abs(predict(trained, X) - predict(fitted, X))) < 1e-12)

  wrong <- nirs4all_pipeline(recipe$steps, nirs4all_pls(1L))
  must_fail(nirs4all_import_native_model(current, wrong))
  incompatible <- fitted
  incompatible$learner <- nirs4all_pls(1L)
  must_fail(nirs4all_export_trained_pipeline(incompatible))
  opposite <- recipes[[if (name == "plain") "embedded" else "plain"]]
  must_fail(nirs4all_import_native_model(current, opposite))
  must_fail(nirs4all_import_native_model(current, recipe, letters[1:3]))
  corrupted <- current
  corrupted[[length(corrupted)]] <- as.raw(bitwXor(
    as.integer(corrupted[[length(corrupted)]]), 1L))
  must_fail(nirs4all_import_native_model(corrupted, recipe))
  future <- current
  future[5:8] <- writeBin(4L, raw(), size = 4L, endian = "little")
  must_fail(nirs4all_import_native_model(future, recipe))
}
# Reject unknown and ownership-incompatible formats at the R gate too.
gate <- getFromNamespace("nirs4all_pls_wire_version_matches", "nirs4all")
stopifnot(!gate(4L, FALSE), !gate(4L, TRUE), !gate(2L, FALSE),
          !gate(1L, TRUE), !gate(NA_integer_, FALSE), !gate(3, FALSE))
message("Native/trained PLS format 1/2/3 compatibility and mismatch guards passed")
