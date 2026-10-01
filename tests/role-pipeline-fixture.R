# The shared n4m role-pipeline fixture (nirs4all-methods
# parity/fixtures/role_pipeline_negative.json, written by the Python binding
# with ABI 2.14; copied to tests/fixtures) replayed through the product: its
# fitted pipelines wrapped in v8 envelopes, and its negative cases refused by
# the product entry points with the native messages.
library(nirs4all)

rp <- jsonlite::fromJSON(file.path("fixtures", "n4m_role_pipeline_negative.json"),
                         simplifyVector = TRUE, simplifyDataFrame = FALSE)
stopifnot(identical(rp$abi, "2.14.0"))
named <- function(X, names = rp$feature_names) {
  colnames(X) <- names
  X
}
# Kernels may drift by a few ulps off Linux x86-64.
tolerance <- 1e-9

state_entry <- function(state) {
  bytes <- jsonlite::base64_dec(state$n4me_base64)
  c(state[c("method_id", "n4me_base64")],
    list(sha256 = digest::digest(bytes, algo = "sha256", serialize = FALSE)),
    state["contains_training_rows"])
}
envelope <- function(steps, states, feature_names = rp$feature_names, class_names = NULL) {
  states <- lapply(states, state_entry)
  if (!is.null(class_names)) states[[length(states)]]$class_names <- as.list(class_names)
  document <- list(schema = "nirs4all.n4m.trained_pipeline.v8",
                   recipe = list(pipeline = steps), n_features = length(rp$feature_names),
                   feature_names = as.list(feature_names), states = states)
  as.character(jsonlite::toJSON(document, auto_unbox = TRUE, null = "null", digits = NA))
}
refused <- function(expr, pattern, label) {
  error <- tryCatch({
    force(expr)
    NULL
  }, error = identity)
  if (!inherits(error, "error") || !grepl(pattern, conditionMessage(error), fixed = TRUE))
    stop(sprintf("%s: expected an error matching '%s', got: %s", label, pattern,
                 if (is.null(error)) "no error" else conditionMessage(error)))
}

# N4ME stores its writer ABI (Methods docs/abi/estimator_roles_design.md D6;
# cpp/src/core/estimator/n4me.cpp::encode_state). Embedded N4MM does likewise
# (cpp/src/c_api/c_api_model.cpp::write_model_to_buffer). These are the three fixed
# historical regression states, not a general state parser. Across writer ABIs
# every byte except those ABI fields and their native checksums must stay exact.
fixture_abi <- writeBin(c(2L, 14L, 0L), raw(), size = 4L, endian = "little")
writer_abi <- writeBin(as.integer(n4m::n4m_abi_version()), raw(), size = 4L,
                       endian = "little")
stopifnot(length(writer_abi) == 12L)
fixture_layout <- list(
  list(method_id = "preprocessing.scatter.snv", size = 202L, n4mm_start = NULL),
  list(method_id = "models.pls.pls_regression", size = 1677L, n4mm_start = 286L),
  list(method_id = "models.regularized.ridge", size = 504L, n4mm_start = 217L))
check_fixture_bytes <- function(original, current, layout) {
  stopifnot(identical(original$method_id, layout$method_id),
            identical(current$method_id, layout$method_id))
  before <- jsonlite::base64_dec(original$n4me_base64)
  after <- jsonlite::base64_dec(current$n4me_base64)
  n <- layout$size
  n4me_header <- c(charToRaw("N4ME"), writeBin(1L, raw(), size = 4L,
                                             endian = "little"))
  stopifnot(length(before) == n, length(after) == n,
            identical(before[1:8], n4me_header), identical(after[1:8], n4me_header),
            identical(before[9:20], fixture_abi), identical(after[9:20], writer_abi))
  allowed <- c(9:20, seq.int(n - 7L, n))
  if (!is.null(layout$n4mm_start)) {
    start <- layout$n4mm_start
    n4mm_header <- c(charToRaw("N4MM"), writeBin(1L, raw(), size = 4L,
                                               endian = "little"))
    header <- seq.int(start, start + 7L)
    abi <- seq.int(start + 8L, start + 19L)
    stopifnot(start > 20L, start + 19L < n - 15L,
              identical(before[header], n4mm_header), identical(after[header], n4mm_header),
              identical(before[abi], fixture_abi), identical(after[abi], writer_abi))
    allowed <- c(allowed, abi, seq.int(n - 15L, n - 8L))
  }
  stopifnot(identical(before[-allowed], after[-allowed]))
  if (identical(writer_abi, fixture_abi)) stopifnot(identical(before, after))
}

# The fitted pipelines replay, preserve their model bytes and retrain identically.
reg <- rp$regression
fitted <- nirs4all_import_trained_pipeline(envelope(reg$steps, reg$states))
stopifnot(identical(fitted$feature_names, rp$feature_names),
          max(abs(nirs4all_predict(fitted, named(rp$x_test)) - reg$predict)) <= tolerance)
exported_text <- nirs4all_export_trained_pipeline(fitted)
exported <- jsonlite::fromJSON(exported_text, simplifyVector = FALSE)
stopifnot(length(reg$states) == 3L, length(exported$states) == 3L,
          identical(unlist(exported$feature_names), rp$feature_names))
for (i in seq_along(fixture_layout))
  check_fixture_bytes(reg$states[[i]], exported$states[[i]], fixture_layout[[i]])
# Native import validates both checksums; a second export must be byte exact
# under the current writer ABI, including the checksums and scientific output.
reimported <- nirs4all_import_trained_pipeline(exported_text)
second <- jsonlite::fromJSON(nirs4all_export_trained_pipeline(reimported), simplifyVector = FALSE)
stopifnot(identical(vapply(second$states, `[[`, "", "n4me_base64"),
                    vapply(exported$states, `[[`, "", "n4me_base64")),
          max(abs(nirs4all_predict(reimported, named(rp$x_test)) - reg$predict)) <= tolerance)
refit <- nirs4all_retrain(fitted, named(rp$x_train), rp$y_train)
stopifnot(max(abs(nirs4all_predict(refit, named(rp$x_test)) - reg$predict)) <= tolerance)

cls <- rp$classification
classifier <- nirs4all_import_trained_pipeline(
  envelope(cls$steps, cls$states, class_names = cls$class_names))
stopifnot(identical(classifier$classes, cls$class_names),
          identical(as.character(nirs4all_predict(classifier, named(rp$x_test))), cls$predict))
refit <- nirs4all_retrain(classifier, named(rp$x_train), rp$labels_train)
stopifnot(identical(as.character(nirs4all_predict(refit, rp$x_test)), cls$predict))

# The negative cases are refused through the product entry points.
replayed <- character()
for (case in rp$cases) {
  label <- case$name
  switch(case$stage,
    create = refused(nirs4all_fit_role_recipe(list(pipeline = case$steps), rp$x_train,
                                              rp$y_train), case$message, label),
    import = {
      states <- lapply(seq_along(case$states), function(k)
        list(method_id = sprintf("state %d", k), n4me_base64 = case$states[[k]]))
      refused(nirs4all_import_trained_pipeline(envelope(case$steps, states)),
              case$message, label)
    },
    predict = {
      names <- if (isTRUE(case$drop_last_column)) rp$feature_names[-12L] else
        case$feature_names
      refused(nirs4all_predict(fitted, named(rp$x_test[, seq_along(names)], names)),
              case$message, label)
    },
    export = {
      kernel <- nirs4all_fit_role_recipe(list(pipeline = case$steps), rp$x_train,
                                         rp[[case$y]])
      refused(nirs4all_export_trained_pipeline(kernel), "allow_training_rows = TRUE", label)
      refused(n4m::n4m_role_pipeline_export(kernel$role_pipeline), case$message, label)
      shared <- nirs4all_import_trained_pipeline(
        nirs4all_export_trained_pipeline(kernel, allow_training_rows = TRUE))
      stopifnot(identical(nirs4all_predict(shared, rp$x_test),
                          nirs4all_predict(kernel, rp$x_test)))
    },
    fit = {
      multi <- nirs4all_fit_role_recipe(list(pipeline = case$steps), rp$x_train, rp[[case$y]])
      stopifnot(max(abs(nirs4all_predict(multi, rp$x_test) - case$predict)) <= tolerance)
    },
    stop(sprintf("unknown fixture stage %s", case$stage)))
  replayed <- c(replayed, label)
}
stopifnot(length(replayed) == 10L, !anyDuplicated(replayed))
