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
# Kernels may drift by a few ulps off Linux x86-64 (the exported bytes do not).
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

# The fitted pipelines replay, re-export the same bytes and retrain identically.
reg <- rp$regression
fitted <- nirs4all_import_trained_pipeline(envelope(reg$steps, reg$states))
stopifnot(identical(fitted$feature_names, rp$feature_names),
          max(abs(nirs4all_predict(fitted, named(rp$x_test)) - reg$predict)) <= tolerance)
exported <- jsonlite::fromJSON(nirs4all_export_trained_pipeline(fitted), simplifyVector = FALSE)
stopifnot(identical(vapply(exported$states, `[[`, "", "n4me_base64"),
                    vapply(reg$states, `[[`, "", "n4me_base64")),
          identical(unlist(exported$feature_names), rp$feature_names))
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
