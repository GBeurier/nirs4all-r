# Generic n4m role recipes and trained envelope v8: R fit/export/import, and
# Python-trained envelopes replayed identically in R (L2 portability).
library(nirs4all)

fixture <- jsonlite::fromJSON(system.file("extdata", "python_trained_roles_v8.json",
                                          package = "nirs4all", mustWork = TRUE),
                              simplifyVector = TRUE)
x_train <- fixture$x_train
x_test <- fixture$x_test

# Python-trained pipelines predict identically in R.
for (case in c("regression", "classification")) {
  document <- jsonlite::toJSON(fixture[[case]]$envelope, auto_unbox = TRUE, digits = NA)
  imported <- nirs4all_import_trained_pipeline(as.character(document))
  predicted <- nirs4all_predict(imported, x_test)
  if (identical(case, "regression")) {
    stopifnot(max(abs(predicted - fixture[[case]]$predict)) <= 1e-12)
  } else {
    stopifnot(identical(as.character(predicted), fixture[[case]]$predict))
  }
  # Retraining the imported recipe in R reproduces the Python fit.
  refit <- nirs4all_retrain(imported, x_train, fixture[[case]]$y_train)
  again <- nirs4all_predict(refit, x_test)
  if (identical(case, "regression")) {
    stopifnot(max(abs(again - fixture[[case]]$predict)) <= 1e-9)
  } else {
    stopifnot(identical(as.character(again), fixture[[case]]$predict))
  }
}

# An R fit exports an envelope that R re-imports identically.
recipe <- fixture$regression$envelope$recipe
fitted <- nirs4all_fit_role_recipe(recipe, x_train, fixture$regression$y_train)
envelope <- nirs4all_export_trained_pipeline(fitted)
stopifnot(grepl("nirs4all.n4m.trained_pipeline.v8", envelope, fixed = TRUE))
replayed <- nirs4all_import_trained_pipeline(envelope)
stopifnot(identical(nirs4all_predict(replayed, x_test), nirs4all_predict(fitted, x_test)))

# Tampered states and foreign steps are refused.
document <- jsonlite::fromJSON(envelope, simplifyVector = FALSE)
document$states[[1]]$sha256 <- strrep("0", 64)
tampered <- tryCatch(nirs4all_import_trained_pipeline(
  as.character(jsonlite::toJSON(document, auto_unbox = TRUE, digits = NA))), error = identity)
stopifnot(inherits(tampered, "error"), grepl("checksum", conditionMessage(tampered)))
foreign <- tryCatch(nirs4all_fit_role_recipe(list(pipeline = list("n4m.SNV")), x_train,
                                             fixture$regression$y_train), error = identity)
stopifnot(inherits(foreign, "error"))
