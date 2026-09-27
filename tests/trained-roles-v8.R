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

# The pipeline is n4m's native role pipeline; the product keeps the envelope.
refused <- function(expr, pattern) {
  error <- tryCatch({
    force(expr)
    NULL
  }, error = identity)
  if (!inherits(error, "error") || !grepl(pattern, conditionMessage(error), fixed = TRUE))
    stop(sprintf("expected an error matching '%s', got: %s", pattern,
                 if (is.null(error)) "no error" else conditionMessage(error)))
}
envelope_of <- function(document)
  as.character(jsonlite::toJSON(document, auto_unbox = TRUE, null = "null", digits = NA))
y_train <- fixture$regression$y_train
n <- nrow(x_train)
names_train <- sprintf("nm%04d", seq_len(ncol(x_train)))
named <- function(X, names = names_train) {
  colnames(X) <- names
  X
}

# Old v8 documents (no feature_names, no contains_training_rows) still load:
# the Python fixture above has neither. R now writes both.
document <- jsonlite::fromJSON(envelope, simplifyVector = FALSE)
stopifnot(is.null(document$feature_names),
          all(vapply(document$states, function(s) identical(s$contains_training_rows, FALSE),
                     logical(1))))

# Column identity: names are stored at fit, exported, checked at predict.
fitted <- nirs4all_fit_role_recipe(recipe, named(x_train), y_train)
stopifnot(identical(fitted$feature_names, names_train),
          identical(nirs4all_predict(fitted, named(x_test)), nirs4all_predict(fitted, x_test)))
permutation <- c(2L, 1L, seq.int(3L, ncol(x_test)))
refused(nirs4all_predict(fitted, named(x_test)[, permutation]), "the columns are reordered")
refused(nirs4all_predict(fitted, named(x_test, rev(names_train))), "input column")
refused(nirs4all_predict(fitted, x_test[, -1L]), "columns; the pipeline was fitted on")
envelope <- nirs4all_export_trained_pipeline(fitted)
document <- jsonlite::fromJSON(envelope, simplifyVector = FALSE)
stopifnot(identical(unlist(document$feature_names), names_train))
replayed <- nirs4all_import_trained_pipeline(envelope)
stopifnot(identical(nirs4all_predict(replayed, named(x_test)), nirs4all_predict(fitted, x_test)))
refused(nirs4all_predict(replayed, named(x_test)[, permutation]), "the columns are reordered")
short_names <- document
short_names$feature_names <- short_names$feature_names[-1L]
refused(nirs4all_import_trained_pipeline(envelope_of(short_names)), "feature_names")

# A recipe that contradicts its states is refused (F05), as is a state whose
# envelope label or training-row flag misdescribes it.
contradiction <- document
contradiction$recipe$pipeline[[4L]]$params$n_components <- 2L
refused(nirs4all_import_trained_pipeline(envelope_of(contradiction)),
        "parameter 'n_components' is 3 in the state but 2 in the recipe")
dropped <- document
dropped$recipe$pipeline[[2L]] <- NULL
refused(nirs4all_import_trained_pipeline(envelope_of(dropped)), "stateful steps but 3 states")
mislabelled <- document
mislabelled$states[[1L]]$method_id <- "preprocessing.scatter.msc"
refused(nirs4all_import_trained_pipeline(envelope_of(mislabelled)), "is labelled")
misflagged <- document
misflagged$states[[3L]]$contains_training_rows <- TRUE
refused(nirs4all_import_trained_pipeline(envelope_of(misflagged)), "misreports its training rows")
wider <- document
wider$n_features <- wider$n_features + 1L
wider$feature_names <- NULL
refused(nirs4all_import_trained_pipeline(envelope_of(wider)), "n_features differs")

# Empty recipes are refused at fit and at import.
refused(nirs4all_fit_role_recipe(list(pipeline = list()), x_train, y_train), "at least one step")
empty <- document
empty$recipe$pipeline <- list()
refused(nirs4all_import_trained_pipeline(envelope_of(empty)), "at least one step")
refused(nirs4all_fit_role_recipe(list(pipeline = list("n4m:preprocessing.scatter.snv")),
                                 x_train, y_train), "ends with one regressor or classifier")

# Targets are never recycled (F01): y must have one value (or row) per sample.
refused(nirs4all_fit_role_recipe(recipe, x_train, y_train[seq_len(n / 3)]),
        sprintf("y has %d values; X has %d rows", n / 3, n))
refused(nirs4all_fit_role_recipe(recipe, x_train, 7), sprintf("1 values; X has %d rows", n))
refused(nirs4all_fit_role_recipe(recipe, x_train, cbind(y_train, y_train)[-1L, ]),
        sprintf("y has %d rows; X has %d", n - 1L, n))
labels <- fixture$classification$y_train
refused(nirs4all_fit_role_recipe(fixture$classification$envelope$recipe, x_train,
                                 labels[-1L]), sprintf("vector of %d values", n))
refused(nirs4all_retrain(replayed, named(x_train), y_train[-1L]),
        sprintf("y has %d values; X has %d rows", n - 1L, n))

# Several responses reach every supervised step (PLS -> PLS, F06).
Y <- cbind(y_train, rev(y_train) + 0.5 * x_train[, 3L])
multi <- list(pipeline = list(
  list(class = "n4m:models.pls.pls_regression", params = list(n_components = 3L)),
  list(class = "n4m:models.pls.pls_regression", params = list(n_components = 2L))))
fitted <- nirs4all_fit_role_recipe(multi, x_train, Y)
predicted <- nirs4all_predict(fitted, x_test)
stopifnot(is.matrix(predicted), identical(dim(predicted), c(nrow(x_test), 2L)))
first <- n4m::n4m_estimator_fit(n4m::n4m_pls_regression(n_components = 3L), x_train, Y)
scores <- n4m::n4m_estimator_transform(first, x_train)
second <- n4m::n4m_estimator_fit(n4m::n4m_pls_regression(n_components = 2L), scores, Y)
stopifnot(max(abs(predicted - predict(second, n4m::n4m_estimator_transform(first, x_test)))) <= 1e-12)
replayed <- nirs4all_import_trained_pipeline(nirs4all_export_trained_pipeline(fitted))
stopifnot(identical(nirs4all_predict(replayed, x_test), predicted))
stopifnot(max(abs(nirs4all_predict(nirs4all_retrain(replayed, x_train, Y), x_test) -
                  predicted)) <= 1e-12)

# States that keep training rows are exported only on request (F10).
kernel <- list(pipeline = list("n4m:preprocessing.scatter.snv", "n4m:models.pls.kernel"))
fitted <- nirs4all_fit_role_recipe(kernel, named(x_train), y_train)
refused(nirs4all_export_trained_pipeline(fitted), "allow_training_rows = TRUE")
refused(nirs4all_export_trained_pipeline(fitted, allow_training_rows = NA),
        "allow_training_rows must be TRUE or FALSE")
envelope <- nirs4all_export_trained_pipeline(fitted, allow_training_rows = TRUE)
document <- jsonlite::fromJSON(envelope, simplifyVector = FALSE)
stopifnot(identical(vapply(document$states, function(s) s$contains_training_rows, logical(1)),
                    c(FALSE, TRUE)))
replayed <- nirs4all_import_trained_pipeline(envelope)
stopifnot(identical(nirs4all_predict(replayed, named(x_test)), nirs4all_predict(fitted, x_test)))
misflagged <- document
misflagged$states[[2L]]$contains_training_rows <- FALSE
refused(nirs4all_import_trained_pipeline(envelope_of(misflagged)), "misreports its training rows")

# Classifier label names travel with the terminal state; RDS bundles keep working.
classifier <- nirs4all_fit_role_recipe(fixture$classification$envelope$recipe,
                                       named(x_train), labels)
document <- jsonlite::fromJSON(nirs4all_export_trained_pipeline(classifier),
                               simplifyVector = FALSE)
stopifnot(identical(unlist(document$states[[2L]]$class_names), c("high", "low")),
          identical(classifier$classes, c("high", "low")))
replayed <- nirs4all_import_trained_pipeline(envelope_of(document))
stopifnot(identical(nirs4all_predict(replayed, named(x_test)), nirs4all_predict(classifier, x_test)))
path <- tempfile(fileext = ".rds")
nirs4all_save(classifier, path)
stopifnot(identical(nirs4all_predict(nirs4all_load(path), x_test),
                    nirs4all_predict(classifier, x_test)))
unlink(path)
wrong_classes <- document
wrong_classes$states[[2L]]$class_names <- list("high", "low", "mid")
refused(nirs4all_import_trained_pipeline(envelope_of(wrong_classes)), "class_names")
