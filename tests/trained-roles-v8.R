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

# Class probabilities route to the native role pipeline, before and after an
# envelope round trip, with columns named by the label table (R15).
probabilities <- nirs4all_predict_proba(classifier, named(x_test))
stopifnot(is.matrix(probabilities), identical(dim(probabilities), c(nrow(x_test), 2L)),
          identical(colnames(probabilities), c("high", "low")),
          identical(unname(probabilities),
                    unname(stats::predict(classifier$role_pipeline, x_test, type = "prob"))),
          max(abs(rowSums(probabilities) - 1)) <= 1e-12,
          identical(colnames(probabilities)[max.col(probabilities, "first")],
                    as.character(nirs4all_predict(classifier, x_test))))
stopifnot(identical(nirs4all_predict_proba(replayed, named(x_test)), probabilities))
refused(nirs4all_predict_proba(replayed, named(x_test)[, permutation]), "the columns are reordered")
refused(nirs4all_predict_proba(replayed, x_test[, -1L]), "columns; the pipeline was fitted on")
refused(nirs4all_predict_proba(nirs4all_fit_role_recipe(recipe, x_train, y_train), x_test),
        "object must be a fitted classifier with probabilities")
python_classifier <- nirs4all_import_trained_pipeline(as.character(jsonlite::toJSON(
  fixture$classification$envelope, auto_unbox = TRUE, digits = NA)))
python_probabilities <- nirs4all_predict_proba(python_classifier, x_test)
stopifnot(identical(colnames(python_probabilities), python_classifier$classes),
          identical(colnames(python_probabilities)[max.col(python_probabilities, "first")],
                    fixture$classification$predict))

# The label table follows the shared contract on import (R05): a non-empty
# list of unique strings or finite numbers with a slot for every native class
# ID. It may be longer than the fitted classes (labels a filter removed).
relabelled <- function(labels) {
  changed <- document
  changed$states[[2L]]$class_names <- labels
  envelope_of(changed)
}
longer <- nirs4all_import_trained_pipeline(relabelled(list("high", "low", "mid")))
stopifnot(identical(longer$classes, c("high", "low")),
          identical(nirs4all_predict(longer, x_test), nirs4all_predict(classifier, x_test)),
          identical(nirs4all_predict_proba(longer, x_test), probabilities))
numeric_labels <- nirs4all_import_trained_pipeline(relabelled(list(1.5, 7L)))
stopifnot(identical(numeric_labels$classes, c("1.5", "7")),
          identical(colnames(nirs4all_predict_proba(numeric_labels, x_test)), c("1.5", "7")))
refused(nirs4all_import_trained_pipeline(relabelled(list())),
        "class_names must be a non-empty list of labels")
refused(nirs4all_import_trained_pipeline(relabelled("high")),
        "class_names must be a non-empty list of labels")
refused(nirs4all_import_trained_pipeline(relabelled(list("only"))),
        "the classifier has class id 1 but envelope class_names has 1 labels")
refused(nirs4all_import_trained_pipeline(relabelled(list("same", "same"))),
        "envelope class_names repeat the label 'same'")
refused(nirs4all_import_trained_pipeline(relabelled(list(1L, "1"))),
        "envelope class_names mixes strings and numbers")
refused(nirs4all_import_trained_pipeline(relabelled(list("high", 2))),
        "envelope class_names mixes strings and numbers")
refused(nirs4all_import_trained_pipeline(relabelled(list("high", NULL))),
        "envelope class_names entry 2 is not a string or a finite number")
refused(nirs4all_import_trained_pipeline(relabelled(list("high", TRUE))),
        "envelope class_names entry 2 is not a string or a finite number")
refused(nirs4all_import_trained_pipeline(relabelled(list(list("high"), "low"))),
        "envelope class_names entry 1 is not a string or a finite number")
regression_document <- jsonlite::fromJSON(
  nirs4all_export_trained_pipeline(nirs4all_fit_role_recipe(recipe, x_train, y_train)),
  simplifyVector = FALSE)
regression_document$states[[length(regression_document$states)]]$class_names <- list("a")
refused(nirs4all_import_trained_pipeline(envelope_of(regression_document)),
        "does not end with a classifier")

# Numeric labels are the native class IDs (no label table); a table must then
# cover those IDs, 10 and 20, not merely count them.
by_id <- nirs4all_fit_role_recipe(fixture$classification$envelope$recipe, x_train,
                                  ifelse(labels == "high", 10, 20))
id_document <- jsonlite::fromJSON(nirs4all_export_trained_pipeline(by_id), simplifyVector = FALSE)
stopifnot(is.null(id_document$states[[2L]]$class_names), identical(by_id$classes, c("10", "20")),
          identical(colnames(nirs4all_predict_proba(by_id, x_test)), c("10", "20")))
id_document$states[[2L]]$class_names <- list("high", "low")
refused(nirs4all_import_trained_pipeline(envelope_of(id_document)),
        "the classifier has class id 10 but envelope class_names has 2 labels")
id_document$states[[2L]]$class_names <- as.list(sprintf("c%02d", 0:20))
covered <- nirs4all_import_trained_pipeline(envelope_of(id_document))
stopifnot(identical(covered$classes, c("c10", "c20")),
          identical(as.character(nirs4all_predict(covered, x_test)),
                    sprintf("c%s", as.character(nirs4all_predict(by_id, x_test)))),
          identical(colnames(nirs4all_predict_proba(covered, x_test)), c("c10", "c20")))
# The exporter re-checks the table it writes.
stopifnot(identical(unlist(jsonlite::fromJSON(nirs4all_export_trained_pipeline(covered),
                                              simplifyVector = FALSE)$states[[2L]]$class_names),
                    sprintf("c%02d", 0:20)))

# n_features is a positive JSON integer compared exactly, never truncated (R16).
text <- envelope_of(document)
stopifnot(grepl(sprintf("\"n_features\":%d,", ncol(x_train)), text, fixed = TRUE))
width_as <- function(value)
  sub(sprintf("\"n_features\":%d,", ncol(x_train)), sprintf("\"n_features\":%s,", value),
      text, fixed = TRUE)
for (value in c(sprintf("%d.9", ncol(x_train)), sprintf("%d.0", ncol(x_train)),
                sprintf("%de0", ncol(x_train)), sprintf("\"%d\"", ncol(x_train)),
                "true", "null", "0", "-1", "3000000000"))
  refused(nirs4all_import_trained_pipeline(width_as(value)),
          "a v8 envelope needs n_features as a positive JSON integer")
stopifnot(identical(nirs4all_import_trained_pipeline(width_as(ncol(x_train)))$n_features,
                    ncol(x_train)))

# Numeric labels stay numbers, at full precision, through fit -> export ->
# import -> predict -> export (shared label contract).
exact_of <- function(document)
  as.character(jsonlite::toJSON(document, auto_unbox = TRUE, null = "null", digits = I(17L)))
class_recipe <- fixture$classification$envelope$recipe
class_names_of <- function(envelope) {
  document <- jsonlite::fromJSON(envelope, simplifyVector = FALSE)
  document$states[[length(document$states)]]$class_names
}
for (levels in list(c(0.5, 1.5), c(1 / 3, 2 / 3))) {
  numeric_fit <- nirs4all_fit_role_recipe(class_recipe, x_train,
                                          ifelse(labels == "high", levels[1L], levels[2L]))
  envelope <- nirs4all_export_trained_pipeline(numeric_fit)
  written <- class_names_of(envelope)
  stopifnot(all(vapply(written, is.double, logical(1))), identical(unlist(written), levels),
            !grepl(sprintf("\"%s\"", levels[1L]), envelope, fixed = TRUE))
  replayed <- nirs4all_import_trained_pipeline(envelope)
  stopifnot(identical(n4m::n4m_role_pipeline_info(replayed$role_pipeline)$label_names, levels),
            identical(replayed$classes, as.character(levels)),
            identical(nirs4all_predict(replayed, x_test), nirs4all_predict(numeric_fit, x_test)),
            identical(colnames(nirs4all_predict_proba(replayed, x_test)), as.character(levels)),
            identical(class_names_of(nirs4all_export_trained_pipeline(replayed)), written))
}
stopifnot(identical(class_names_of(
  "{\"states\": [{\"class_names\": [0.5, 1.5]}]}"), list(0.5, 1.5)))

# Uniqueness is by value: two close fractions (equal to 15 significant digits)
# are two labels, on export and on import.
close <- c(0.1234567890123456, 0.1234567890123457)
close_fit <- nirs4all_fit_role_recipe(class_recipe, x_train,
                                      ifelse(labels == "high", close[1L], close[2L]))
envelope <- nirs4all_export_trained_pipeline(close_fit)
stopifnot(identical(unlist(class_names_of(envelope)), close))
stopifnot(identical(n4m::n4m_role_pipeline_info(
  nirs4all_import_trained_pipeline(envelope)$role_pipeline)$label_names, close))
close_document <- document
close_document$states[[2L]]$class_names <- as.list(close)
stopifnot(identical(n4m::n4m_role_pipeline_info(nirs4all_import_trained_pipeline(
  exact_of(close_document))$role_pipeline)$label_names, close))
refused(nirs4all_import_trained_pipeline(relabelled(list(0.25, 0.25))),
        "envelope class_names repeat the label '0.25'")

# An integer label beyond 2^53 is refused, never rounded: in the JSON text
# (jsonlite would read 2^53 + 1 as 2^53) and as a value.
with_table <- function(table)
  sub("\"class_names\":[\"high\",\"low\"]", sprintf("\"class_names\":[%s]", table),
      envelope_of(document), fixed = TRUE)
stopifnot(grepl("\"class_names\":[\"high\",\"low\"]", envelope_of(document), fixed = TRUE))
for (table in c("9007199254740993,1", "1,-9007199254740993", "0.5,18014398509481984"))
  refused(nirs4all_import_trained_pipeline(with_table(table)),
          "is an integer beyond 2^53, not exactly representable")
beyond_document <- document
beyond_document$states[[2L]]$class_names <- list(1, 2^53 + 2)
refused(nirs4all_import_trained_pipeline(exact_of(beyond_document)),
        "entry 2 (9007199254740994) is an integer beyond 2^53")
at_limit <- nirs4all_import_trained_pipeline(with_table("9007199254740992,-9007199254740992"))
stopifnot(identical(n4m::n4m_role_pipeline_info(at_limit$role_pipeline)$label_names,
                    c(2^53, -2^53)))
beyond_fit <- nirs4all_fit_role_recipe(class_recipe, x_train,
                                       ifelse(labels == "high", 0.5, 2^53 + 2))
refused(nirs4all_export_trained_pipeline(beyond_fit), "is an integer beyond 2^53")

# jsonlite reads "a\u0000x" as "a": an envelope holding the escape \u0000 (or a
# NUL byte in its file) is refused before parsing, including in a nested
# manifest; an escaped backslash followed by "u0000" is ordinary text.
named_classifier <- nirs4all_export_trained_pipeline(classifier)
nul_text <- sub(sprintf("\"%s\"", names_train[1L]), "\"a\\u0000x\"", named_classifier, fixed = TRUE)
stopifnot(!identical(nul_text, named_classifier))
refused(nirs4all_import_trained_pipeline(nul_text), "contains the JSON escape \\u0000 (NUL)")
refused(nirs4all_import_trained_pipeline(sub("\\u0000", "\\\\\\u0000", nul_text, fixed = TRUE)),
        "contains the JSON escape \\u0000 (NUL)")
literal <- nirs4all_import_trained_pipeline(sub("\\u0000", "\\\\u0000", nul_text, fixed = TRUE))
stopifnot(identical(literal$feature_names[1L], "a\\u0000x"))
path <- tempfile(fileext = ".json")
writeBin(c(charToRaw(sub("\"high\"", "\"hi", named_classifier, fixed = TRUE)), as.raw(0L)), path)
refused(nirs4all_import_trained_pipeline(path), "contains a NUL byte")
unlink(path)
manifest <- "{\"recipe\": \"a\\u0000x\"}"
refused(nirs4all_import_trained_pipeline(as.character(jsonlite::toJSON(list(
  schema = "nirs4all.n4m.trained_pipeline.v4", manifest_json = manifest,
  manifest_sha256 = digest::digest(manifest, algo = "sha256", serialize = FALSE),
  model = list()), auto_unbox = TRUE))), "contains the JSON escape \\u0000 (NUL)")
