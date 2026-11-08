# Transport/codec test of the owner-executed SparsePLS-DA classification profile
# (`portable_linear_classification.v1`) of nirs4all_run_portable_pipeline().
#
# A POSIX shell stub stands in for `nirs4all-core-archive`: it records the exact
# argument vector and request bytes and returns a canned response. This test
# inspects the class codec, serialization, dispatch, projection and error
# propagation only. It runs no fit, makes no scientific or owner-parity claim and
# does not show that the owner classification operation exists or is qualified
# (see portable-owner-classification.R for the real Core CLI profile).
library(nirs4all)

if (.Platform$OS.type == "windows") {
  cat("NOT RUN: portable-owner-classification-transport.R needs a POSIX shell stub\n")
  quit(save = "no", status = 0L)
}

capture <- tempfile("portable-class-stub-")
dir.create(capture)
stub <- file.path(capture, "nirs4all-core-archive")
writeLines(c(
  "#!/bin/sh",
  "echo \"$@\" > \"$STUB_CAPTURE/args\"",
  "while [ $# -gt 0 ]; do",
  "  case \"$1\" in --input) in=\"$2\"; shift;; --output) out=\"$2\"; shift;; esac",
  "  shift",
  "done",
  "cp \"$in\" \"$STUB_CAPTURE/request.json\"",
  "if [ -f \"$STUB_CAPTURE/exit\" ]; then",
  "  cat \"$STUB_CAPTURE/message\" >&2",
  "  exit \"$(cat \"$STUB_CAPTURE/exit\")\"",
  "fi",
  "cp \"$STUB_CAPTURE/response.json\" \"$out\""), stub)
Sys.chmod(stub, "0755")
Sys.setenv(STUB_CAPTURE = capture)
library_file <- file.path(capture, "libn4m.so")
writeLines("not a library; the stub never loads it", library_file)

reset <- function() {
  for (name in c("args", "request.json", "exit", "message"))
    if (file.exists(file.path(capture, name))) file.remove(file.path(capture, name))
}
request <- function() jsonlite::fromJSON(file.path(capture, "request.json"),
                                         simplifyVector = FALSE)
raw_request <- function() paste(readLines(file.path(capture, "request.json"),
                                          warn = FALSE), collapse = "\n")
failure <- function(expr) tryCatch({ force(expr); NULL }, error = identity)
roundtrip <- function(value) jsonlite::fromJSON(jsonlite::toJSON(value, auto_unbox = TRUE,
  null = "null", digits = I(17L)), simplifyVector = FALSE)

classification_profile <- "portable_linear_classification.v1"
write_response <- function(value)
  writeLines(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", digits = I(17L)),
             file.path(capture, "response.json"))

# Canned owner response. The owner's `selected` entry is deliberately the variant
# with the LOWER accuracy, so a host-side argmax would be visible. Predictions and
# targets are class ids; one evaluated class is reported as unseen.
unseen <- list(list(class_id = 2L, count = 1L, sample_ids = list("position_3")))
owner_response <- function(mode = "single_holdout", predictions = list(0L, 1L, 1L),
                           targets = list(0L, 1L, 2L)) {
  entry <- function(n, accuracy, ordinal, id) list(n_components = n, accuracy = accuracy,
    predictions = predictions, params = list(n_components = n, sparsity_lambda = 0.1),
    ordinal = ordinal, variant_id = id)
  training <- identical(mode, "training")
  list(schema_version = 1L, profile = classification_profile,
       owner_profile = "pipeline_steps.v1", mode = mode, name = "classifier",
       rows = 12L, cols = 5L,
       split = if (training) list(kind = "all", trainIndices = as.list(0:11),
                                  testIndices = as.list(0:11)) else
         list(kind = "KennardStone", trainIndices = list(5L, 0L, 3L, 1L, 2L, 4L, 6L, 7L, 8L),
              testIndices = list(11L, 9L, 10L)),
       preprocessing = list(list(type = "StandardNormalVariate",
                                 params = list(ddof = 1L, with_mean = TRUE, with_std = FALSE))),
       variants = list(entry(1L, 0.5, "ord:00000", "v0"), entry(2L, 0.75, "ord:00001", "v1")),
       selected = entry(1L, 0.5, "ord:00000", "v0"),
       targets = targets, unseen_validation_classes = unseen,
       evaluation = list(scope = if (training) "training" else "selection_validation",
                         independent_test = FALSE),
       identities = list(kind = "positional", sample_ids = list("position_0"),
                         training_sample_ids = list("position_5"),
                         evaluation_sample_ids = list("position_11"), axis = NULL),
       owner = list(profile = if (training) "training_bank.v2" else
                      "single_resampled_holdout_bank.v2",
                    decision = list(metric_name = "accuracy", objective = "maximize"),
                    evidence = list(unseen_validation_classes = unseen)))
}
write_response(owner_response())

X <- outer(seq_len(12L), seq_len(5L), function(i, j) sin(i * j / 5) + i / 12)
recipe <- function(splitter = TRUE) {
  steps <- list()
  if (splitter) steps[[1L]] <- list(class = "n4m.KennardStone", params = list(test_size = 0.25))
  steps[[length(steps) + 1L]] <- list(class = "n4m.SNV",
    params = list(ddof = 1L, with_mean = TRUE, with_std = FALSE))
  steps[[length(steps) + 1L]] <- list(model = list(class = "n4m.SparsePLSDA",
    params = list(sparsity_lambda = 0.1)), `_range_` = list(1L, 2L, 1L),
    param = "n_components")
  list(name = "classifier", pipeline = steps)
}
run <- function(definition, dataset)
  nirs4all_run_portable_pipeline(definition, dataset, core_cli = stub,
                                 methods_library = library_file)

# 1. Unsorted factor levels: ids follow R's own level order (virginica = 0), not
# alphabetical order; no label domain travels on the wire.
levels_unsorted <- c("virginica", "setosa", "versicolor")
y_factor <- factor(rep(levels_unsorted, 4L), levels = levels_unsorted)
reset(); result <- run(recipe(), list(X = X, y = y_factor))
args <- strsplit(readLines(file.path(capture, "args")), " ", fixed = TRUE)[[1L]]
sent <- request()
stopifnot(identical(args[[1L]], "portable-pipeline-run"), length(args) == 7L,
          identical(sent$profile, classification_profile),
          identical(names(sent$dataset), c("X", "y")),
          identical(unlist(sent$dataset$y), rep(0:2, 4L)),      # level order, integers
          !grepl("class_labels|class_values|virginica|setosa", raw_request()),
          identical(sent$definition$pipeline[[3L]]$model$params$sparsity_lambda, 0.1),
          identical(sent$definition$pipeline[[3L]]$`_range_`, list(1L, 2L, 1L)),
          identical(sent$definition$pipeline[[2L]]$params$with_std, FALSE))
# Alphabetical order would have produced different ids for the same data.
stopifnot(!identical(unlist(sent$dataset$y), as.integer(factor(as.character(y_factor))) - 1L))

# 2. Projection: owner order, owner winner, ids decoded by lookup only.
stopifnot(identical(result$profile$route, "owner"),
          identical(result$profile$name, classification_profile),
          identical(result$mode, "single_holdout"),
          identical(result$split$kind, "KennardStone"),
          identical(result$split$testIndices, c(11L, 9L, 10L)),
          identical(result$targets, c("virginica", "setosa", "versicolor")),
          length(result$variants) == 2L,
          identical(result$variants[[1L]]$predictions, c("virginica", "setosa", "setosa")),
          identical(result$variants[[2L]]$accuracy, 0.75),
          identical(result$selected$accuracy, 0.5),               # owner's winner, not argmax
          identical(result$selected$variant_id, "v0"),
          identical(result$selected$n_components, 1L),
          is.null(result$variants[[1L]]$rmse),
          identical(result$class_values, levels_unsorted),
          identical(result$evaluation, list(scope = "selection_validation",
                                            independent_test = FALSE)),
          identical(result$owner, roundtrip(owner_response()$owner)))
# The unseen-class report is the owner's, copied untouched, in both places.
stopifnot(identical(result$unseen_validation_classes, roundtrip(unseen)),
          identical(result$unseen_validation_classes,
                    result$owner$evidence$unseen_validation_classes))

# 3. Training mode: all rows evaluated, scope `training`, report still preserved.
write_response(owner_response("training", predictions = as.list(c(0L, 1L, 2L, 0L)),
                              targets = as.list(c(0L, 1L, 2L, 0L))))
reset(); training <- run(recipe(splitter = FALSE), list(X = X, y = y_factor))
stopifnot(identical(training$mode, "training"), identical(training$split$kind, "all"),
          identical(training$evaluation, list(scope = "training", independent_test = FALSE)),
          identical(training$owner$profile, "training_bank.v2"),
          identical(training$unseen_validation_classes, roundtrip(unseen)),
          identical(training$targets, c("virginica", "setosa", "versicolor", "virginica")))
write_response(owner_response())

# 4. Numeric codes keep their type and their sorted-unique order.
y_double <- c(10.5, 20.5, 30.5)[rep(1:3, 4L)]
reset(); doubles <- run(recipe(), list(X = X, y = y_double))
stopifnot(identical(unlist(request()$dataset$y), rep(0:2, 4L)),
          is.double(doubles$targets), identical(doubles$targets, c(10.5, 20.5, 30.5)),
          identical(doubles$variants[[1L]]$predictions, c(10.5, 20.5, 20.5)),
          identical(doubles$class_values, c(10.5, 20.5, 30.5)))
y_integer <- rep(c(5L, 3L, 9L), 4L)                       # sorted unique: 3, 5, 9
reset(); integers <- run(recipe(), list(X = X, y = y_integer))
stopifnot(identical(unlist(request()$dataset$y), match(y_integer, c(3L, 5L, 9L)) - 1L),
          is.integer(integers$targets), identical(integers$targets, c(3L, 5L, 9L)),
          identical(integers$class_values, c(3L, 5L, 9L)))
# Presentation keeps the legacy `unname` semantics: decoded labels are values only,
# also when the class domain carries names; the stored domain is not altered.
y_named <- stats::setNames(c(10.5, 20.5, 30.5)[rep(1:3, 4L)], sprintf("row-%02d", 1:12))
reset(); named_run <- run(recipe(), list(X = X, y = y_named))
stopifnot(is.null(names(named_run$targets)), identical(named_run$targets, c(10.5, 20.5, 30.5)),
          is.null(names(named_run$variants[[1L]]$predictions)))
named_domain <- c(a = 10.5, b = 20.5, c = 30.5)
projected <- nirs4all:::.nirs4all_portable_project(roundtrip(owner_response()),
  classification_profile, named_domain)
stopifnot(is.null(names(projected$targets)), identical(projected$targets, c(10.5, 20.5, 30.5)),
          is.null(names(projected$selected$predictions)),
          is.null(names(projected$variants[[2L]]$predictions)),
          identical(projected$class_values, named_domain))
# A character target is factored alphabetically (R's factor semantics).
y_text <- rep(c("b", "c", "a"), 4L)
reset(); text <- run(recipe(), list(X = X, y = y_text))
stopifnot(identical(unlist(request()$dataset$y), rep(c(1L, 2L, 0L), 4L)),
          identical(text$class_values, c("a", "b", "c")))

# 5. Formats-backed classification: declared IDs, axis, metadata and provenance
# travel unchanged; the categorical target is coded by R's codec.
ids <- sprintf("sample-%02d", 1:12)
metadata <- lapply(1:12, function(i) if (i == 2L) list() else
  list(batch = paste0("b", i), scans = list(1.0000000000000002, list(2, 3))))
provenance <- lapply(1:12, function(i) list(format = "csv", row = i))
formats <- structure(list(X = `rownames<-`(X, ids),
  y = `names<-`(rep(c("setosa", "versicolor", "virginica"), 4L), ids), sample_ids = ids,
  wavelengths = seq(1100, by = 2.5, length.out = 5L), axis_kind = "wavelength",
  axis_unit = "nm", signal_type = "reflectance", target_name = "species",
  metadata = metadata, formats = NULL, provenance = provenance), class = "nirs4all_dataset")
reset(); declared <- run(recipe(), formats)
sent <- request()$dataset
stopifnot(identical(names(sent), c("X", "y", "sample_ids", "axis", "metadata", "provenance")),
          identical(unlist(sent$y), rep(0:2, 4L)),
          identical(unlist(sent$sample_ids), ids),
          identical(names(sent$axis), c("kind", "unit", "signal_type", "wavelengths")),
          identical(sent$metadata, roundtrip(metadata)),
          identical(sent$provenance, roundtrip(provenance)),
          identical(declared$class_values, c("setosa", "versicolor", "virginica")),
          identical(declared$targets, c("setosa", "versicolor", "virginica")))

# 6. Original document: flags and root controls reach the owner; an edit of the
# loaded definition changes the sent flag while an unrelated control remains.
annotated <- recipe()
annotated$future_root_control <- list(flag = TRUE)
loaded <- nirs4all_load_pipeline(annotated)
reset(); run(loaded, list(X = X, y = y_factor))
stopifnot(identical(request()$definition$future_root_control, list(flag = TRUE)))
edited <- loaded
edited$pipeline[[3L]]$model$params$sparsity_lambda <- 0.3
reset(); run(edited, list(X = X, y = y_factor))
stopifnot(identical(request()$definition$pipeline[[3L]]$model$params$sparsity_lambda, 0.3),
          identical(request()$definition$future_root_control, list(flag = TRUE)))

# 7. Plan operation: classification profile, no dataset, no Methods library.
plan_response <- list(schema_version = 1L, profile = classification_profile,
  owner_profile = "pipeline_steps.v1", mode = "single_holdout", name = "classifier",
  splitter = list(type = "KennardStone", params = list(test_size = 0.25)),
  preprocessing = list(), nComponents = list(1L, 2L), recipe = list(),
  candidates = list(list(n_components = 1L, sparsity_lambda = 0.1),
                    list(n_components = 2L, sparsity_lambda = 0.1)),
  evaluation = list(scope = "selection_validation", independent_test = FALSE))
write_response(plan_response)
reset(); plan <- nirs4all_parse_execution_plan(recipe(), core_cli = stub)
args <- strsplit(readLines(file.path(capture, "args")), " ", fixed = TRUE)[[1L]]
stopifnot(identical(args[[1L]], "portable-pipeline-plan"), length(args) == 5L,
          identical(request()$profile, classification_profile),
          !"dataset" %in% names(request()),
          identical(plan$admission, "owner"), identical(plan$profile$name, classification_profile),
          identical(plan$owner, roundtrip(plan_response)))
write_response(owner_response())

# 8. opls_nipals is no longer residual: its declared algorithm and flags reach the
# owner unchanged on the regression profile (no solver substitution).
opls <- list(pipeline = list(list(class = "n4m.SNV"), list(model = list(class = "n4m.PLS",
  params = list(n_components = 2L, algo = "opls_nipals", scale_x = FALSE, center_y = TRUE)))))
regression_response <- owner_response()
regression_response$profile <- "portable_linear_regression.v1"
regression_response$variants <- lapply(regression_response$variants, function(entry) {
  entry$rmse <- entry$accuracy; entry$accuracy <- NULL
  entry$predictions <- list(1.5, 2.5, 3.5); entry })
regression_response$selected <- regression_response$variants[[1L]]
regression_response$targets <- list(1.0, 2.0, 3.0)
regression_response$unseen_validation_classes <- NULL
write_response(regression_response)
reset(); opls_result <- run(opls, list(X = X, y = seq_len(12L) + 0.5))
sent <- request()
stopifnot(identical(sent$profile, "portable_linear_regression.v1"),
          identical(sent$definition$pipeline[[2L]]$model$params$algo, "opls_nipals"),
          identical(sent$definition$pipeline[[2L]]$model$params$scale_x, FALSE),
          identical(opls_result$profile$route, "owner"),
          is.null(opls_result$unseen_validation_classes))
write_response(owner_response())

# 9. Residual classification stays residual for its other declared reasons and is
# never sent to the owner.
branched <- nirs4all_export_pipeline(nirs4all_pipeline(list(nirs4all_concat(list(
  a = list(nirs4all_snv()), b = list(nirs4all_msc())))), nirs4all_sparse_pls_da(2L)))
reset(); residual <- nirs4all_parse_execution_plan(branched, core_cli = stub)
stopifnot(identical(residual$admission, "residual_b01_local"),
          identical(unname(residual$profile$residual), "feature_branch"),
          !file.exists(file.path(capture, "args")))

# 10. Boundary refusals stay on the R side, before any native call.
reset()
for (bad in list(factor(rep(c("a", "b", "c"), 4L), ordered = TRUE), factor(rep("a", 12L)),
                 c(rep("a", 11L), NA), rep(c("a", "b", "c"), 4L)[-1L])) {
  error <- failure(run(recipe(), list(X = X, y = bad)))
  stopifnot(inherits(error, "error"), !file.exists(file.path(capture, "args")))
}

# 11. Record-backed classification is forwarded unchanged and refused by the owner
# by name; R neither parses the record nor falls back.
record <- list(schema = "nirs4all.dataset.v1", dataset = list(
  sample_ids = as.list(ids), y = list(values = as.list(rep(0:2, 4L)), dtype = "int64",
  shape = list(12L))))
backed <- structure(list(record = record, core_cli = stub), class = "nirs4all_dataset")
refusal <- paste("portable_linear_classification.v1 refuses record-backed datasets until",
                 "Core carries IO y_categorical class labels")
write(9L, file.path(capture, "exit"))
writeLines(refusal, file.path(capture, "message"))
error <- failure(nirs4all_run_portable_pipeline(recipe(), backed, methods_library = library_file))
stopifnot(inherits(error, "nirs4all_portable_owner_refusal"),
          grepl("refuses record-backed datasets", conditionMessage(error), fixed = TRUE),
          identical(request()$profile, classification_profile),
          identical(request()$dataset, roundtrip(record)))
reset()

# 12. Contract violations are errors, never completed locally.
write_response(modifyList(owner_response(), list(profile = "portable_linear_regression.v1")))
error <- failure(run(recipe(), list(X = X, y = y_factor)))
stopifnot(grepl("unsupported portable owner response", conditionMessage(error)))
write_response(owner_response(predictions = list(0L, 7L, 1L)))
error <- failure(run(recipe(), list(X = X, y = y_factor)))
stopifnot(grepl("outside the declared class domain", conditionMessage(error)))
missing_report <- owner_response()
missing_report$unseen_validation_classes <- NULL
write_response(missing_report)
error <- failure(run(recipe(), list(X = X, y = y_factor)))
stopifnot(grepl("lacks unseen_validation_classes", conditionMessage(error)))
cat("portable-owner-classification-transport: ok (transport/codec only; no parity claimed)\n")
