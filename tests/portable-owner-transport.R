# Transport-only test of the owner route of nirs4all_run_portable_pipeline().
#
# A POSIX shell stub stands in for `nirs4all-core-archive`. It records the
# exact argument vector and request bytes and returns a canned response. This
# test inspects serialization, dispatch, projection and error propagation only;
# it makes no scientific or owner-parity claim (see portable-owner-linear.R for
# the real Core CLI profile, which needs NIRS4ALL_CORE_CLI and N4M_LIB_PATH).
library(nirs4all)

if (.Platform$OS.type == "windows") {
  cat("NOT RUN: portable-owner-transport.R needs a POSIX shell stub\n")
  quit(save = "no", status = 0L)
}

capture <- tempfile("portable-stub-")
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
  "  echo 'portable-pipeline-run: unsupported step at pipeline[1]' >&2",
  "  exit \"$(cat \"$STUB_CAPTURE/exit\")\"",
  "fi",
  "cp \"$STUB_CAPTURE/response.json\" \"$out\""), stub)
Sys.chmod(stub, "0755")
Sys.setenv(STUB_CAPTURE = capture)
library_file <- file.path(capture, "libn4m.so")
writeLines("not a library; the stub never loads it", library_file)

reset <- function() {
  for (name in c("args", "request.json", "exit"))
    if (file.exists(file.path(capture, name))) file.remove(file.path(capture, name))
}
request <- function() jsonlite::fromJSON(file.path(capture, "request.json"),
                                         simplifyVector = FALSE)
failure <- function(expr) tryCatch({ force(expr); NULL }, error = identity)

# The canned response deliberately selects the variant with the LARGER rmse,
# so a host-side argmin would be visible as a different `selected`.
response <- list(
  schema_version = 1L, profile = "portable_linear_regression.v1",
  owner_profile = "pipeline_steps.v1", mode = "single_holdout", name = "stub",
  rows = 6L, cols = 3L,
  split = list(kind = "KennardStone", trainIndices = list(5L, 0L, 3L, 1L),
               testIndices = list(4L, 2L)),
  preprocessing = list(list(type = "StandardNormalVariate",
                            params = list(ddof = 0L, with_mean = TRUE, with_std = FALSE))),
  variants = list(
    list(n_components = 1L, rmse = 0.5, predictions = list(1.5, 2.5),
         params = list(n_components = 1L), ordinal = "ord:00000", variant_id = "v0"),
    list(n_components = 2L, rmse = 0.25, predictions = list(1.25, 2.25),
         params = list(n_components = 2L), ordinal = "ord:00001", variant_id = "v1")),
  selected = list(n_components = 1L, rmse = 0.5, predictions = list(1.5, 2.5),
                  params = list(n_components = 1L), ordinal = "ord:00000",
                  variant_id = "v0"),
  targets = list(1.0, 2.0),
  evaluation = list(scope = "selection_validation", independent_test = FALSE),
  identities = list(kind = "positional", sample_ids = list("position:0"),
                    training_sample_ids = list("position:5"),
                    evaluation_sample_ids = list("position:4"), axis = NULL),
  owner = list(profile = "single_resampled_holdout_bank.v1",
               decision = list(), evidence = list()))
write_response <- function(value = response)
  writeLines(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", digits = I(17L)),
             file.path(capture, "response.json"))
write_response()

X <- outer(seq_len(6L), seq_len(3L), function(i, j) sin(i * j / 3) + i / 6)
y <- c(1, 2, 3, 4, 5, 6) + 0.1234567890123
linear <- list(name = "snv_pls", description = "kept", random_state = 11L,
  future_root_control = list(flag = TRUE),
  pipeline = list(
    list(`_comment` = "kept verbatim"),
    list(class = "n4m.KennardStone", params = list(test_size = 0.25)),
    list(class = "n4m.SNV", params = list(with_std = FALSE)),
    list(model = list(class = "n4m.PLS", params = list(n_components = 2L)))))

# 1. Plain list: argument vector, complete original document, positional IDs.
result <- nirs4all_run_portable_pipeline(linear, list(X = X, y = y),
  core_cli = stub, methods_library = library_file)
args <- strsplit(readLines(file.path(capture, "args")), " ", fixed = TRUE)[[1L]]
stopifnot(identical(args[[1L]], "portable-pipeline-run"),
          identical(args[c(2L, 4L, 6L)], c("--input", "--output", "--methods-library")),
          identical(args[[7L]], normalizePath(library_file)), length(args) == 7L)
sent <- request()
stopifnot(identical(sent$schema_version, 1L),
          identical(sent$profile, "portable_linear_regression.v1"),
          !"mode" %in% names(sent), !"source_id" %in% names(sent),
          identical(names(sent$definition),
                    c("name", "description", "random_state", "future_root_control",
                      "pipeline")),
          identical(sent$definition$pipeline[[1L]], list(`_comment` = "kept verbatim")),
          identical(sent$definition$future_root_control, list(flag = TRUE)),
          identical(sent$definition$random_state, 11L),
          identical(sent$definition$pipeline[[3L]]$params$with_std, FALSE),
          identical(sent$dataset$y[[1L]], y[[1L]]),
          length(sent$dataset$X) == 6L, length(sent$dataset$X[[1L]]) == 3L,
          identical(sent$dataset$X[[2L]][[3L]], X[2L, 3L]),
          !"sample_ids" %in% names(sent$dataset), !"axis" %in% names(sent$dataset))

# 2. Projection only: the owner's winner, order and types are passed through.
stopifnot(identical(result$split$kind, "KennardStone"),
          identical(result$split$trainIndices, c(5L, 0L, 3L, 1L)),
          identical(result$split$testIndices, c(4L, 2L)),
          identical(result$rows, 6L), identical(result$cols, 3L),
          length(result$variants) == 2L,
          identical(result$selected$n_components, 1L), result$selected$rmse == 0.5,
          identical(result$selected$variant_id, "v0"),
          identical(result$variants[[2L]]$ordinal, "ord:00001"),
          identical(result$targets, c(1, 2)),
          identical(result$evaluation,
                    list(scope = "selection_validation", independent_test = FALSE)),
          identical(result$mode, "single_holdout"),
          identical(result$profile$route, "owner"),
          identical(result$owner$profile, "single_resampled_holdout_bank.v1"))

# 3. YAML with a one-element sequence keeps array shape; JSON text round-trips.
yaml_text <- paste("pipeline:", "  - class: n4m.SNV", "  - model:",
  "      class: n4m.PLS", "      params:", "        n_components: 2", sep = "\n")
reset(); nirs4all_run_portable_pipeline(yaml_text, list(X = X, y = y),
  core_cli = stub, methods_library = library_file)
sent <- request()
stopifnot(is.list(sent$definition$pipeline), length(sent$definition$pipeline) == 2L)
reset(); nirs4all_run_portable_pipeline(jsonlite::toJSON(
  list(pipeline = list(list(class = "n4m.SNV", params = structure(list(), names = character())),
       list(model = list(class = "n4m.PLS", params = list(n_components = 2L))))),
  auto_unbox = TRUE), list(X = X, y = y), core_cli = stub, methods_library = library_file)
stopifnot(identical(request()$definition$pipeline[[1L]]$params, structure(list(),
  names = character())))

# 3b. Explicit JSON/YAML type information is preserved: `params: []` stays an array
# and `params: {}` stays an object in the bytes sent to the owner.
raw_request <- function() paste(readLines(file.path(capture, "request.json"), warn = FALSE),
                                collapse = "\n")
array_text <- '{"pipeline":[{"class":"n4m.SNV","params":[]},{"model":{"class":"n4m.PLS","params":{}}}]}'
reset(); nirs4all_run_portable_pipeline(array_text, list(X = X, y = y), core_cli = stub,
                                        methods_library = library_file)
stopifnot(grepl('"class":"n4m.SNV","params":[]', raw_request(), fixed = TRUE),
          grepl('"class":"n4m.PLS","params":{}', raw_request(), fixed = TRUE))
reset(); nirs4all_run_portable_pipeline(paste("pipeline:", "  - class: n4m.SNV",
  "    params: {}", "  - model:", "      class: n4m.PLS", "      params: []", sep = "\n"),
  list(X = X, y = y), core_cli = stub, methods_library = library_file)
stopifnot(grepl('"class":"n4m.SNV","params":{}', raw_request(), fixed = TRUE))

# 3c. A loaded definition keeps the complete original document: unedited it sends
# the same bytes as the raw source (comments, unknown and root controls included);
# edits to its visible fields reach the owner and unrelated controls remain.
raw_recipe <- paste0('{"name":"loaded","future_root_control":{"flag":true},',
  '"_comment":"root comment","random_state":7,"pipeline":[',
  '{"_comment":"step comment"},',
  '{"class":"n4m.KennardStone","params":{"test_size":0.25}},',
  '{"class":"n4m.SNV","params":[]},',
  '{"model":{"class":"n4m.PLS","params":{"n_components":2}}}]}')
send <- function(source) {
  reset()
  nirs4all_run_portable_pipeline(source, list(X = X, y = y), core_cli = stub,
                                 methods_library = library_file)
  raw_request()
}
raw_bytes <- send(raw_recipe)
loaded <- nirs4all_load_pipeline(raw_recipe)
stopifnot(identical(names(loaded)[1:4], c("name", "description", "random_state", "pipeline")),
          length(loaded$pipeline) == 3L,                 # visible shape: comments stripped
          identical(send(loaded), raw_bytes),            # original still reaches the owner
          identical(send(nirs4all_load_pipeline(loaded)), raw_bytes))
sent <- request()$definition
stopifnot(identical(sent$future_root_control, list(flag = TRUE)),
          identical(sent$`_comment`, "root comment"),
          identical(sent$pipeline[[1L]], list(`_comment` = "step comment")),
          identical(sent$pipeline[[3L]]$params, list()))
edited <- loaded
snv_at <- which(vapply(edited$pipeline, function(step) identical(step$class, "n4m.SNV"),
                       logical(1)))
edited$pipeline[[snv_at]]$params$with_std <- FALSE
edited$pipeline[[length(edited$pipeline)]]$model$params$scale_x <- FALSE
send(edited)
sent <- request()$definition
stopifnot(identical(sent$pipeline[[snv_at]]$params$with_std, FALSE),
          identical(sent$pipeline[[length(sent$pipeline)]]$model$params$scale_x, FALSE),
          identical(sent$pipeline[[length(sent$pipeline)]]$model$params$n_components, 2L),
          identical(sent$future_root_control, list(flag = TRUE)),
          identical(sent$`_comment`, "root comment"), identical(sent$random_state, 7L),
          identical(sent$name, "loaded"), length(sent$pipeline) == 3L)
# Reloading an edited definition keeps the edits (the baseline is the original).
send(nirs4all_load_pipeline(edited))
stopifnot(identical(request()$definition$pipeline[[snv_at]]$params$with_std, FALSE))
# A name-only edit leaves the original steps, comments included, untouched.
renamed <- loaded
renamed$name <- "renamed"
send(renamed)
sent <- request()$definition
stopifnot(identical(sent$name, "renamed"), length(sent$pipeline) == 4L,
          identical(sent$pipeline[[1L]], list(`_comment` = "step comment")),
          identical(sent$future_root_control, list(flag = TRUE)))
# Removing random_state in the visible definition removes it from the request.
no_state <- loaded
no_state$random_state <- NULL
send(no_state)
stopifnot(!"random_state" %in% names(request()$definition),
          identical(request()$definition$future_root_control, list(flag = TRUE)))
# The same holds for a YAML source (sequences stay arrays; YAML `#` comments are
# dropped by the parser and are not data).
yaml_recipe <- paste("name: y", "future_root_control: 1", "pipeline:",
  "  - class: n4m.SNV", "  - model:", "      class: n4m.PLS", "      params:",
  "        n_components: 2", sep = "\n")
yaml_loaded <- nirs4all_load_pipeline(yaml_recipe)
stopifnot(identical(send(yaml_loaded), send(yaml_recipe)),
          identical(request()$definition$future_root_control, 1L))

# 4. Formats-backed S3 form: declared IDs and axis facts at full precision.
formats <- structure(list(X = `rownames<-`(X, paste0("s", 1:6)), y = `names<-`(y, paste0("s", 1:6)),
  sample_ids = paste0("s", 1:6), wavelengths = c(1100.123456789012345, 1101.5, 1102.25),
  axis_kind = "wavelength", axis_unit = "nm", signal_type = "reflectance",
  target_name = "protein", metadata = NULL, formats = NULL, provenance = NULL),
  class = "nirs4all_dataset")
reset(); nirs4all_run_portable_pipeline(linear, formats, core_cli = stub,
                                        methods_library = library_file)
sent <- request()
stopifnot(identical(unlist(sent$dataset$sample_ids), paste0("s", 1:6)),
          identical(sent$dataset$axis$kind, "wavelength"),
          identical(sent$dataset$axis$unit, "nm"),
          identical(sent$dataset$axis$signal_type, "reflectance"),
          identical(sent$dataset$axis$wavelengths[[1L]], 1100.123456789012345))

# 4b. Formats metadata/provenance: one opaque JSON entry per row, in X/sample_ids
# order, unchanged; never folded into the axis or turned into groups/identity.
metadata <- list(
  list(batch = "b1", scans = list(1.0000000000000002, 2), nested = list(a = list(1, list(2, 3)))),
  list(),                                   # an empty entry is allowed
  list(batch = "b2"), list(batch = "b3"), list(batch = "b4"),
  list(batch = "b5", ratio = 0.12345678901234568))
provenance <- list(
  list(format = "csv", row = 1L, steps = list("read", list(name = "scale", value = 0.1))),
  list(), list(format = "csv", row = 3L), list(format = "csv", row = 4L),
  list(format = "csv", row = 5L), list(format = "csv", row = 6L))
rich <- formats
rich$metadata <- metadata
rich$provenance <- provenance
reset(); nirs4all_run_portable_pipeline(linear, rich, core_cli = stub,
                                        methods_library = library_file)
sent <- request()$dataset
stopifnot(identical(names(sent), c("X", "y", "sample_ids", "axis", "metadata", "provenance")),
          identical(names(sent$axis), c("kind", "unit", "signal_type", "wavelengths")),
          length(sent$metadata) == 6L, length(sent$provenance) == 6L,
          identical(sent$metadata[[1L]]$scans, list(1.0000000000000002, 2L)),
          identical(sent$metadata[[1L]]$nested$a, list(1L, list(2L, 3L))),
          identical(sent$metadata[[2L]], list()),
          identical(sent$metadata[[6L]]$ratio, 0.12345678901234568),
          identical(sent$provenance[[1L]]$steps[[2L]]$value, 0.1),
          identical(sent$provenance[[3L]], list(format = "csv", row = 3L)),
          identical(unlist(sent$sample_ids), paste0("s", 1:6)))
# A dataset without them sends neither field.
reset(); nirs4all_run_portable_pipeline(linear, formats, core_cli = stub,
                                        methods_library = library_file)
stopifnot(!any(c("metadata", "provenance") %in% names(request()$dataset)))
# Owner-returned context is projected unchanged (synthetic response, transport only).
returned <- response
returned$identities$sample_ids <- as.list(paste0("s", 1:6))
returned$owner$evidence <- list(rows = list(list(opaque = list(1.0000000000000002, list(2, "x"))),
                                            list()))
write_response(returned)
reset(); returned_result <- nirs4all_run_portable_pipeline(linear, rich, core_cli = stub,
  methods_library = library_file)
stopifnot(identical(returned_result$owner$evidence, jsonlite::fromJSON(jsonlite::toJSON(
            returned$owner$evidence, auto_unbox = TRUE, digits = I(17L)), simplifyVector = FALSE)),
          identical(returned_result$identities$sample_ids, as.list(paste0("s", 1:6))))
write_response()

# 5. Record-backed S3 form: the IO record is forwarded unchanged and its own CLI is used.
record <- list(schema = "nirs4all.dataset.v1", dataset = list(
  sample_ids = as.list(paste0("r", 1:6)), y = list(values = as.list(y), dtype = "float64",
  shape = list(6L)), partitions = list(values = as.list(rep("train", 6L)))))
backed <- structure(list(record = record, core_cli = stub), class = "nirs4all_dataset")
reset(); nirs4all_run_portable_pipeline(linear, backed, methods_library = library_file)
stopifnot(identical(request()$dataset, jsonlite::fromJSON(
  jsonlite::toJSON(record, auto_unbox = TRUE, digits = I(17L)), simplifyVector = FALSE)))

# 6. Plan operation: no dataset, no Methods library, owner admission returned unchanged.
plan_response <- list(schema_version = 1L, profile = "portable_linear_regression.v1",
  owner_profile = "pipeline_steps.v1", mode = "single_holdout", name = "snv_pls",
  splitter = list(type = "KennardStone", params = list(test_size = 0.25)),
  preprocessing = list(), nComponents = list(2L), recipe = list(), candidates = list(),
  evaluation = list(scope = "selection_validation", independent_test = FALSE))
write_response(plan_response)
reset(); plan <- nirs4all_parse_execution_plan(linear, core_cli = stub)
args <- strsplit(readLines(file.path(capture, "args")), " ", fixed = TRUE)[[1L]]
sent <- request()
stopifnot(identical(args[[1L]], "portable-pipeline-plan"), length(args) == 5L,
          !"dataset" %in% names(sent), identical(sent$definition$random_state, 11L),
          identical(plan$admission, "owner"), identical(plan$owner$mode, "single_holdout"),
          identical(plan$profile$route, "owner"), identical(plan$n_components, 2L))
write_response()
reset(); local_plan <- nirs4all_parse_execution_plan(linear)
stopifnot(identical(local_plan$admission, "source_preflight"),
          !file.exists(file.path(capture, "args")), is.null(local_plan$owner))

# 7. A classifier with a feature branch is admitted by the owner plan operation under
# the explicit graph classification profile (no local residual route, original
# document unchanged). The canned plan response only has to carry that profile.
branched <- nirs4all_export_pipeline(nirs4all_pipeline(list(nirs4all_concat(list(
  a = list(nirs4all_snv()), b = list(nirs4all_msc())))), nirs4all_sparse_pls_da(2L)))
write_response(list(schema_version = 1L, profile = "portable_feature_classification.v1",
  owner_profile = "feature_graph.v1", mode = "training", name = "classifier",
  evaluation = list(scope = "training", independent_test = FALSE)))
reset(); branched_plan <- nirs4all_parse_execution_plan(branched, core_cli = stub)
stopifnot(identical(branched_plan$admission, "owner"),
          identical(branched_plan$profile$route, "owner"),
          identical(branched_plan$profile$name, "portable_feature_classification.v1"),
          !length(branched_plan$profile$residual),
          identical(branched_plan$owner$profile, "portable_feature_classification.v1"),
          identical(request()$profile, "portable_feature_classification.v1"),
          !"dataset" %in% names(request()),
          identical(request()$definition, jsonlite::fromJSON(branched, simplifyVector = FALSE)))
write_response()

# 7b. No-fit generator expansion: the complete original document goes to
# `portable-pipeline-expand` (exact two-field request, no profile/dataset/library),
# R builds one object per owner definition with the unchanged constructor and names
# them by position; an owner refusal is an error with no local rerun.
generator_doc <- list(name = "gen", future_root_control = list(flag = TRUE),
  pipeline = list(list(`_comment` = "kept"),
    list(`_or_` = list(list(class = "n4m.SNV"),
                       list(class = "n4m.SavitzkyGolay",
                            params = list(window_length = 5L, polyorder = 2L)))),
    list(model = list(class = "n4m.PLS"), `_range_` = list(1L, 2L, 1L),
         param = "n_components")))
concrete <- function(step, components) list(name = "gen",
  pipeline = list(step, list(model = list(class = "n4m.PLS",
                                          params = list(n_components = components)))))
snv_step <- list(class = "n4m.SNV")
savgol_step <- list(class = "n4m.SavitzkyGolay",
                    params = list(window_length = 5L, polyorder = 2L))
definitions <- list(concrete(snv_step, 1L), concrete(snv_step, 2L),
                    concrete(savgol_step, 1L), concrete(savgol_step, 2L))
write_response(list(schema_version = 1L, definitions = definitions))
reset(); expanded <- nirs4all_expand_portable_pipelines(generator_doc, core_cli = stub)
args <- strsplit(readLines(file.path(capture, "args")), " ", fixed = TRUE)[[1L]]
sent <- request()
stopifnot(identical(args[[1L]], "portable-pipeline-expand"), length(args) == 5L,
          identical(args[c(2L, 4L)], c("--input", "--output")),
          identical(names(sent), c("schema_version", "definition")),
          identical(sent$schema_version, 1L),
          identical(sent$definition, jsonlite::fromJSON(jsonlite::toJSON(generator_doc,
            auto_unbox = TRUE, null = "null", digits = I(17L)), simplifyVector = FALSE)),
          identical(names(expanded), sprintf("variant_%04d", 1:4)))
for (index in 1:4)
  stopifnot(isTRUE(all.equal(expanded[[index]],
    nirs4all_pipeline_from_portable(definitions[[index]]))))
stopifnot(identical(vapply(expanded[[3L]]$steps, `[[`, "", "kind"), "savgol"),
          identical(expanded[[2L]]$learner$spec$n_components, 2L))
write(9L, file.path(capture, "exit"))
error <- failure(nirs4all_expand_portable_pipelines(generator_doc, core_cli = stub))
stopifnot(inherits(error, "nirs4all_portable_owner_refusal"),
          file.exists(file.path(capture, "args")))
reset(); write_response()

# 8. Owner failure and unavailability are errors: no local rerun, no reroute.
write(9L, file.path(capture, "exit"))
error <- failure(nirs4all_run_portable_pipeline(linear, list(X = X, y = y),
  core_cli = stub, methods_library = library_file))
stopifnot(inherits(error, "nirs4all_portable_owner_refusal"),
          grepl("unsupported step at pipeline[1]", conditionMessage(error), fixed = TRUE))
reset()
Sys.setenv(N4M_LIB_PATH = "", N4M_LIBRARY_PATH = "")
error <- failure(nirs4all_run_portable_pipeline(linear, list(X = X, y = y), core_cli = stub))
stopifnot(inherits(error, "error"), grepl("native Methods library", conditionMessage(error)),
          !file.exists(file.path(capture, "args")))
error <- failure(nirs4all_run_portable_pipeline(linear, list(X = X, y = y),
  core_cli = file.path(capture, "missing-cli"), methods_library = library_file))
stopifnot(inherits(error, "error"), grepl("core_cli", conditionMessage(error)))
# Host-side dataset boundary is checked before any native call.
error <- failure(nirs4all_run_portable_pipeline(linear, list(X = X, y = y[-1L]),
  core_cli = stub, methods_library = library_file))
stopifnot(grepl("row-aligned", conditionMessage(error)), !file.exists(file.path(capture, "args")))
# A malformed owner response is refused, never completed locally.
write_response(list(schema_version = 2L))
error <- failure(nirs4all_run_portable_pipeline(linear, list(X = X, y = y),
  core_cli = stub, methods_library = library_file))
stopifnot(grepl("unsupported portable owner response", conditionMessage(error)))
cat("portable-owner-transport: ok (transport only; no scientific parity claimed)\n")
