# Transport test of the owner-executed regression feature-branch profile
# (`portable_feature_regression.v1`, owner profile `feature_graph.v1`) of
# nirs4all_run_portable_pipeline().
#
# A POSIX shell stub stands in for `nirs4all-core-archive`: it records the exact
# argument vector and request bytes and returns a canned response. The test
# inspects the explicit profile, the untouched original topology (including a
# non-alphabetical object order), the optional `feature_blocks` passthrough, the
# classifier-with-branch admission and error propagation. It runs no fit, makes no
# scientific or owner-parity claim and does not show that the owner graph
# profile exists or is qualified (see portable-owner-branch.R for the real CLI).
library(nirs4all)

if (.Platform$OS.type == "windows") {
  cat("NOT RUN: portable-owner-branch-transport.R needs a POSIX shell stub\n")
  quit(save = "no", status = 0L)
}

capture <- tempfile("portable-branch-stub-")
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
write_response <- function(value)
  writeLines(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", digits = I(17L)),
             file.path(capture, "response.json"))

feature_profile <- "portable_feature_regression.v1"
# Two sequential joins: the owner's object is keyed by join node id and each value
# keeps the declared branch order (non-alphabetical here); R only copies it.
blocks <- list(
  "portable:join:0" = list(list(branch_id = "z_msc", column_count = 12L),
                           list(branch_id = "a_snv", column_count = 12L)),
  "portable:join:1" = list(list(branch_id = "left", column_count = 4L),
                           list(branch_id = "right", column_count = 9L)))
entry <- function(n, rmse, ordinal, id, with_blocks = TRUE) {
  value <- list(n_components = n, rmse = rmse, predictions = list(1.5, 2.5),
                params = list(n_components = n), ordinal = ordinal, variant_id = id)
  if (with_blocks) value$feature_blocks <- blocks
  value
}
# The owner's `selected` is deliberately the variant with the LARGER rmse, so a
# host-side argmin would be visible.
owner_response <- function(with_blocks = TRUE, profile = feature_profile) list(
  schema_version = 1L, profile = profile, owner_profile = "feature_graph.v1",
  mode = "single_holdout", name = "branch", rows = 12L, cols = 8L,
  split = list(kind = "KennardStone", trainIndices = list(5L, 0L, 3L, 1L, 2L, 4L, 6L, 7L, 8L),
               testIndices = list(11L, 9L)),
  preprocessing = list(list(branch = list()), list(merge = "features")),
  variants = list(entry(1L, 0.5, "ord:00000", "v0", with_blocks),
                  entry(2L, 0.25, "ord:00001", "v1", with_blocks)),
  selected = entry(1L, 0.5, "ord:00000", "v0", with_blocks),
  targets = list(1.0, 2.0),
  evaluation = list(scope = "selection_validation", independent_test = FALSE),
  identities = list(kind = "positional", sample_ids = list("position_0"),
                    training_sample_ids = list("position_5"),
                    evaluation_sample_ids = list("position_11"), axis = NULL),
  owner = list(profile = "single_resampled_holdout_graph_bank.v1",
               decision = list(), evidence = list(variants = list())))
write_response(owner_response())

X <- outer(seq_len(12L), seq_len(8L), function(i, j) sin(i * j / 7) + i * j / 50)
y <- 2 + X[, 2L] - 0.3 * X[, 5L]
run <- function(definition, dataset = list(X = X, y = y))
  nirs4all_run_portable_pipeline(definition, dataset, core_cli = stub,
                                 methods_library = library_file)

# 1. Non-alphabetical object order, a root control and a comment reach the owner
# in the original bytes; the explicit profile is the derived feature profile.
recipe_text <- paste0('{"name":"branch","future_root_control":{"flag":true},"pipeline":[',
  '{"_comment":"kept"},',
  '{"class":"n4m.KennardStone","params":{"test_size":0.25}},',
  '{"branch":{"z_msc":[{"class":"n4m.MSC"}],',
  '"a_snv":[{"class":"n4m.SNV","params":{"with_std":false}}]}},',
  '{"merge":"features"},',
  '{"model":{"class":"n4m.PLS","params":{"n_components":2}},',
  '"_range_":[1,2,1],"param":"n_components"}]}')
reset(); result <- run(recipe_text)
args <- strsplit(readLines(file.path(capture, "args")), " ", fixed = TRUE)[[1L]]
sent <- request()
steps <- sent$definition$pipeline
stopifnot(identical(args[[1L]], "portable-pipeline-run"), length(args) == 7L,
          identical(sent$profile, feature_profile),
          identical(names(steps[[3L]]$branch), c("z_msc", "a_snv")),
          as.integer(regexpr('"z_msc"', raw_request(), fixed = TRUE)) <
            as.integer(regexpr('"a_snv"', raw_request(), fixed = TRUE)),
          identical(steps[[4L]], list(merge = "features")),
          identical(steps[[1L]], list(`_comment` = "kept")),
          identical(sent$definition$future_root_control, list(flag = TRUE)),
          identical(steps[[3L]]$branch$a_snv[[1L]]$params$with_std, FALSE),
          identical(steps[[5L]]$`_range_`, list(1L, 2L, 1L)),
          !"class_labels" %in% names(sent$dataset), identical(names(sent$dataset), c("X", "y")))

# 2. Projection: exact profile, owner winner, optional blocks copied untouched.
stopifnot(identical(result$profile$route, "owner"),
          identical(result$profile$name, feature_profile),
          identical(result$profile$owner_profile, "feature_graph.v1"),
          identical(result$owner$profile, "single_resampled_holdout_graph_bank.v1"),
          identical(result$split$kind, "KennardStone"),
          identical(result$selected$rmse, 0.5),                 # owner's winner, not argmin
          identical(result$selected$variant_id, "v0"),
          identical(result$variants[[1L]]$feature_blocks, roundtrip(blocks)),
          identical(result$selected$feature_blocks, roundtrip(blocks)),
          identical(names(result$variants[[2L]]$feature_blocks),
                    c("portable:join:0", "portable:join:1")),
          identical(vapply(result$selected$feature_blocks[["portable:join:0"]],
                           `[[`, "", "branch_id"), c("z_msc", "a_snv")),
          identical(vapply(result$selected$feature_blocks[["portable:join:1"]],
                           `[[`, integer(1), "column_count"), c(4L, 9L)),
          identical(result$evaluation, list(scope = "selection_validation",
                                            independent_test = FALSE)),
          is.null(result$unseen_validation_classes))

# 3. The optional field is absent when the owner does not supply it (no invention).
write_response(owner_response(with_blocks = FALSE))
reset(); plain <- run(recipe_text)
stopifnot(is.null(plain$variants[[1L]]$feature_blocks), is.null(plain$selected$feature_blocks))
write_response(owner_response())

# 4. Array and `{name, steps}` branch forms are forwarded exactly as declared;
# R neither names, sorts nor flattens them.
array_form <- list(pipeline = list(
  list(branch = list(list(list(class = "n4m.MSC")),
                     list(name = "second", steps = list(list(class = "n4m.SNV"))))),
  list(merge = "features"),
  list(model = list(class = "n4m.PLS", params = list(n_components = 2L)))))
reset(); run(array_form)
stopifnot(identical(request()$definition$pipeline[[1L]],
                    roundtrip(array_form$pipeline[[1L]])))

# 5. Plan operation: explicit profile, no dataset, owner admission returned as is.
plan_response <- list(schema_version = 1L, profile = feature_profile,
  owner_profile = "feature_graph.v1", mode = "single_holdout", name = "branch",
  splitter = list(type = "KennardStone", params = list(test_size = 0.25)),
  preprocessing = list(), nComponents = list(1L, 2L), recipe = list(), candidates = list(),
  evaluation = list(scope = "selection_validation", independent_test = FALSE))
write_response(plan_response)
reset(); plan <- nirs4all_parse_execution_plan(recipe_text, core_cli = stub)
args <- strsplit(readLines(file.path(capture, "args")), " ", fixed = TRUE)[[1L]]
stopifnot(identical(args[[1L]], "portable-pipeline-plan"), length(args) == 5L,
          identical(request()$profile, feature_profile), !"dataset" %in% names(request()),
          identical(plan$admission, "owner"), identical(plan$profile$name, feature_profile),
          identical(plan$owner, roundtrip(plan_response)))
write_response(owner_response())

# 6. A branch with a classifier is admitted by the owner plan operation under the
# graph classification profile; nothing stays residual and nothing runs locally.
combined <- nirs4all_export_pipeline(nirs4all_pipeline(list(nirs4all_concat(list(
  a = list(nirs4all_snv()), b = list(nirs4all_msc())))), nirs4all_sparse_pls_da(2L)))
write_response(list(schema_version = 1L, profile = "portable_feature_classification.v1",
  owner_profile = "feature_graph.v1", mode = "training", name = "combined",
  evaluation = list(scope = "training", independent_test = FALSE)))
reset(); admitted <- nirs4all_parse_execution_plan(combined, core_cli = stub)
stopifnot(identical(admitted$admission, "owner"),
          identical(admitted$profile$name, "portable_feature_classification.v1"),
          !length(admitted$profile$residual),
          identical(request()$profile, "portable_feature_classification.v1"))
write_response(owner_response())

# 7. Existing source refusals are kept (augmentation with a branch), before any
# native call.
augmented <- list(pipeline = list(
  list(train_augmentation = list(class = "n4m.NativeXAugmentation",
    params = list(kind = "gaussian_noise", values = list(0.05), seed = 3))),
  list(branch = list(a = list(list(class = "n4m.SNV")), b = list(list(class = "n4m.MSC")))),
  list(merge = "features"), list(model = list(class = "n4m.PLS"))))
reset(); error <- failure(run(augmented))
stopifnot(inherits(error, "error"), !file.exists(file.path(capture, "args")))

# 8. An owner refusal is an error with the native message: no local rerun, no
# fallback, no partial result.
write(9L, file.path(capture, "exit"))
writeLines("feature_graph.v1 refuses the recipe at $.pipeline[2].branch.z_msc[0]",
           file.path(capture, "message"))
if (file.exists(file.path(capture, "args"))) file.remove(file.path(capture, "args"))
error <- failure(run(recipe_text))
stopifnot(inherits(error, "nirs4all_portable_owner_refusal"),
          grepl("feature_graph.v1 refuses the recipe", conditionMessage(error), fixed = TRUE),
          file.exists(file.path(capture, "args")))
reset()

# 9. A response for another profile is a contract violation, never completed locally.
write_response(owner_response(profile = "portable_linear_regression.v1"))
error <- failure(run(recipe_text))
stopifnot(grepl("unsupported portable owner response", conditionMessage(error)))
cat("portable-owner-branch-transport: ok (transport only; no parity claimed)\n")
