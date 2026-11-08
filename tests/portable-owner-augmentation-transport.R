# Transport test of the owner-executed standalone regression train augmentation
# and extra fit inputs (MBPLS `block_sizes`, GroupSparsePLS `group_assignment`):
# the existing profile `portable_feature_regression.v1` (owner profile
# `feature_graph.v1`) of nirs4all_run_portable_pipeline().
#
# A POSIX shell stub stands in for `nirs4all-core-archive`: it records the exact
# argument vector and request bytes and returns a canned response. The test
# inspects the explicit profile, the untouched original numbers (declared order,
# multi-parameter values, seeds above 2^31 and up to 2^53-1), the unchanged
# feature-axis arrays, the combined-recipe admissions and error propagation. It runs
# no fit, makes no scientific or owner-parity claim and does not show that the
# owner augmentation/fit-input route exists or is qualified (see
# portable-owner-augmentation.R for the real Core CLI).
library(nirs4all)

if (.Platform$OS.type == "windows") {
  cat("NOT RUN: portable-owner-augmentation-transport.R needs a POSIX shell stub\n")
  quit(save = "no", status = 0L)
}

capture <- tempfile("portable-aug-stub-")
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
# Canned owner response without a branch: the optional `feature_blocks` is absent,
# so the host must not produce one.
owner_response <- function() list(
  schema_version = 1L, profile = feature_profile, owner_profile = "feature_graph.v1",
  mode = "single_holdout", name = "augmented", rows = 12L, cols = 8L,
  split = list(kind = "KennardStone", trainIndices = list(5L, 0L, 3L, 1L, 2L, 4L, 6L, 7L, 8L),
               testIndices = list(11L, 9L, 10L)),
  preprocessing = list(),
  variants = list(list(n_components = 2L, rmse = 0.5, predictions = list(1.5, 2.5, 3.5),
                       params = list(n_components = 2L), ordinal = "ord:00000", variant_id = "v0")),
  selected = list(n_components = 2L, rmse = 0.5, predictions = list(1.5, 2.5, 3.5),
                  params = list(n_components = 2L), ordinal = "ord:00000", variant_id = "v0"),
  targets = list(1.0, 2.0, 3.0),
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
aug <- function(kind, values, seed) list(train_augmentation = list(
  class = "n4m.NativeXAugmentation", params = list(kind = kind, values = values, seed = seed)))

# 1. Two augmenters, multi-parameter numbers in declared order (band_perturb and
# dead_band fixtures of native-augmentations.R, including the enum/bool-like
# positions as plain numbers), seeds above 2^31 and the largest exact seed. R sends
# the original numbers; it neither names positions nor casts the seeds.
band <- list(1, 2, 4, 0.9, 1.1, -0.01, 0.01)
dead <- list(1, 2, 4, 0.01, 0.5, 0)
definition <- list(name = "augmented", pipeline = list(
  list(class = "n4m.KennardStone", params = list(test_size = 0.25)),
  aug("band_perturb", band, 4294967299),
  aug("dead_band", dead, 9007199254740991),
  list(class = "n4m.SNV"),
  list(model = list(class = "n4m.PLS", params = list(n_components = 2L)))))
text <- as.character(jsonlite::toJSON(definition, auto_unbox = TRUE, digits = I(17L)))
reset(); result <- run(text)
args <- strsplit(readLines(file.path(capture, "args")), " ", fixed = TRUE)[[1L]]
sent <- request()
steps <- sent$definition$pipeline
stopifnot(identical(args[[1L]], "portable-pipeline-run"), length(args) == 7L,
          identical(sent$profile, feature_profile),
          identical(names(sent$dataset), c("X", "y")),
          length(steps) == 5L, identical(steps[[1L]]$params$test_size, 0.25),
          identical(names(steps[[2L]]$train_augmentation), c("class", "params")),
          identical(names(steps[[2L]]$train_augmentation$params), c("kind", "values", "seed")),
          identical(steps[[2L]]$train_augmentation$params$kind, "band_perturb"),
          identical(steps[[3L]]$train_augmentation$params$kind, "dead_band"),
          identical(as.numeric(unlist(steps[[2L]]$train_augmentation$params$values)),
                    unlist(band)),
          identical(as.numeric(unlist(steps[[3L]]$train_augmentation$params$values)),
                    unlist(dead)),
          identical(steps[[2L]]$train_augmentation$params$seed, 4294967299),
          identical(steps[[3L]]$train_augmentation$params$seed, 9007199254740991),
          grepl('"seed":4294967299', raw_request(), fixed = TRUE),
          grepl('"seed":9007199254740991', raw_request(), fixed = TRUE),
          !grepl("methodId|fit_inputs_v1|e\\+", raw_request()))
stopifnot(identical(result$profile$name, feature_profile),
          identical(result$profile$owner_profile, "feature_graph.v1"),
          identical(result$owner$profile, "single_resampled_holdout_graph_bank.v1"),
          is.null(result$selected$feature_blocks),
          is.null(result$variants[[1L]]$feature_blocks),
          identical(result$evaluation, list(scope = "selection_validation",
                                            independent_test = FALSE)))

# 2. The same wire from a YAML source keeps one-element and large values exactly.
yaml_text <- paste("pipeline:", "  - train_augmentation:",
  "      class: n4m.NativeXAugmentation", "      params:", "        kind: gaussian_noise",
  "        values: [0.05]", "        seed: 4294967296", "  - class: n4m.SNV",
  "  - model:", "      class: n4m.PLS", "      params:", "        n_components: 2", sep = "\n")
reset(); run(yaml_text)
node <- request()$definition$pipeline[[1L]]$train_augmentation$params
stopifnot(identical(node$values, list(0.05)), identical(node$seed, 4294967296),
          identical(names(node), c("kind", "values", "seed")))

# 3. MBPLS block_sizes and GroupSparsePLS group_assignment stay positional
# feature-axis arrays of the original document; R adds no reserved parameter, no
# `blocks`/`feature_groups` and no sample `groups`.
mbpls <- list(pipeline = list(list(class = "n4m.SNV"), list(model = list(
  class = "n4m.MBPLS", params = list(n_components = 2L, block_sizes = c(3L, 3L, 2L))))))
reset(); run(mbpls)
sent <- request()
stopifnot(identical(sent$profile, feature_profile),
          identical(sent$definition$pipeline[[2L]]$model$params$block_sizes, list(3L, 3L, 2L)),
          identical(sent$definition$pipeline[[2L]]$model$params$n_components, 2L),
          !grepl("fit_inputs_v1|blocks\"|feature_groups|groups\"", raw_request()))
group <- list(pipeline = list(list(class = "n4m.SNV"), list(model = list(
  class = "n4m.GroupSparsePLS", params = list(n_components = 2L,
    group_assignment = c(0L, 0L, 1L, 1L, 2L, 2L, 3L, 3L), group_lambda = 0.2)))))
reset(); run(group)
sent <- request()
params <- sent$definition$pipeline[[2L]]$model$params
stopifnot(identical(sent$profile, feature_profile),
          identical(params$group_assignment, as.list(c(0L, 0L, 1L, 1L, 2L, 2L, 3L, 3L))),
          identical(params$group_lambda, 0.2),
          !grepl("fit_inputs_v1|feature_groups|\"groups\"|\"blocks\"", raw_request()))
# Augmentation together with extra fit inputs is still the same standalone profile.
both <- mbpls
both$pipeline <- c(list(aug("gaussian_noise", list(0.05), 3)), both$pipeline)
reset(); run(both)
stopifnot(identical(request()$profile, feature_profile),
          identical(request()$definition$pipeline[[1L]]$train_augmentation$params$seed, 3L))

# 4. Plan operation: explicit profile, no dataset, owner admission returned unchanged.
plan_response <- list(schema_version = 1L, profile = feature_profile,
  owner_profile = "feature_graph.v1", mode = "single_holdout", name = "augmented",
  splitter = list(type = "KennardStone", params = list(test_size = 0.25)),
  preprocessing = list(), nComponents = list(2L), recipe = list(), candidates = list(),
  evaluation = list(scope = "selection_validation", independent_test = FALSE))
write_response(plan_response)
reset(); plan <- nirs4all_parse_execution_plan(text, core_cli = stub)
args <- strsplit(readLines(file.path(capture, "args")), " ", fixed = TRUE)[[1L]]
stopifnot(identical(args[[1L]], "portable-pipeline-plan"), length(args) == 5L,
          identical(request()$profile, feature_profile), !"dataset" %in% names(request()),
          identical(plan$admission, "owner"), identical(plan$profile$name, feature_profile),
          identical(plan$owner, roundtrip(plan_response)))
write_response(owner_response())

# 5. Combined recipes are admitted by the owner plan operation under the explicit
# graph profile (classification for a classifier, regression for fit inputs with a
# branch): original document unchanged.
branch_steps <- list(nirs4all_concat(list(a = list(nirs4all_snv()), b = list(nirs4all_msc()))))
classifier_augmented <- nirs4all_export_pipeline(nirs4all_pipeline(
  learner = nirs4all_sparse_pls_da(2L), augmentations = list(
    nirs4all_native_augmentation("gaussian_noise", 0.05, seed = 3))))
branch_fit_inputs <- nirs4all_export_pipeline(nirs4all_pipeline(branch_steps,
  nirs4all_n4m_method("mb_pls", 2L, list(block_sizes = c(12L, 12L)))))
for (case in list(list(classifier_augmented, "portable_feature_classification.v1"),
                  list(branch_fit_inputs, "portable_feature_regression.v1"))) {
  write_response(list(schema_version = 1L, profile = case[[2L]],
    owner_profile = "feature_graph.v1", mode = "training", name = "combined",
    evaluation = list(scope = "training", independent_test = FALSE)))
  reset(); admitted <- nirs4all_parse_execution_plan(case[[1L]], core_cli = stub)
  stopifnot(identical(admitted$admission, "owner"),
            identical(admitted$profile$name, case[[2L]]),
            identical(request()$profile, case[[2L]]),
            identical(request()$definition, jsonlite::fromJSON(case[[1L]], simplifyVector = FALSE)))
}
write_response(owner_response())

# 6. Existing source refusals are kept and happen before any native call.
reset()
for (bad in list(
    # augmentation after preprocessing
    list(pipeline = list(list(class = "n4m.SNV"), aug("gaussian_noise", list(0.05), 1),
                         list(model = list(class = "n4m.PLS")))),
    # values must be a positional numeric array
    list(pipeline = list(aug("gaussian_noise", 0.05, 1), list(model = list(class = "n4m.PLS")))),
    # seed above 2^53 - 1
    list(pipeline = list(aug("gaussian_noise", list(0.05), 9007199254740992),
                         list(model = list(class = "n4m.PLS")))),
    # a named (feature-name keyed) group assignment is not positional
    list(pipeline = list(list(model = list(class = "n4m.GroupSparsePLS", params = list(
      n_components = 2L, group_assignment = c(a = 0L, b = 1L))))))
)) {
  error <- failure(run(bad))
  stopifnot(inherits(error, "error"), !file.exists(file.path(capture, "args")))
}

# 7. An owner refusal is an error with the native message: no local rerun, no
# fallback, no partial result.
write(9L, file.path(capture, "exit"))
writeLines("feature_graph.v1 refuses $.recipe[0].train_augmentation.params.values[3]",
           file.path(capture, "message"))
error <- failure(run(text))
stopifnot(inherits(error, "nirs4all_portable_owner_refusal"),
          grepl("feature_graph.v1 refuses", conditionMessage(error), fixed = TRUE),
          file.exists(file.path(capture, "args")))
reset()
cat("portable-owner-augmentation-transport: ok (transport only; no parity claimed)\n")
