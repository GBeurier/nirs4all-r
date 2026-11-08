# Canonical V1 .n4a operations through the native Core CLI (n4a-* commands).
#
# Real CLI, real owners, real Methods library; nothing is mocked. The profile
# witness is standard_scale + ridge with two ordered targets (protein,
# moisture); it is not a universal-support claim.
#
# Environment:
#   NIRS4ALL_CORE_CLI            nirs4all-core-archive that provides n4a-*
#   N4M_LIBRARY_PATH             Methods library file
#   N4M_EXPECTED_LIBRARY_SHA256  independently attested SHA-256 of that file
#   NIRS4ALL_N4A_FIXTURE_DIR     directory with train.json, train-v2.json,
#                                future.json, future-permuted.json, recipe.json
#                                (written by the Rust test with
#                                N4A_CLI_WRITE_FIXTURES=<dir>)
#   NIRS4ALL_REQUIRE_CORE_N4A=1  turn missing prerequisites into a failure
#
# Without prerequisites only the argument checks run and an explicit SKIP line
# is printed. R_N4A_CANONICAL_PASS is printed only after every assertion ran.
library(nirs4all)

env <- function(name) Sys.getenv(name, "")
refusal <- function(expr) tryCatch({ expr; NULL }, nirs4all_n4a_refusal = function(e) e)
failure <- function(expr) tryCatch({ expr; NULL }, error = function(e) e)

# --- Argument checks that need no native executable -------------------------
dataset_like <- structure(list(record = list()), class = "nirs4all_dataset")
stopifnot(inherits(failure(nirs4all_n4a_produce(dataset_like, "{}", tempfile(),
  methods_library = "/l", methods_library_sha256 = "00")), "error"))
stopifnot(inherits(failure(nirs4all_n4a_predict(list())), "error"))
stopifnot(grepl("methods_library", failure(nirs4all_n4a_produce(tempfile(), "{}", tempfile(),
  methods_library = "", methods_library_sha256 = ""))$message))

# --- Memory budget: validation and exact forwarding through every n4a-* transport ---
budget <- nirs4all:::.nirs4all_n4a_budget
stopifnot(is.null(budget(NULL)), is.null(budget("")), identical(budget(0), "0"),
          identical(budget(4096), "4096"), identical(budget(4096L), "4096"),
          identical(budget(9007199254740991), "9007199254740991"),
          identical(budget("18446744073709551615"), "18446744073709551615"),
          identical(budget("007"), "007"), identical(budget("000"), "000"),
          identical(budget("0018446744073709551615"), "0018446744073709551615"))
for (bad in list(9007199254740992, 4096.5, -1, NaN, Inf, NA, NA_character_, TRUE, "0018446744073709551616", "-1",
                 "+1", "1e3", " 1", "abc", "18446744073709551616", "99999999999999999999",
                 c(1, 2), list(1), integer(0)))
  stopifnot(inherits(failure(budget(bad)), "error"))
# A stub CLI echoes its arguments into the report; no native executable is used.
stub_dir <- tempfile("n4a-stub-")
dir.create(stub_dir)
stub_input <- file.path(stub_dir, "input.json")
writeLines("x", stub_input)
stopifnot(grepl("memory_budget_bytes", failure(nirs4all_n4a_inspect(stub_input, memory_budget_bytes = 1.5))$message, fixed = TRUE))
if (.Platform$OS.type == "unix") {
  stub <- file.path(stub_dir, "stub-cli.sh")
  writeLines('{"argv":"@ARGV@","sample_ids":["a"],"target_names":["t"],"target_units":[null],"values":[[1.5]],"source_id":"s","partition":"p","input_mode":"m","archive_sha256":"x","replay":{},"receipt":{},"training_outcome":{},"training_source_id":"s","source_archive_sha256":"y","readability":{"container_sha256":"z"},"executability":"not_evaluated"}', file.path(stub_dir, "template.json"))
  writeLines(c("#!/bin/sh", 'out=""; prev=""',
    'for a in "$@"; do if [ "$prev" = "--output" ]; then out="$a"; fi; prev="$a"; done',
    'sed "s|@ARGV@|$*|" "$(dirname "$0")/template.json" > "$out"'), stub)
  Sys.chmod(stub, "755")
  flag <- "--memory-budget-bytes"
  sent <- function(x, value) grepl(paste(flag, value), x$native_json, fixed = TRUE)
  none <- function(x) !grepl("--memory-budget", x$native_json, fixed = TRUE)
  big <- "18446744073709551615"
  fake_runtime <- list(methods_library = "/l", methods_library_sha256 = "00")
  object <- structure(list(path = stub_input, archive_sha256 = "z", core_cli = stub, source_id = "s"),
                      class = "nirs4all_n4a")
  stopifnot(sent(nirs4all_n4a_inspect(stub_input, core_cli = stub, memory_budget_bytes = big), big),
            sent(do.call(nirs4all_n4a_produce, c(list(stub_input, stub_input, file.path(stub_dir, "p.n4a"),
              core_cli = stub, memory_budget_bytes = big), fake_runtime)), big),
            sent(do.call(nirs4all_n4a_predict, c(list(object, memory_budget_bytes = 4096), fake_runtime)), "4096"),
            sent(do.call(nirs4all_n4a_retrain, c(list(object, stub_input, file.path(stub_dir, "r.n4a"),
              memory_budget_bytes = big), fake_runtime)), big),
            none(nirs4all_n4a_inspect(stub_input, core_cli = stub)),
            none(nirs4all_n4a_inspect(stub_input, core_cli = stub, memory_budget_bytes = "")))
}
unlink(stub_dir, recursive = TRUE)

needed <- c("NIRS4ALL_CORE_CLI", "N4M_LIBRARY_PATH", "N4M_EXPECTED_LIBRARY_SHA256",
            "NIRS4ALL_N4A_FIXTURE_DIR")
missing <- needed[!nzchar(vapply(needed, env, ""))]
if (length(missing)) {
  if (identical(env("NIRS4ALL_REQUIRE_CORE_N4A"), "1"))
    stop("Missing required environment: ", paste(missing, collapse = ", "), call. = FALSE)
  message("SKIP n4a-canonical: ", paste(missing, collapse = ", "),
          " not set; no native assertion ran")
} else local({
  cli <- env("NIRS4ALL_CORE_CLI")
  library_path <- env("N4M_LIBRARY_PATH")
  sha <- env("N4M_EXPECTED_LIBRARY_SHA256")
  fixtures <- env("NIRS4ALL_N4A_FIXTURE_DIR")
  # A directory with spaces exercises argument quoting.
  directory <- tempfile("nirs4all n4a canonical ")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  fixture <- function(name) file.path(fixtures, name)
  read_json <- function(name) jsonlite::fromJSON(fixture(name), simplifyVector = FALSE)
  file_sha <- function(path) digest::digest(path, algo = "sha256", file = TRUE)
  runtime <- list(methods_library = library_path, methods_library_sha256 = sha)
  predict_with <- function(...) do.call(nirs4all_n4a_predict, c(list(...), runtime))
  tolerance <- 1e-9

  # Non-container input is refused with the native message.
  garbage <- file.path(directory, "garbage.n4a")
  writeLines("not a container", garbage)
  stopifnot(!is.null(refusal(nirs4all_n4a_inspect(garbage, core_cli = cli))))

  # Produce once; the reported identity is the identity of the published bytes.
  archive <- file.path(directory, "model.n4a")
  model <- do.call(nirs4all_n4a_produce, c(list(fixture("train.json"), fixture("recipe.json"),
    archive, run_id = "run:r:n4a:produce", container_id = "r-n4a-produce",
    embed_training_record = TRUE, core_cli = cli), runtime))
  stopifnot(file.exists(archive), identical(model$archive_sha256, file_sha(archive)),
            grepl("^[0-9a-f]{64}$", model$archive_sha256))
  sealed <- model$archive_sha256
  stopifnot(!is.null(refusal(do.call(nirs4all_n4a_produce, c(list(fixture("train.json"),
    fixture("recipe.json"), archive, core_cli = cli), runtime))) ),
    identical(file_sha(archive), sealed))

  # Inspect: readable, executability not evaluated.
  info <- nirs4all_n4a_inspect(archive, core_cli = cli, expected_sha256 = sealed)
  stopifnot(identical(info$executability, "not_evaluated"),
            identical(info$readability$storage, "verified"),
            identical(info$archive_sha256, sealed),
            identical(info$readability$manifest$schema, "nirs4all.n4a.container.v1"),
            identical(info$readability$training_view$status, "readable"))

  # External prediction in the original and in a permuted physical row order.
  future_ids <- unlist(read_json("future.json")$dataset$sample_ids)
  permuted_ids <- unlist(read_json("future-permuted.json")$dataset$sample_ids)
  p1 <- predict_with(model, fixture("future.json"), partition = "predict")
  p2 <- predict_with(model, fixture("future-permuted.json"), partition = "predict")
  stopifnot(identical(file_sha(archive), sealed))  # no refit, no rewrite
  stopifnot(identical(p1$sample_ids, future_ids), identical(p2$sample_ids, permuted_ids),
            identical(p1$target_names, c("protein", "moisture")),
            identical(dim(p1$values), c(5L, 2L)), identical(dim(p2$values), c(5L, 2L)),
            all(is.finite(p1$values)), all(is.finite(p2$values)),
            identical(p1$partition, "predict"), identical(p1$source_id, "spectra"),
            identical(p1$input_mode, "external"),
            identical(p2$replay$phase, "PREDICT"),
            identical(p2$receipt$authenticity, "not_established"),
            identical(rownames(p2$values), permuted_ids),
            identical(colnames(p2$values), c("protein", "moisture")))
  # Each sample keeps its prediction wherever it sits in the file.
  stopifnot(max(abs(p2$values - p1$values[permuted_ids, ])) < tolerance)
  # Non-vacuous: the permutation is visible positionally.
  stopifnot(max(abs(p2$values - p1$values)) > 1e-6)
  # The exposed rows are the replay rows aligned by sample ID, once.
  block <- p2$replay$outputs[[1L]]$predictions[[1L]]
  block_ids <- unlist(block$sample_ids)
  block_values <- do.call(rbind, lapply(block$values, unlist))
  stopifnot(identical(unname(p2$values), block_values[match(p2$sample_ids, block_ids), ]),
            !identical(p2$sample_ids, block_ids))

  # Stored in-sample prediction: each column is closer to its own target.
  train <- read_json("train.json")
  truth <- do.call(rbind, lapply(train$dataset$y$values, unlist))
  stored <- predict_with(model)
  stopifnot(identical(stored$input_mode, "stored"), identical(stored$partition, "train"),
            identical(stored$sample_ids, unlist(train$dataset$sample_ids)))
  for (column in 1:2) {
    own <- mean(abs(stored$values[, column] - truth[, column]))
    other <- mean(abs(stored$values[, column] - truth[, 3L - column]))
    stopifnot(own < other)
  }

  # Refusals keep the native message and never touch the archive.
  bad <- model
  bad$archive_sha256 <- strrep("0", 64L)
  stale <- refusal(predict_with(bad, fixture("future.json")))
  stopifnot(!is.null(stale), grepl("archive bytes changed", stale$detail, fixed = TRUE))
  stopifnot(!is.null(refusal(nirs4all_n4a_predict(model, fixture("future.json"),
    methods_library = library_path, methods_library_sha256 = strrep("0", 64L)))))
  stopifnot(!is.null(refusal(predict_with(model, fixture("future.json"),
    entrypoint = "calibrate-external"))))
  stopifnot(identical(file_sha(archive), sealed))

  # Retrain through the owner API (stored recipe and source ID, no R-side
  # training). Owner errors propagate: there is no skip once prerequisites exist.
  retrained <- file.path(directory, "retrained.n4a")
  fresh <- do.call(nirs4all_n4a_retrain, c(list(model, fixture("train-v2.json"), retrained,
    run_id = "run:r:n4a:retrain", container_id = "r-n4a-retrain",
    embed_training_record = TRUE), runtime))
  stopifnot(file.exists(retrained), identical(file_sha(archive), sealed),
            !identical(fresh$archive_sha256, sealed),
            identical(fresh$source_archive_sha256, sealed),
            identical(fresh$archive_sha256, file_sha(retrained)))
  after <- predict_with(fresh, fixture("future.json"), partition = "predict")
  stopifnot(identical(after$sample_ids, p1$sample_ids),
            max(abs(after$values - p1$values)) > 1e-6)
  cat("R_N4A_CANONICAL_PASS produce=1 inspect=1 predict_external_permuted=1",
      "predict_stored=1 refusals=1 retrain=1\n")
})
