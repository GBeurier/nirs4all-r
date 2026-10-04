# Installed public API witness, using independently produced native archives and
# oracle predictions. Optional for R CMD check, mandatory for the XL04 gate.
library(nirs4all)
manifest_path <- Sys.getenv("NIRS4ALL_CORE_R_ZIP_FIXTURE_MANIFEST")
mandatory <- identical(Sys.getenv("NIRS4ALL_REQUIRE_CORE_R_ZIP"), "1")
if (!nzchar(manifest_path)) {
  if (mandatory) stop("XL04 requires actual U07 process archive fixtures", call. = FALSE)
  message("SKIP public native ZIP replay: qualification fixture manifest absent")
} else {
  stopifnot(.Platform$OS.type != "windows", !nzchar(Sys.which("python")),
            !nzchar(Sys.which("python3")))
  # Qualification launches Rscript by absolute path with PATH limited to a
  # native-only directory; Python is neither a transport nor an available tool.
  fixtures <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  stopifnot(identical(fixtures$schema_version, 1L), length(fixtures$cases) >= 1L)
  versions <- vapply(fixtures$cases, function(case) as.integer(case$archive_version), integer(1))
  stopifnot(2L %in% versions, all(versions %in% c(2L, 3L)))
  core <- Sys.getenv("NIRS4ALL_CORE_ARCHIVE_CLI")
  dag <- Sys.getenv("NIRS4ALL_DAG_CLI")
  stopifnot(nzchar(core), nzchar(dag))
  read_json <- function(path) jsonlite::fromJSON(path, simplifyVector = FALSE)
  lifecycle <- function(path) {
    if (!file.exists(path)) return(list())
    lapply(readLines(path, warn = FALSE), function(line)
      jsonlite::fromJSON(line, simplifyVector = FALSE))
  }
  operations <- function(path) vapply(lifecycle(path), `[[`, character(1), "operation")
  make_adapter <- function(case, config = read_json(case$source_config_path)) {
    # This config is current test input, not read from package/ZIP contents.
    nirs4all_core_multimodal_adapter(config$sources, config$operators,
      config$node_params, config$source_ids, config$manifest, config$trusted_manifest,
      config$target_names)
  }
  run <- function(case, archive, adapter, request = case$request_path,
                  envelopes = case$envelopes_path, trust = case$trusted_controllers_path) {
    nirs4all_core_archive_predict(archive, request, envelopes, adapter, trust,
      dag_cli = dag, outcome_id = paste0("outcome:r.zip.", case$archive_version),
      run_id = paste0("run:r.zip.", case$archive_version), process_timeout_ms = 30000)
  }
  expect_refusal <- function(action, adapter, require_no_worker = FALSE) {
    failure <- tryCatch({ action(); NULL }, error = identity)
    stopifnot(inherits(failure, "error"))
    ops <- operations(adapter$audit_path)
    stopifnot(!any(ops %in% c("fit", "FIT_CV", "REFIT")))
    if (require_no_worker) stopifnot(length(ops) == 0L)
    # If native hydration occurred before a buffer refusal, every imported
    # state must still be disposed exactly once by the owning lifecycle.
    stopifnot(sum(ops == "hydrate") == sum(ops == "dispose"))
    failure
  }
  for (case in fixtures$cases) {
    archive <- nirs4all_core_archive(case$archive, core_cli = core)
    stopifnot(identical(as.integer(archive$archive$schema_version), as.integer(case$archive_version)))
    original <- digest::digest(file = case$archive, algo = "sha256", serialize = FALSE)
    package_ref <- if (case$archive_version == 2L)
      archive$archive$replay$portable_predictor_package else
      archive$archive$replay$portable_refit_package
    package_path <- tempfile(fileext = ".json")
    status <- system2(core, vapply(c("read-member", "--archive", archive$path,
      "--expected-archive-sha256", archive$archive$archive_sha256,
      "--member", package_ref$member_path, "--output", package_path), shQuote, character(1)))
    stopifnot(identical(status, 0L), identical(
      digest::digest(file = package_path, algo = "sha256", serialize = FALSE), package_ref$raw_sha256))
    unlink(package_path)
    adapter <- make_adapter(case)
    outcome <- run(case, archive, adapter)
    stopifnot(identical(outcome$phase, "PREDICT"), length(outcome$outputs) == 1L)
    prediction <- outcome$outputs[[1L]]$predictions[[1L]]
    stopifnot(identical(unlist(prediction$sample_ids, use.names = FALSE),
                        unlist(case$expected_sample_ids, use.names = FALSE)))
    values <- unlist(prediction$values, use.names = FALSE)
    expected <- unlist(case$expected_predictions, use.names = FALSE)
    stopifnot(length(values) == length(expected), all(is.finite(values)),
              max(abs(values - expected)) <= case$absolute_tolerance,
              identical(original, digest::digest(file = case$archive, algo = "sha256", serialize = FALSE)))
    ops <- operations(adapter$audit_path)
    stopifnot(sum(ops == "PREDICT") >= 1L, sum(ops == "hydrate") >= 1L,
              sum(ops == "hydrate") == sum(ops == "release"),
              sum(ops == "hydrate") == sum(ops == "dispose"),
              !any(ops %in% c("fit", "FIT_CV", "REFIT")))
    producer <- read_json(case$source_config_path)$manifest$controller_id
    stopifnot(all(vapply(outcome$lineage, function(record)
      identical(record$controller_id, producer) && identical(record$phase, "PREDICT"), logical(1))))

    # Signed request mutation and independent trust drift are native refusals
    # before worker construction, not failures of an invented R validator.
    request <- read_json(case$request_path)
    request$source_outcome_fingerprint <- paste(rep("0", 64L), collapse = "")
    bad_adapter <- make_adapter(case)
    expect_refusal(function() run(case, archive, bad_adapter, request = request),
                   bad_adapter, require_no_worker = TRUE)
    trust <- read_json(case$trusted_controllers_path)
    trust[[1L]]$controller_version <- "untrusted-version"
    bad_adapter <- make_adapter(case)
    expect_refusal(function() run(case, archive, bad_adapter, trust = trust),
                   bad_adapter, require_no_worker = TRUE)
    envelopes <- read_json(case$envelopes_path)
    envelopes[[1L]]$schema_fingerprint <- paste(rep("0", 64L), collapse = "")
    bad_adapter <- make_adapter(case)
    expect_refusal(function() run(case, archive, bad_adapter, envelopes = envelopes),
                   bad_adapter, require_no_worker = TRUE)
    envelopes <- read_json(case$envelopes_path)
    envelopes[[1L]]$predict_cohort$physical_sample_ids[[1L]] <- "sample:foreign"
    bad_adapter <- make_adapter(case)
    expect_refusal(function() run(case, archive, bad_adapter, envelopes = envelopes),
                   bad_adapter, require_no_worker = TRUE)

    # Mutating the loaded archive's original bytes must fail Core identity
    # before any DAG operator process, even if the old view is still retained.
    copy <- tempfile(fileext = ".n4a")
    stopifnot(file.copy(case$archive, copy))
    changed <- nirs4all_core_archive(copy, core_cli = core)
    file <- file(copy, open = "ab")
    writeBin(as.raw(1L), file)
    close(file)
    bad_adapter <- make_adapter(case)
    expect_refusal(function() run(case, changed, bad_adapter), bad_adapter, require_no_worker = TRUE)
    unlink(copy)

    # A mismatched current source identity reaches only the existing adapter
    # import guard; the native state must be released even on this refusal.
    config <- read_json(case$source_config_path)
    descriptor <- config$sources$nir$descriptor
    source_identity <- jsonlite::fromJSON(descriptor$identity, simplifyVector = FALSE)
    source_identity$xl04_foreign_source <- TRUE
    config$sources$nir$descriptor$identity <- jsonlite::toJSON(source_identity,
      auto_unbox = TRUE, null = "null", digits = I(17L), force = TRUE)
    bad_adapter <- make_adapter(case, config)
    expect_refusal(function() run(case, archive, bad_adapter), bad_adapter)

    # The imported recipe/schema are valid, but one current buffer lacks a
    # requested ID. This fails after actual hydration and exercises native
    # finally/release rather than a constructor-only negative.
    config <- read_json(case$source_config_path)
    config$sources$nir$sample_ids[[1L]] <- "sample:unavailable"
    bad_adapter <- make_adapter(case, config)
    expect_refusal(function() run(case, archive, bad_adapter), bad_adapter)
    ops <- operations(bad_adapter$audit_path)
    stopifnot(sum(ops == "hydrate") >= 1L, sum(ops == "hydrate") == sum(ops == "release"))
  }
  message("PASS public Core U07 native R process replay without Python in PATH")
}
