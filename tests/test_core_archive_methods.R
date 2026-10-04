# Public callback-free native Methods replay. These captures come from actual
# native REFIT/package writers; this test does not construct fitted artifacts.
library(nirs4all)
manifest_path <- Sys.getenv("NIRS4ALL_CORE_R_METHODS_FIXTURE_MANIFEST")
mandatory <- identical(Sys.getenv("NIRS4ALL_REQUIRE_CORE_R_ZIP"), "1")
if (!nzchar(manifest_path)) {
  if (mandatory) stop("XL04 requires actual N4ME/Role V2/V3 archive fixtures", call. = FALSE)
  message("SKIP direct native Methods ZIP replay: qualification fixture manifest absent")
} else {
  stopifnot(!nzchar(Sys.which("python")), !nzchar(Sys.which("python3")))
  fixtures <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  stopifnot(identical(fixtures$schema_version, 1L), length(fixtures$cases) >= 3L)
  versions <- vapply(fixtures$cases, function(case) as.integer(case$archive_version), integer(1))
  families <- vapply(fixtures$cases, `[[`, character(1), "family")
  stopifnot(all(c(2L, 3L) %in% versions),
            all(c("n4me-pls", "role-pls") %in% families),
            any(versions == 3L & families == "n4me-pls"),
            any(versions == 3L & families == "role-pls"),
            any(versions == 2L & families == "role-pls"))
  # The native V2 assembler currently excludes N4ME. A V3 N4ME witness must not
  # be relabelled as a V2 parent; fixture provenance preserves that distinction.
  stopifnot(!any(versions == 2L & families == "n4me-pls"))
  core <- Sys.getenv("NIRS4ALL_CORE_ARCHIVE_CLI")
  native_library <- Sys.getenv("NIRS4ALL_METHODS_LIB_PATH")
  native_sha <- Sys.getenv("NIRS4ALL_METHODS_LIB_SHA256")
  stopifnot(nzchar(core), nzchar(native_library), grepl("^[0-9a-f]{64}$", native_sha))
  read_json <- function(path) jsonlite::fromJSON(path, simplifyVector = FALSE)
  matrix_rows <- function(rows) do.call(rbind, lapply(rows, function(row)
    as.double(unlist(row, use.names = FALSE))))
  # Independent PLS1 closed-form oracle, in test code only. No n4m/nirs4all fit
  # method or replay output contributes coefficients or expected predictions.
  pls1_oracle <- function(X, y, new_X) {
    center <- colMeans(X)
    spread <- apply(X, 2L, stats::sd)
    stopifnot(all(is.finite(X)), all(is.finite(y)), all(is.finite(new_X)),
              nrow(X) == length(y), all(spread > 0), stats::sd(y) > 0)
    scaled_X <- sweep(sweep(X, 2L, center), 2L, spread, "/")
    scaled_y <- (y - mean(y)) / stats::sd(y)
    weights <- drop(crossprod(scaled_X, scaled_y))
    weights <- weights / sqrt(sum(weights * weights))
    scores <- drop(scaled_X %*% weights)
    loading <- sum(scores * scaled_y) / sum(scores * scores)
    new_scaled <- sweep(sweep(new_X, 2L, center), 2L, spread, "/")
    drop(new_scaled %*% weights) * loading * stats::sd(y) + mean(y)
  }
  run <- function(case, archive, request = case$request_path,
      envelopes = case$envelopes_path, inputs = case$methods_inputs_path, sha = native_sha) {
    nirs4all_core_archive_predict_methods(archive, request, envelopes, inputs,
      native_library, sha, outcome_id = paste0("outcome:r.direct.", case$archive_version, ".", case$family),
      run_id = paste0("run:r.direct.", case$archive_version, ".", case$family))
  }
  expect_refusal <- function(action) {
    failure <- tryCatch({ action(); NULL }, error = identity)
    stopifnot(inherits(failure, "error"))
    failure
  }
  for (case in fixtures$cases) {
    archive <- nirs4all_core_archive(case$archive, core_cli = core)
    stopifnot(identical(as.integer(archive$archive$schema_version), as.integer(case$archive_version)))
    original <- digest::digest(file = case$archive, algo = "sha256", serialize = FALSE)
    data <- read_json(case$oracle_data_path)
    stopifnot(identical(data$family, case$family), data$n_components == 1L, identical(data$scale, TRUE))
    X <- matrix_rows(if (case$archive_version == 2L) data$parent_X else data$X)
    y <- as.double(unlist(if (case$archive_version == 2L) data$parent_y else data$y, use.names = FALSE))
    expected <- pls1_oracle(X, y, matrix_rows(data$predict_X))
    outcome <- run(case, archive)
    stopifnot(identical(outcome$phase, "PREDICT"), length(outcome$outputs) == 1L)
    prediction <- outcome$outputs[[1L]]$predictions[[1L]]
    stopifnot(identical(unlist(prediction$sample_ids, use.names = FALSE),
                        unlist(data$predict_ids, use.names = FALSE)),
              identical(unlist(prediction$target_names, use.names = FALSE), "protein"))
    values <- as.double(unlist(prediction$values, use.names = FALSE))
    stopifnot(length(values) == length(expected), all(is.finite(values)),
              max(abs(values - expected)) <= 2e-7,
              identical(original, digest::digest(file = case$archive, algo = "sha256", serialize = FALSE)))
    request <- read_json(case$request_path)
    source_fingerprint <- if (case$archive_version == 2L)
      outcome$source_training_outcome$outcome_fingerprint else
      outcome$source_refit_outcome_fingerprint
    stopifnot(identical(source_fingerprint, request$source_outcome_fingerprint),
              all(vapply(outcome$lineage, function(record)
                identical(record$phase, "PREDICT") &&
                  identical(record$controller_id, case$controller_id) &&
                  length(record$artifact_refs) == 0L, logical(1))))

    bad_request <- request
    bad_request$phase <- "REFIT"
    expect_refusal(function() run(case, archive, request = bad_request))
    bad_request <- request
    bad_request$source_outcome_fingerprint <- paste(rep("0", 64L), collapse = "")
    expect_refusal(function() run(case, archive, request = bad_request))
    envelopes <- read_json(case$envelopes_path)
    envelopes[[1L]]$schema_fingerprint <- paste(rep("0", 64L), collapse = "")
    expect_refusal(function() run(case, archive, envelopes = envelopes))
    inputs <- read_json(case$methods_inputs_path)
    input_key <- names(inputs)[[1L]]
    authority <- read_json(case$envelopes_path)[[input_key]]
    requested_ids <- if (!is.null(authority$predict_cohort))
      unlist(authority$predict_cohort$physical_sample_ids, use.names = FALSE) else
      unique(vapply(authority$coordinator_relations$records, `[[`, character(1), "sample_id"))
    stopifnot(inputs[[1L]]$sample_ids[[1L]] %in% requested_ids)
    inputs[[1L]]$sample_ids[[1L]] <- "predict.foreign"
    expect_refusal(function() run(case, archive, inputs = inputs))
    # Permuting both IDs and their real feature rows preserves the cohort's
    # native prediction order and values, rather than binding rows by position.
    permuted <- read_json(case$methods_inputs_path)
    permuted[[1L]]$sample_ids <- rev(permuted[[1L]]$sample_ids)
    permuted[[1L]]$x <- rev(permuted[[1L]]$x)
    permuted_outcome <- run(case, archive, inputs = permuted)
    permuted_prediction <- permuted_outcome$outputs[[1L]]$predictions[[1L]]
    stopifnot(identical(unlist(permuted_prediction$sample_ids, use.names = FALSE),
      unlist(prediction$sample_ids, use.names = FALSE)), identical(
      as.double(unlist(permuted_prediction$values, use.names = FALSE)), values))
    inputs <- read_json(case$methods_inputs_path)
    inputs[[1L]]$y <- lapply(inputs[[1L]]$sample_ids, function(id) list(17))
    # The list route gives a clear R preflight; an exact JSON file additionally
    # witnesses the owning native target-free guard without trusting R checks.
    expect_refusal(function() run(case, archive, inputs = inputs))
    targets_path <- tempfile(fileext = ".json")
    writeLines(jsonlite::toJSON(inputs, auto_unbox = TRUE, null = "null", force = TRUE,
                              digits = I(17L)), targets_path, useBytes = TRUE)
    expect_refusal(function() run(case, archive, inputs = targets_path))
    unlink(targets_path)
    expect_refusal(function() run(case, archive, sha = paste(rep("0", 64L), collapse = "")))
    copy <- tempfile(fileext = ".n4a")
    stopifnot(file.copy(case$archive, copy))
    changed <- nirs4all_core_archive(copy, core_cli = core)
    connection <- file(copy, open = "ab")
    writeBin(as.raw(1L), connection)
    close(connection)
    expect_refusal(function() run(case, changed))
    unlink(copy)
    # A fresh, successful call after all refusals proves there is no mutable
    # archive/fitted-state carryover into the next native invocation.
    repeat_outcome <- run(case, archive)
    repeated <- as.double(unlist(repeat_outcome$outputs[[1L]]$predictions[[1L]]$values,
                                use.names = FALSE))
    stopifnot(identical(repeated, values), identical(original,
      digest::digest(file = case$archive, algo = "sha256", serialize = FALSE)))
  }
  message("PASS native Core N4ME/Role V2/V3 public R replay, independent PLS1 oracle, no Python in PATH")
}
