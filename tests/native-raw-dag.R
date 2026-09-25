strict <- identical(Sys.getenv("NIRS4ALL_REQUIRE_DAG_PARITY"), "1")
cli <- Sys.getenv("NIRS4ALL_DAGML_CLI", "")
if (!nzchar(cli)) cli <- Sys.which("dag-ml-cli")
available <- nzchar(cli) && file.exists(cli) &&
  requireNamespace("dagml", quietly = TRUE)
if (!available && strict) stop("strict native DAG parity requires dagml and dag-ml-cli")

if (available) {
  library(nirs4all)
  X <- outer(seq_len(18L), seq_len(9L),
             function(i, j) sin(i * j / 11) + cos(i / 3 + j) + i * j / 170)
  colnames(X) <- paste0("wl", seq_len(ncol(X)))
  y <- 2 + 0.7 * X[, 2L] - 0.4 * X[, 7L]
  X_test <- X[c(1L, 7L, 15L), , drop = FALSE] + 0.017
  python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON", "")
  profiles <- list(
    plain_pls = list(pipeline = nirs4all_pipeline(learner = nirs4all_pls(2L)),
                     mode = "predict_plain", algorithm = 0L, format = 1L),
    embedded_pls = list(pipeline = nirs4all_pipeline(
      list(nirs4all_snv(), nirs4all_savgol(5L)), nirs4all_pls(2L)),
      mode = "predict", algorithm = 0L, format = 2L),
    ridge = list(pipeline = nirs4all_pipeline(
      learner = nirs4all_n4m_method("ridge", params = list(ridge_lambda = 0.5))),
      mode = "predict_affine", algorithm = 11L, format = 1L))
  for (name in names(profiles)) {
    profile <- profiles[[name]]
    outcome <- nirs4all_dag_cv_refit_predict(profile$pipeline, X, y,
      folds = 3L, cli = cli, process_workers = 2L)
    artifact <- outcome$bundle$refit_artifacts[[1L]]$artifact
    stopifnot(identical(artifact$backend, "raw"),
              identical(artifact$kind, "n4m_model"),
              length(outcome$bundle$raw_artifact_payloads) == 1L,
              identical(outcome$feature_names, colnames(X)))
    payload <- outcome$bundle$raw_artifact_payloads[[artifact$id]]
    bytes <- as.raw(as.integer(unlist(payload, use.names = FALSE)))
    descriptor <- n4m::n4m_model_descriptor(bytes)
    stopifnot(identical(descriptor$algorithm, profile$algorithm),
              identical(descriptor$format_version, profile$format),
              identical(artifact$content_fingerprint,
                digest::digest(bytes, algo = "sha256", serialize = FALSE)))
    expected <- predict(nirs4all_fit(profile$pipeline, X, y), X_test)
    stopifnot(max(abs(nirs4all_dag_predict(outcome, X_test) - expected)) < 1e-10)
    detached <- outcome
    detached$workdir <- tempfile("nirs4all-absent-workdir-")
    stopifnot(!dir.exists(detached$workdir),
              max(abs(nirs4all_dag_predict(detached, X_test) - expected)) < 1e-10)
    r_peer <- if (file.exists("helpers/native_raw_dag_peer.R"))
      "helpers/native_raw_dag_peer.R" else "tests/helpers/native_raw_dag_peer.R"
    rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows")
      "Rscript.exe" else "Rscript")
    outcome_file <- tempfile(fileext = ".rds")
    X_file <- tempfile(fileext = ".rds")
    r_result <- tempfile(fileext = ".json")
    saveRDS(detached, outcome_file)
    saveRDS(X_test, X_file)
    r_output <- suppressWarnings(system2(rscript,
      c(shQuote(r_peer), shQuote(outcome_file), shQuote(X_file),
        shQuote(r_result)), stdout = TRUE, stderr = TRUE))
    r_status <- attr(r_output, "status")
    if (!is.null(r_status) && r_status != 0L)
      stop("fresh R N4MM replay failed: ", paste(r_output, collapse = "\n"))
    stopifnot(max(abs(jsonlite::fromJSON(r_result) - expected)) < 1e-10)
    unlink(c(outcome_file, X_file, r_result))
    if (identical(name, "plain_pls")) {
      probe_bridge <- function() {
        keys <- c("NIRS4ALL_DAG_DATA_RDS", "NIRS4ALL_DAG_DSL",
                  "NIRS4ALL_DAG_ENVELOPE", "NIRS4ALL_DAG_ARTIFACT_DIR")
        old <- Sys.getenv(keys, unset = NA_character_)
        on.exit(for (i in seq_along(keys)) {
          if (is.na(old[[i]])) Sys.unsetenv(keys[[i]])
          else do.call(Sys.setenv, stats::setNames(list(old[[i]]), keys[[i]]))
        }, add = TRUE)
        paths <- file.path(outcome$workdir,
          c("data.rds", "dsl.json", "envelope.json", "artifacts"))
        do.call(Sys.setenv, stats::setNames(as.list(paths), keys))
        adapter <- file.path(outcome$workdir, if (.Platform$OS.type == "windows")
          "run-r-adapter.cmd" else "run-r-adapter")
        frames <- list(
          list(type = "init", schema_version = 1L,
               controller_id = "controller:nirs4all-r", worker_index = 0L,
               worker_count = 1L),
          list(type = "portable_artifact", schema_version = 1L,
               task = list(operation = "hydrate_artifact_payload",
                           schema_version = 1L,
                           request = list(node_id = "model:nirs4all-r",
                             controller_id = "controller:nirs4all-r",
                             artifact = artifact),
                           payload = as.list(as.integer(bytes)))),
          list(type = "portable_artifact", schema_version = 1L,
               task = list(operation = "release_hydrated_artifact_payload",
                           schema_version = 1L,
                           handle = list(handle = 2000000001L, kind = "model",
                             owner_controller = "controller:nirs4all-r"))),
          list(type = "close", schema_version = 1L))
        frame_file <- tempfile(fileext = ".jsonl")
        writeLines(vapply(frames, function(frame)
          as.character(jsonlite::toJSON(frame, auto_unbox = TRUE)), ""), frame_file)
        on.exit(unlink(frame_file), add = TRUE)
        output <- suppressWarnings(system2(adapter, "--jsonl", stdin = frame_file,
                                            stdout = TRUE, stderr = TRUE))
        status <- attr(output, "status")
        if (!is.null(status) && status != 0L)
          stop("R portable bridge failed: ", paste(output, collapse = "\n"))
        replies <- lapply(output, jsonlite::fromJSON, simplifyVector = FALSE)
        if (length(replies) != 4L)
          stop("unexpected R portable bridge replies: ",
               paste(output, collapse = "\n"))
        stopifnot(
                  identical(replies[[2L]]$type, "portable_artifact"),
                  identical(replies[[2L]]$result$operation,
                            "hydrated_artifact_payload"),
                  identical(replies[[3L]]$result$operation,
                            "released_hydrated_artifact_payload"))
      }
      probe_bridge()
    }
    changed <- outcome
    changed$bundle$raw_artifact_payloads[[artifact$id]][[1L]] <-
      (as.integer(payload[[1L]]) + 1L) %% 256L
    stopifnot(inherits(try(nirs4all_dag_predict(changed, X_test),
                           silent = TRUE), "try-error"))
    stopifnot(inherits(try(nirs4all_dag_predict(
      outcome, X_test[, rev(seq_len(ncol(X_test)))]), silent = TRUE), "try-error"))
    if (nzchar(python)) {
      helper <- if (file.exists("helpers/native_n4mm_peer.py"))
        "helpers/native_n4mm_peer.py" else "tests/helpers/native_n4mm_peer.py"
      model_file <- tempfile(fileext = ".n4mm")
      request_file <- tempfile(fileext = ".json")
      result_file <- tempfile(fileext = ".json")
      writeBin(bytes, model_file)
      request <- list(
        X = lapply(seq_len(nrow(X)), function(i) unname(as.numeric(X[i, ]))),
        predict_X = lapply(seq_len(nrow(X_test)), function(i)
          unname(as.numeric(X_test[i, ]))),
        y = unname(as.numeric(y)))
      writeLines(as.character(jsonlite::toJSON(request, auto_unbox = FALSE,
                                                digits = NA)), request_file)
      output <- suppressWarnings(system2(python,
        c(shQuote(helper), profile$mode, shQuote(model_file),
          shQuote(request_file), shQuote(result_file)),
        stdout = TRUE, stderr = TRUE))
      status <- attr(output, "status")
      if (!is.null(status) && status != 0L)
        stop("Python N4MM DAG replay failed: ", paste(output, collapse = "\n"))
      actual <- jsonlite::fromJSON(result_file)$predictions
      stopifnot(max(abs(actual - expected)) < 1e-10)
      unlink(c(model_file, request_file, result_file))
    }
  }
  mixed <- nirs4all_dag_cv_refit_predict(nirs4all_pipeline(
    list(nirs4all_snv()), nirs4all_n4m_method("ridge")),
    X, y, folds = 3L, cli = cli)
  stopifnot(identical(mixed$bundle$refit_artifacts[[1L]]$artifact$backend, "rds"),
            !length(mixed$bundle$raw_artifact_payloads))
}
