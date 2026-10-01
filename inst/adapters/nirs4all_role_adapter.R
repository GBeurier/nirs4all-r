#!/usr/bin/env Rscript
# Published DAG-ML process frames only. All numerics use the native n4m binding.
suppressPackageStartupMessages(library(nirs4all))
args <- commandArgs(trailingOnly = TRUE)
encode <- function(value) as.character(jsonlite::toJSON(value, auto_unbox = TRUE,
  null = "null", digits = I(17L)))
emit <- function(value) { cat(value, "\n", sep = ""); flush(stdout()) }
if (identical(args, "--describe")) {
  emit(encode(list(schema_version = 1L, protocol = "dag-ml-process-adapter",
    adapter_id = "nirs4all-r-methods-role-adapter", supported_modes = list("jsonl"),
    capabilities = list("control_frames_v1", "node_task_json_v1", "node_result_json_v1",
      "parallel_invocation_v1", "persistent_workers", "worker_env",
      "stateful_refit_artifacts", "portable_artifact_bridge_v1"))))
  quit(save = "no", status = 0L)
}
config_path <- Sys.getenv("NIRS4ALL_DAG_ROLE_CONFIG")
if (!nzchar(config_path)) stop("NIRS4ALL_DAG_ROLE_CONFIG is required")
config <- readRDS(config_path)
controller <- nirs4all:::.nirs4all_role_controller(config)
if (identical(args, "--verify")) { controller$close(); quit(save = "no", status = 0L) }
if (!identical(args, "--jsonl")) stop("Methods RAW capture requires a persistent --jsonl worker")
run <- function() {
  on.exit(controller$close(), add = TRUE)
  input <- file("stdin", open = "r")
  on.exit(close(input), add = TRUE)
  initialized <- FALSE
  repeat {
    line <- readLines(input, n = 1L, warn = FALSE)
    if (!length(line)) break
    tryCatch({
      frame <- jsonlite::fromJSON(line, simplifyVector = FALSE)
      nirs4all:::.nirs4all_role_require(identical(frame$schema_version, 1L), "Unsupported process frame schema")
      if (identical(frame$type, "init")) {
        nirs4all:::.nirs4all_role_require(!initialized &&
          identical(frame$controller_id, config$manifest$controller_id), "Wrong or duplicate Methods controller initialization")
        initialized <- TRUE
        emit(encode(list(type = "ack", schema_version = 1L, status = "initialized")))
      } else if (identical(frame$type, "close")) {
        controller$close()
        emit(encode(list(type = "ack", schema_version = 1L, status = "closed")))
        break
      } else {
        nirs4all:::.nirs4all_role_require(initialized && frame$type %in% c("task", "portable_artifact"),
          "Methods adapter requires initialization and a published task frame")
        result <- controller$invoke(frame$task)
        encoded <- encode(list(type = if (identical(frame$type, "task")) "result" else "portable_artifact",
          schema_version = 1L, result = result))
        if (identical(frame$type, "task")) {
          # Retain the native u64 token exactly; R doubles cannot represent it.
          seeds <- regmatches(line, gregexpr('"seed"[[:space:]]*:[[:space:]]*(null|[0-9]+)', line))[[1L]]
          nirs4all:::.nirs4all_role_require(length(seeds) > 0L, "Native task seed is missing")
          seed <- sub('.*:[[:space:]]*', '', tail(seeds, 1L))
          encoded <- sub('"seed":"__DAGML_U64_SEED__"', paste0('"seed":', seed), encoded, fixed = TRUE)
        }
        emit(encoded)
      }
    }, error = function(error) {
      # Abort retained state on failure. Native replay can subsequently report
      # release attempts; no fitted native pointer survives the failed worker.
      controller$close()
      emit(encode(list(type = "error", schema_version = 1L, error = list(
        code = "nirs4all_r_role_adapter_error", message = conditionMessage(error), retryable = FALSE))))
    })
  }
}
run()
