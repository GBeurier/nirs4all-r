# Canonical V1 `.n4a` container operations through `nirs4all-core-archive`.
#
# These functions only transport file paths to the n4a-inspect, n4a-produce,
# n4a-predict, n4a-retrain, n4a-write and n4a-read commands and decode the JSON
# report. The Core,
# DAG and Methods owners decide what can run; R adds no method list, parser,
# compiler, training loop or numerics, and never falls back to the Archive
# V2/V3 or JSON Package routes.

.nirs4all_n4a_condition <- function(message, operation, detail, subclass = character()) {
  structure(class = c(subclass, "nirs4all_n4a_refusal", "error", "condition"),
            list(message = message, call = NULL, operation = operation,
                 detail = detail))
}

# One CLI call; returns the decoded report with the exact native JSON attached.
.nirs4all_n4a_call <- function(core_cli, operation, flags) {
  core_cli <- .nirs4all_native_cli(core_cli)
  directory <- tempfile("nirs4all-n4a-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  output <- file.path(directory, "output.json")
  log <- file.path(directory, "native.log")
  args <- c(operation, "--output", output)
  for (name in names(flags)) {
    if (!is.null(flags[[name]]))
      args <- c(args, paste0("--", gsub("_", "-", name, fixed = TRUE)),
                as.character(flags[[name]]))
  }
  status <- system2(core_cli, vapply(args, shQuote, character(1)),
                    stdout = log, stderr = log)
  detail <- if (file.exists(log)) paste(readLines(log, warn = FALSE), collapse = "\n") else ""
  if (!identical(status, 0L) || !file.exists(output)) {
    if (grepl(paste0("unknown archive command: ", operation), detail, fixed = TRUE))
      stop(.nirs4all_n4a_condition(paste0(
        "The native CLI does not provide ", operation, ". Use a nirs4all-core-archive ",
        "build that includes the canonical n4a-* commands."), operation, detail,
        "nirs4all_n4a_unavailable"))
    stop(.nirs4all_n4a_condition(paste0(operation, " refused (", status, "): ", detail),
                                 operation, detail))
  }
  native_json <- paste(readLines(output, warn = FALSE), collapse = "\n")
  report <- jsonlite::fromJSON(native_json, simplifyVector = FALSE, bigint_as_char = TRUE)
  attr(report, "native_json") <- native_json
  report
}

.nirs4all_n4a_record <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path) ||
      !file.exists(path) || dir.exists(path))
    stop(paste("record_path must be the path of an existing IO dataset JSON file;",
               "datasets and lists are refused because re-encoding would change",
               "the record bytes"), call. = FALSE)
  normalizePath(path, mustWork = TRUE)
}

.nirs4all_n4a_new_path <- function(path, label) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path))
    stop(paste(label, "must be a nonempty file path"), call. = FALSE)
  parent <- dirname(path)
  if (!dir.exists(parent)) stop(paste(label, "needs an existing parent directory"), call. = FALSE)
  file.path(normalizePath(parent, mustWork = TRUE), basename(path))
}

.nirs4all_n4a_runtime <- function(methods_library, methods_library_sha256) {
  ok <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  if (!ok(methods_library) || !ok(methods_library_sha256))
    stop(paste("methods_library and methods_library_sha256 are required (or set",
               "N4M_LIBRARY_PATH and N4M_EXPECTED_LIBRARY_SHA256)"), call. = FALSE)
  list(library = normalizePath(methods_library, mustWork = FALSE),
       sha256 = methods_library_sha256)
}

.nirs4all_n4a_flag <- function(value, label) {
  if (!is.logical(value) || length(value) != 1L || is.na(value))
    stop(paste(label, "must be TRUE or FALSE"), call. = FALSE)
  if (value) "true" else "false"
}

# Optional allocation budget, forwarded as exact decimal text to
# `--memory-budget-bytes`. NULL or "" means no flag and no cap in this host.
# Accepted: a nonnegative whole number up to 2^53 - 1 (numeric or integer), or
# decimal digits (no sign or spaces; leading zeros are valid and kept as given)
# whose value is at most 18446744073709551615, compared as text (the only exact
# way to pass larger budgets). A double above 2^53 - 1 may
# already have been rounded, so it is refused. No default and no RAM detection.
.nirs4all_n4a_budget <- function(value, label = "memory_budget_bytes") {
  if (is.null(value)) return(NULL)
  bad <- function() stop(paste0(label, " must be NULL, a nonnegative whole number up to ",
    "2^53 - 1, or exact decimal text up to 18446744073709551615"), call. = FALSE)
  if (length(value) != 1L || is.na(value)) bad()
  if (is.character(value)) {
    if (!nzchar(value)) return(NULL)
    limit <- "18446744073709551615"
    if (!grepl("^[0-9]+$", value)) bad()
    digits <- sub("^0+(?=.)", "", value, perl = TRUE)  # compare the value without leading zeros
    if (nchar(digits) > nchar(limit)) bad()
    if (nchar(digits) == nchar(limit)) {
      difference <- utf8ToInt(digits) - utf8ToInt(limit)
      first <- which(difference != 0L)[1L]
      if (!is.na(first) && difference[first] > 0L) bad()
    }
    return(value)
  }
  if (!is.numeric(value) || is.factor(value) || !is.finite(value) || value < 0 ||
      value != floor(value) || value > 9007199254740991) bad()
  sprintf("%.0f", value)
}

.nirs4all_n4a_object <- function(object) {
  if (!inherits(object, "nirs4all_n4a") || !is.character(object$path) ||
      !is.character(object$archive_sha256))
    stop("object must come from nirs4all_n4a_inspect, _produce or _retrain", call. = FALSE)
  invisible(object)
}

#' @rdname nirs4all_n4a
#' @export
nirs4all_n4a_inspect <- function(path, core_cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive"),
                                 expected_sha256 = NULL, memory_budget_bytes = NULL) {
  budget <- .nirs4all_n4a_budget(memory_budget_bytes)
  path <- .nirs4all_core_path(path, "path")
  report <- .nirs4all_n4a_call(core_cli, "n4a-inspect",
    list(archive = path, expected_archive_sha256 = expected_sha256,
         memory_budget_bytes = budget))
  structure(list(path = path, archive_sha256 = report$readability$container_sha256,
                 core_cli = .nirs4all_native_cli(core_cli),
                 readability = report$readability, executability = report$executability,
                 native_json = attr(report, "native_json")),
            class = "nirs4all_n4a")
}

#' @rdname nirs4all_n4a
#' @export
nirs4all_n4a_produce <- function(record_path, recipe, archive, source_id = "spectra",
    methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
    methods_library_sha256 = Sys.getenv("N4M_EXPECTED_LIBRARY_SHA256"),
    run_id = paste0("run:r:n4a:produce:", basename(tempfile())),
    container_id = paste0("r-n4a-", basename(tempfile())),
    embed_training_record = FALSE,
    core_cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive"),
    memory_budget_bytes = NULL) {
  budget <- .nirs4all_n4a_budget(memory_budget_bytes)
  runtime <- .nirs4all_n4a_runtime(methods_library, methods_library_sha256)
  record_path <- .nirs4all_n4a_record(record_path)
  archive <- .nirs4all_n4a_new_path(archive, "archive")
  embed <- .nirs4all_n4a_flag(embed_training_record, "embed_training_record")
  directory <- tempfile("nirs4all-n4a-recipe-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  if (is.character(recipe) && length(recipe) == 1L && !is.na(recipe) && file.exists(recipe)) {
    recipe_path <- normalizePath(recipe, mustWork = TRUE)
  } else {
    # JSON text is forwarded as given; a list goes through the existing recipe
    # marshalling that keeps singleton choices. The owner validates the content.
    text <- if (is.character(recipe) && length(recipe) == 1L) recipe else
      .nirs4all_pipeline_recipe_json(recipe)
    recipe_path <- file.path(directory, "recipe.json")
    writeLines(text, recipe_path, useBytes = TRUE)
  }
  report <- .nirs4all_n4a_call(core_cli, "n4a-produce", list(
    input = record_path, recipe = recipe_path, source_id = source_id,
    methods_library = runtime$library, methods_library_sha256 = runtime$sha256,
    run_id = run_id, container_id = container_id, archive = archive,
    embed_training_record = embed, memory_budget_bytes = budget))
  structure(list(path = archive, archive_sha256 = report$archive_sha256,
                 core_cli = .nirs4all_native_cli(core_cli), source_id = source_id,
                 training_outcome = report$training_outcome, report = report,
                 native_json = attr(report, "native_json")),
            class = "nirs4all_n4a")
}

#' @rdname nirs4all_n4a
#' @export
nirs4all_n4a_predict <- function(object, record_path = NULL, partition = NULL,
    methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
    methods_library_sha256 = Sys.getenv("N4M_EXPECTED_LIBRARY_SHA256"),
    source_id = NULL, entrypoint = NULL,
    request_id = paste0("req:r:n4a:predict:", basename(tempfile())),
    run_id = paste0("run:r:n4a:predict:", basename(tempfile())),
    core_cli = NULL, memory_budget_bytes = NULL) {
  budget <- .nirs4all_n4a_budget(memory_budget_bytes)
  .nirs4all_n4a_object(object)
  runtime <- .nirs4all_n4a_runtime(methods_library, methods_library_sha256)
  external <- !is.null(record_path)
  if (external) record_path <- .nirs4all_n4a_record(record_path)
  if (is.null(entrypoint)) entrypoint <- if (external) "predict-external" else "predict"
  if (is.null(partition)) partition <- if (external) "predict" else "train"
  if (is.null(source_id)) source_id <- if (is.null(object$source_id)) "spectra" else object$source_id
  if (is.null(core_cli)) core_cli <- object$core_cli
  report <- .nirs4all_n4a_call(core_cli, "n4a-predict", list(
    archive = object$path, expected_archive_sha256 = object$archive_sha256,
    entrypoint = entrypoint, input = record_path, source_id = source_id,
    partition = partition, request_id = request_id, run_id = run_id,
    methods_library = runtime$library, methods_library_sha256 = runtime$sha256,
    memory_budget_bytes = budget))
  sample_ids <- vapply(report$sample_ids, as.character, character(1))
  target_names <- vapply(report$target_names, as.character, character(1))
  units <- vapply(report$target_units, function(unit)
    if (is.null(unit)) NA_character_ else as.character(unit), character(1))
  # Rows are already in the physical record order, aligned once by the CLI.
  values <- matrix(as.double(unlist(report$values, use.names = FALSE)),
                   nrow = length(sample_ids), byrow = TRUE,
                   dimnames = list(sample_ids, target_names))
  structure(list(sample_ids = sample_ids, target_names = target_names,
                 target_units = units, values = values, source_id = report$source_id,
                 partition = report$partition, input_mode = report$input_mode,
                 archive_sha256 = report$archive_sha256, replay = report$replay,
                 receipt = report$receipt, native_json = attr(report, "native_json")),
            class = "nirs4all_n4a_prediction")
}

#' @rdname nirs4all_n4a
#' @export
nirs4all_n4a_retrain <- function(object, record_path, archive,
    methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
    methods_library_sha256 = Sys.getenv("N4M_EXPECTED_LIBRARY_SHA256"),
    run_id = paste0("run:r:n4a:retrain:", basename(tempfile())),
    container_id = paste0("r-n4a-", basename(tempfile())),
    embed_training_record = FALSE, core_cli = NULL, memory_budget_bytes = NULL) {
  budget <- .nirs4all_n4a_budget(memory_budget_bytes)
  .nirs4all_n4a_object(object)
  runtime <- .nirs4all_n4a_runtime(methods_library, methods_library_sha256)
  record_path <- .nirs4all_n4a_record(record_path)
  archive <- .nirs4all_n4a_new_path(archive, "archive")
  embed <- .nirs4all_n4a_flag(embed_training_record, "embed_training_record")
  if (is.null(core_cli)) core_cli <- object$core_cli
  # The owner takes the source ID and the stored recipe from the validated package.
  report <- .nirs4all_n4a_call(core_cli, "n4a-retrain", list(
    archive = object$path, expected_archive_sha256 = object$archive_sha256,
    input = record_path, run_id = run_id,
    container_id = container_id, destination = archive,
    embed_training_record = embed, methods_library = runtime$library,
    methods_library_sha256 = runtime$sha256, memory_budget_bytes = budget))
  structure(list(path = archive, archive_sha256 = report$archive_sha256,
                 core_cli = .nirs4all_native_cli(core_cli),
                 source_id = report$training_source_id,
                 source_archive_sha256 = report$source_archive_sha256,
                 training_outcome = report$training_outcome, report = report,
                 native_json = attr(report, "native_json")),
            class = "nirs4all_n4a")
}

# ---------------------------------------------------------------------------
# Owner-neutral exchange: write, update and read arbitrary members.
#
# The manifest and the member list are exact JSON files and the members are
# exact binary files; nothing is encoded, parsed or deserialized in R, and no
# model, Methods or numerical code is involved. Core stores and returns bytes.
# ---------------------------------------------------------------------------

.nirs4all_n4a_file <- function(path, label) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path) ||
      !file.exists(path) || dir.exists(path))
    stop(paste(label, "must be the path of an existing file; lists and in-memory",
               "values are refused because re-encoding would change the bytes"),
         call. = FALSE)
  normalizePath(path, mustWork = TRUE)
}

.nirs4all_n4a_text <- function(value, label) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value))
    stop(paste(label, "must be a nonempty character string"), call. = FALSE)
  value
}

#' @rdname nirs4all_n4a
#' @export
nirs4all_n4a_write <- function(manifest_path, members_path, destination,
    base = NULL, base_sha256 = NULL,
    core_cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive"),
    memory_budget_bytes = NULL) {
  budget <- .nirs4all_n4a_budget(memory_budget_bytes)
  manifest_path <- .nirs4all_n4a_file(manifest_path, "manifest_path")
  members_path <- .nirs4all_n4a_file(members_path, "members_path")
  destination <- .nirs4all_n4a_new_path(destination, "destination")
  if (xor(is.null(base), is.null(base_sha256)))
    stop("base and base_sha256 must be given together", call. = FALSE)
  if (!is.null(base)) {
    base <- .nirs4all_n4a_file(base, "base")
    base_sha256 <- .nirs4all_n4a_text(base_sha256, "base_sha256")
  }
  report <- .nirs4all_n4a_call(core_cli, "n4a-write", list(
    manifest = manifest_path, members = members_path, destination = destination,
    archive = base, expected_archive_sha256 = base_sha256,
    memory_budget_bytes = budget))
  native_json <- attr(report, "native_json")
  attr(report, "native_json") <- NULL
  structure(list(path = destination, base = base, report = report,
                 native_json = native_json), class = "nirs4all_n4a_exchange")
}

#' @rdname nirs4all_n4a
#' @export
nirs4all_n4a_read <- function(path, directory, expected_sha256 = NULL,
    member = NULL, artifact = NULL,
    core_cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive"),
    memory_budget_bytes = NULL) {
  budget <- .nirs4all_n4a_budget(memory_budget_bytes)
  path <- .nirs4all_n4a_file(path, "path")
  directory <- .nirs4all_n4a_new_path(directory, "directory")
  if (!is.null(member) && !is.null(artifact))
    stop("member and artifact are mutually exclusive", call. = FALSE)
  if (!is.null(member)) member <- .nirs4all_n4a_text(member, "member")
  if (!is.null(artifact)) artifact <- .nirs4all_n4a_text(artifact, "artifact")
  if (!is.null(expected_sha256))
    expected_sha256 <- .nirs4all_n4a_text(expected_sha256, "expected_sha256")
  report <- .nirs4all_n4a_call(core_cli, "n4a-read", list(
    archive = path, expected_archive_sha256 = expected_sha256, member = member,
    artifact = artifact, directory = directory, memory_budget_bytes = budget))
  native_json <- attr(report, "native_json")
  attr(report, "native_json") <- NULL
  # Archive paths are never used as OS paths: the owner numbers the files
  # (member-000000.bin, ...) and writes the exact manifest as manifest.json.
  files <- sort(list.files(directory, pattern = "^member-[0-9]+[.]bin$", full.names = TRUE))
  manifest_file <- file.path(directory, "manifest.json")
  structure(list(directory = directory, member_files = files,
                 manifest_file = if (file.exists(manifest_file)) manifest_file else NULL,
                 report = report, native_json = native_json),
            class = "nirs4all_n4a_exchange")
}
