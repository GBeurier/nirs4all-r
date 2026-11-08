# Owner-neutral .n4a exchange (n4a-write / n4a-read) through the native Core CLI.
#
# Real CLI, no Methods runtime, no model code. The inputs are the files of the
# Core owner fixture bindings/rust/nirs4all/tests/parity/fixtures/n4a_host_opaque_v1
# (layout and report fields: Core author PUBLIC_API.md):
#
#   manifest.json, members.json (member "file" paths are relative to this
#   directory), members/*, expected.json, update/{manifest.json,members.json,members/model.bin}
#
# Environment:
#   NIRS4ALL_CORE_CLI              nirs4all-core-archive that provides n4a-write/read
#   NIRS4ALL_N4A_HOST_FIXTURE_DIR  that fixture directory (copied into a path with spaces)
#   NIRS4ALL_REQUIRE_CORE_N4A=1    qualification mode: any missing prerequisite is an error
#
# Golden containers: expected.json holds expected$container (base) and
# expected$update$container (updated archive), each with byte_length,
# container_sha256 and manifest_sha256, filled from the real owner writer. They
# are REQUIRED: a missing, null or malformed golden fails the test (no bypass).
# Each is compared with the owner write and read reports, the actual archive
# file hash and length, and the SHA-256 of the exact manifest.json read back
# from the archive. Determinism (two writes give the same bytes) is also kept.
# Per-member lengths and SHA-256 come from expected.json.
# Nothing is claimed as passed unless R_N4A_EXCHANGE_PASS is printed.
library(nirs4all)

env <- function(name) Sys.getenv(name, "")
required <- identical(env("NIRS4ALL_REQUIRE_CORE_N4A"), "1")
refusal <- function(expr) tryCatch({ expr; NULL }, nirs4all_n4a_refusal = function(e) e)
failure <- function(expr) tryCatch({ expr; NULL }, error = function(e) e)

# --- Argument checks that need no native executable -------------------------
some_file <- tempfile()
writeLines("x", some_file)
stopifnot(inherits(failure(nirs4all_n4a_write(list(a = 1), some_file, tempfile())), "error"),
          inherits(failure(nirs4all_n4a_write(some_file, some_file, tempfile(),
                                              base = some_file)), "error"),
          inherits(failure(nirs4all_n4a_read(some_file, tempfile(), member = "a",
                                             artifact = "b")), "error"),
          inherits(failure(nirs4all_n4a_read(tempfile(), tempfile())), "error"))

# --- Memory budget: validation and exact forwarding (stub CLI, no native work) ---
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
stopifnot(grepl("memory_budget_bytes", failure(nirs4all_n4a_read(stub_input, file.path(stub_dir, "d0"), memory_budget_bytes = 1.5))$message, fixed = TRUE))
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
  stopifnot(sent(nirs4all_n4a_write(stub_input, stub_input, file.path(stub_dir, "w.n4a"),
                                    core_cli = stub, memory_budget_bytes = 7), "7"),
            sent(nirs4all_n4a_read(stub_input, file.path(stub_dir, "d"), core_cli = stub,
                                   memory_budget_bytes = big), big),
            sent(nirs4all_n4a_read(stub_input, file.path(stub_dir, "d2"), core_cli = stub,
                                   memory_budget_bytes = "4096"), "4096"),
            none(nirs4all_n4a_read(stub_input, file.path(stub_dir, "d3"), core_cli = stub)),
            none(nirs4all_n4a_read(stub_input, file.path(stub_dir, "d4"), core_cli = stub,
                                   memory_budget_bytes = "")))
}
unlink(stub_dir, recursive = TRUE)

needed <- c("NIRS4ALL_CORE_CLI", "NIRS4ALL_N4A_HOST_FIXTURE_DIR")
missing <- needed[!nzchar(vapply(needed, env, ""))]
if (length(missing)) {
  if (required) stop("Missing required environment: ", paste(missing, collapse = ", "), call. = FALSE)
  message("NOT RUN n4a-exchange: ", paste(missing, collapse = ", "),
          " not set (depends on the Core fixture n4a_host_opaque_v1); no native assertion ran")
} else local({
  cli <- env("NIRS4ALL_CORE_CLI")
  # A directory with spaces exercises argument quoting; the fixture keeps its
  # relative layout inside it, so the owner's own member list is used as is.
  work <- tempfile("nirs4all n4a exchange ")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  at <- function(...) file.path(work, ...)
  fixture <- at("host fixture")
  dir.create(fixture)
  stopifnot(all(file.copy(list.files(env("NIRS4ALL_N4A_HOST_FIXTURE_DIR"), full.names = TRUE),
                          fixture, recursive = TRUE)))
  file_sha <- function(path) digest::digest(path, algo = "sha256", file = TRUE)
  bytes <- function(path) readBin(path, "raw", file.size(path))
  member_list <- function(list_file) {
    entries <- jsonlite::fromJSON(list_file, simplifyVector = FALSE)
    paths <- vapply(entries, function(m) m$path, "")
    files <- file.path(dirname(list_file), vapply(entries, function(m) m$file, ""))
    stopifnot(all(file.exists(files)))
    list(paths = paths, files = unname(files),
         bytes = stats::setNames(lapply(files, bytes), paths))
  }
  ran <- character()
  # Required golden container: non-null 64-hex hashes and a positive integer
  # length that equal the owner reports, the archive file and the exact manifest
  # read back from the archive.
  check_container <- function(label, golden, write_report, read_report, archive, manifest_file) {
    hex64 <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && grepl("^[0-9a-f]{64}$", x)
    n <- golden$byte_length
    if (!(is.numeric(n) && length(n) == 1L && is.finite(n) && n > 0 && n == floor(n)))
      stop(label, ": expected.json byte_length must be a positive integer (golden not filled in)", call. = FALSE)
    for (field in c("container_sha256", "manifest_sha256"))
      if (!hex64(golden[[field]]))
        stop(label, ": expected.json ", field, " must be 64 lowercase hex (golden not filled in)", call. = FALSE)
    for (report in list(write_report, read_report))
      if (!(identical(report$archive_sha256, golden$container_sha256) &&
            is.numeric(report$byte_length) && as.numeric(report$byte_length) == n &&
            identical(report$manifest_sha256, golden$manifest_sha256)))
        stop(label, ": owner ", report$operation, " report differs from the expected.json container", call. = FALSE)
    if (!(identical(file_sha(archive), golden$container_sha256) && file.size(archive) == n &&
          identical(file_sha(manifest_file), golden$manifest_sha256)))
      stop(label, ": archive file or its exact manifest.json differs from the expected.json container",
           call. = FALSE)
    invisible(TRUE)
  }

  # --- CLI-only refusals ------------------------------------------------------
  empty_manifest <- at("empty manifest.json"); writeLines("{}", empty_manifest)
  no_members <- at("no members.json"); writeLines("[]", no_members)
  stopifnot(!is.null(refusal(nirs4all_n4a_write(empty_manifest, no_members,
                                                at("never written.n4a"), core_cli = cli))),
            !file.exists(at("never written.n4a")))
  garbage <- at("garbage.n4a"); writeLines("not a container", garbage)
  stopifnot(!is.null(refusal(nirs4all_n4a_read(garbage, at("never extracted"), core_cli = cli))),
            !dir.exists(at("never extracted")))
  ran <- c(ran, "cli_refusals")

  # --- Write the owner fixture, byte for byte ---------------------------------
  manifest <- file.path(fixture, "manifest.json")
  members <- file.path(fixture, "members.json")
  owned <- member_list(members)
  expected_paths <- paste0("host/", c("recipe.json", "data.csv", "model.bin",
                                      "foreign-model.bin", "scores.json", "results.json"))
  stopifnot(all(expected_paths %in% owned$paths))
  # Fixture precondition: opaque binary with NUL, 0xFF and CRLF.
  model <- as.integer(owned$bytes[["host/model.bin"]])
  stopifnot(any(model == 0L), any(model == 255L), any(head(model, -1L) == 13L & tail(model, -1L) == 10L))

  # The fixture's own expected.json (per-member length and SHA-256, update case,
  # refusal names) must agree with the member files and with the report.
  expected <- jsonlite::fromJSON(file.path(fixture, "expected.json"), simplifyVector = FALSE)
  exp_paths <- vapply(expected$members, function(m) m$path, "")
  stopifnot(identical(exp_paths, expected_paths))
  for (m in expected$members) {
    stopifnot(identical(digest::digest(owned$bytes[[m$path]], algo = "sha256", serialize = FALSE), m$sha256),
              length(owned$bytes[[m$path]]) == m$byte_length)
  }

  a_path <- at("host a.n4a")
  written <- nirs4all_n4a_write(manifest, members, a_path, core_cli = cli)
  sha_a <- file_sha(a_path)
  wrep <- written$report
  stopifnot(file.exists(a_path), identical(written$path, normalizePath(a_path)),
            identical(wrep$operation, "n4a-write"), identical(wrep$archive_sha256, sha_a),
            as.numeric(wrep$byte_length) == file.size(a_path), is.null(wrep$base_archive_sha256),
            length(wrep$members) == length(owned$paths))
  # The report lists the written inventory in manifest order, as in expected.json.
  stopifnot(identical(vapply(wrep$members, function(m) m$path, ""), expected_paths))
  for (m in expected$members) {
    got <- Filter(function(r) identical(r$path, m$path), wrep$members)[[1L]]
    stopifnot(identical(got$sha256, m$sha256), as.numeric(got$byte_length) == m$byte_length)
  }
  # No overwrite, and the same inputs give the same container.
  stopifnot(!is.null(refusal(nirs4all_n4a_write(manifest, members, a_path, core_cli = cli))),
            identical(file_sha(a_path), sha_a))
  nirs4all_n4a_write(manifest, members, at("host b.n4a"), core_cli = cli)
  stopifnot(identical(file_sha(at("host b.n4a")), sha_a))
  # Without a base the member list must cover the manifest; an update list is a
  # subset, so the same list used as a fresh write is refused (no output).
  stopifnot(!is.null(refusal(nirs4all_n4a_write(manifest, file.path(fixture, "update", "members.json"),
                                                at("host uncovered.n4a"), core_cli = cli))),
            !file.exists(at("host uncovered.n4a")))

  # --- Readable, not executable -----------------------------------------------
  info <- nirs4all_n4a_inspect(a_path, core_cli = cli, expected_sha256 = sha_a)
  stopifnot(identical(info$executability, "not_evaluated"),
            identical(info$readability$storage, "verified"),
            identical(info$archive_sha256, sha_a))
  # The host entrypoint belongs to an unknown owner: prediction is refused (the
  # runtime arguments are deliberately unusable, so no runtime can have run).
  # It must be the OWNER refusal; a missing-runtime error would also be a refusal
  # but would show that the runtime was reached before the owner check.
  predict_refusal <- refusal(nirs4all_n4a_predict(info, some_file, entrypoint = expected$refusals$entrypoint_id,
    methods_library = "/missing/n4a-exchange/libn4m.so",
    methods_library_sha256 = strrep("0", 64L)))
  stopifnot(!is.null(predict_refusal),
            grepl("owner content for `model` is unresolved", conditionMessage(predict_refusal), fixed = TRUE),
            !grepl("Methods owner runtime is unavailable", conditionMessage(predict_refusal), fixed = TRUE),
            !grepl("/missing/n4a-exchange/libn4m.so", conditionMessage(predict_refusal), fixed = TRUE))

  # --- Read: whole container, then member by member ---------------------------
  whole <- nirs4all_n4a_read(a_path, at("read all"), expected_sha256 = sha_a, core_cli = cli)
  wr <- whole$report
  stopifnot(identical(wr$operation, "n4a-read"), identical(wr$archive_sha256, sha_a),
            identical(wr$selection$kind, "all"), identical(wr$manifest_file, "manifest.json"),
            length(wr$members) == length(owned$paths), !is.null(whole$manifest_file),
            length(whole$member_files) == length(owned$paths))
  # Numbering follows the report's members array = manifest inventory order.
  stopifnot(identical(vapply(wr$members, function(m) m$path, ""), expected_paths),
            identical(vapply(wr$members, function(m) m$file, ""),
                      sprintf("member-%06d.bin", seq_along(expected_paths) - 1L)))
  for (m in wr$members) {
    stopifnot(identical(bytes(file.path(whole$directory, m$file)), owned$bytes[[m$path]]))
  }
  check_container("base container", expected$container, wrep, wr, a_path, whole$manifest_file)
  for (path in owned$paths) {
    safe <- gsub("[^A-Za-z0-9]", "_", path)
    one <- nirs4all_n4a_read(a_path, at(paste("read member", safe)), member = path,
                             expected_sha256 = sha_a, core_cli = cli)
    stopifnot(identical(basename(one$member_files), "member-000000.bin"),
              identical(bytes(one$member_files), owned$bytes[[path]]),
              identical(one$report$selection$kind, "member"),
              identical(bytes(one$manifest_file), bytes(whole$manifest_file)))
  }
  # By artifact: "model" owns host/model.bin; the reference artifact "dataset-ref"
  # has no embedded member, so it yields none (and still writes manifest.json).
  by_model <- nirs4all_n4a_read(a_path, at("read artifact model"), artifact = "model", core_cli = cli)
  stopifnot(identical(by_model$report$selection$kind, "artifact"),
            identical(by_model$report$selection$value, "model"),
            length(by_model$report$members) == 1L,
            identical(by_model$report$members[[1L]]$path, "host/model.bin"),
            identical(bytes(by_model$member_files), owned$bytes[["host/model.bin"]]))
  by_reference <- nirs4all_n4a_read(a_path, at("read artifact reference"), artifact = "dataset-ref",
                                    core_cli = cli)
  stopifnot(length(by_reference$report$members) == 0L, length(by_reference$member_files) == 0L,
            !is.null(by_reference$manifest_file))
  # Reads never touch the source; a used directory and unknown names are refused.
  stopifnot(identical(file_sha(a_path), sha_a),
            !is.null(refusal(nirs4all_n4a_read(a_path, at("read all"), core_cli = cli))),
            !is.null(refusal(nirs4all_n4a_read(a_path, at("read missing"),
                                               member = expected$refusals$unknown_member,
                                               core_cli = cli))),
            !dir.exists(at("read missing")),
            !is.null(refusal(nirs4all_n4a_read(a_path, at("read unknown artifact"),
                                               artifact = "no-such-artifact", core_cli = cli))),
            !dir.exists(at("read unknown artifact")))

  # --- Tampered and missing archives ------------------------------------------
  tampered <- at("tampered.n4a")
  raw <- bytes(a_path)
  middle <- length(raw) %/% 2L
  raw[middle] <- as.raw(bitwXor(as.integer(raw[middle]), 1L))
  writeBin(raw, tampered)
  stopifnot(!is.null(refusal(nirs4all_n4a_read(tampered, at("read tampered"),
                                               expected_sha256 = sha_a, core_cli = cli))),
            !dir.exists(at("read tampered")),
            !is.null(refusal(nirs4all_n4a_read(tampered, at("read tampered 2"), core_cli = cli))),
            !dir.exists(at("read tampered 2")),
            !is.null(refusal(nirs4all_n4a_read(a_path, at("read wrong sha"),
                                               expected_sha256 = strrep("0", 64L), core_cli = cli))),
            inherits(failure(nirs4all_n4a_read(at("missing.n4a"), at("read none"),
                                               core_cli = cli)), "error"))
  ran <- c(ran, "roundtrip")

  # --- Update: immutable base, new container, never overwritten ---------------
  same <- nirs4all_n4a_write(manifest, members, at("host c.n4a"), base = a_path,
                             base_sha256 = sha_a, core_cli = cli)
  stopifnot(identical(same$report$base_archive_sha256, sha_a),
            identical(file_sha(at("host c.n4a")), sha_a), identical(file_sha(a_path), sha_a))
  upd_manifest <- file.path(fixture, "update", "manifest.json")
  upd_members <- file.path(fixture, "update", "members.json")
  update <- member_list(upd_members)
  d_path <- at("host d.n4a")
  updated <- nirs4all_n4a_write(upd_manifest, upd_members, d_path, base = a_path,
                                base_sha256 = sha_a, core_cli = cli)
  sha_d <- file_sha(d_path)
  urep <- updated$report
  replaced <- expected$update$replaced[[1L]]
  stopifnot(!identical(sha_d, sha_a), identical(file_sha(a_path), sha_a),
            identical(urep$base_archive_sha256, sha_a), identical(urep$archive_sha256, sha_d),
            identical(update$paths, replaced$path),
            identical(digest::digest(update$bytes[[replaced$path]], algo = "sha256", serialize = FALSE),
                      replaced$sha256),
            length(update$bytes[[replaced$path]]) == replaced$byte_length)
  # The written inventory is the update manifest's: replaced + retained, no deleted.
  kept <- unlist(expected$update$retained_from_base)
  deleted <- unlist(expected$update$deleted)
  reported <- vapply(urep$members, function(m) m$path, "")
  stopifnot(identical(sort(reported), sort(c(replaced$path, kept))), !any(deleted %in% reported),
            identical(reported, setdiff(expected_paths, deleted)))
  for (m in urep$members) {
    want <- if (identical(m$path, replaced$path)) update$bytes[[m$path]] else owned$bytes[[m$path]]
    stopifnot(identical(m$sha256, digest::digest(want, algo = "sha256", serialize = FALSE)))
  }
  whole_d <- nirs4all_n4a_read(d_path, at("read d all"), expected_sha256 = sha_d, core_cli = cli)
  check_container("updated container", expected$update$container, urep, whole_d$report, d_path,
                  whole_d$manifest_file)
  for (path in reported) {
    got <- nirs4all_n4a_read(d_path, at(paste("read d", gsub("[^A-Za-z0-9]", "_", path))),
                             member = path, expected_sha256 = sha_d, core_cli = cli)
    # Replaced member: the update bytes. Retained members: the base bytes.
    want <- if (identical(path, replaced$path)) update$bytes[[path]] else owned$bytes[[path]]
    stopifnot(identical(bytes(got$member_files), want))
  }
  stopifnot(!identical(update$bytes[[replaced$path]], owned$bytes[[replaced$path]]))
  for (path in deleted) {
    stopifnot(!is.null(refusal(nirs4all_n4a_read(d_path, at("read deleted"), member = path,
                                                 core_cli = cli))),
              !dir.exists(at("read deleted")))
    # ...while the immutable base still holds it.
    still <- nirs4all_n4a_read(a_path, at("read deleted in base"), member = path, core_cli = cli)
    stopifnot(identical(bytes(still$member_files), owned$bytes[[path]]))
  }
  # Owner refusals during an update: undeclared and duplicate replacements.
  undeclared <- at("undeclared members.json")
  writeLines(as.character(jsonlite::toJSON(list(list(path = expected$refusals$undeclared_replacement,
    file = file.path(fixture, "update", "members", "model.bin"))), auto_unbox = TRUE)), undeclared)
  duplicate <- at("duplicate members.json")
  twice <- list(path = replaced$path, file = file.path(fixture, "update", "members", "model.bin"))
  writeLines(as.character(jsonlite::toJSON(list(twice, twice), auto_unbox = TRUE)), duplicate)
  stopifnot(!is.null(refusal(nirs4all_n4a_write(upd_manifest, undeclared, at("host f.n4a"),
                                                base = a_path, base_sha256 = sha_a, core_cli = cli))),
            !is.null(refusal(nirs4all_n4a_write(upd_manifest, duplicate, at("host g.n4a"),
                                                base = a_path, base_sha256 = sha_a, core_cli = cli))),
            !file.exists(at("host f.n4a")), !file.exists(at("host g.n4a")))
  # Never overwritten: used destination, the base itself, wrong base hash.
  stopifnot(!is.null(refusal(nirs4all_n4a_write(upd_manifest, upd_members, d_path, base = a_path,
                                                base_sha256 = sha_a, core_cli = cli))),
            !is.null(refusal(nirs4all_n4a_write(upd_manifest, upd_members, a_path, base = a_path,
                                                base_sha256 = sha_a, core_cli = cli))),
            !is.null(refusal(nirs4all_n4a_write(upd_manifest, upd_members, at("host e.n4a"),
                                                base = a_path, base_sha256 = strrep("0", 64L),
                                                core_cli = cli))),
            !file.exists(at("host e.n4a")), identical(file_sha(a_path), sha_a),
            identical(file_sha(d_path), sha_d))
  ran <- c(ran, "update")
  cat("R_N4A_EXCHANGE_PASS", paste0(ran, "=1", collapse = " "), "\n")
})
