# Native frontend: no optimizer, fold loop or score calculation in R.

.nirs4all_tuning_seed_decimal <- function(seed) {
  if (length(seed) != 1L || !is.numeric(seed) || !is.finite(seed) ||
      seed < 0 || seed > 2^53 - 1 || seed != floor(seed))
    stop("seed must be a nonnegative integer within R's exact numeric range 0..2^53-1", call. = FALSE)
  format(seed, scientific = FALSE, trim = TRUE, digits = 17L)
}

nirs4all_generate <- function(choices, strategy = "cartesian", constraints = list(),
                              count = NULL, seed = 0, max_variants = 10000,
                              cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive")) {
  if (length(seed) != 1L || !is.numeric(seed) || !is.finite(seed) || seed < 0 || seed > 2^53 - 1 || seed != floor(seed))
    stop("seed must be a nonnegative exactly representable integer", call. = FALSE)
  if (!length(constraints)) constraints <- structure(list(), names = character())
  choices <- lapply(choices, function(value) if (is.atomic(value)) as.list(value) else value)
  record <- list(choices = choices, strategy = strategy, constraints = constraints,
                 count = count, seed = seed, max_variants = max_variants)
  if (is.null(count)) record$count <- NULL
  reply <- .nirs4all_workflow_native_call("generate", list(), record = record, cli = cli)
  if (is.null(reply$seed_decimals) || length(reply$seed_decimals) != length(reply$variants))
    stop("native generator requires exact decimal seed transport", call. = FALSE)
  for (i in seq_along(reply$variants)) reply$variants[[i]]$seed <- reply$seed_decimals[[i]]
  reply$variants
}

nirs4all_tune <- function(data, trials = 8L, seed = 91L, sampler = "random", metric = "rmse",
                          source_id = "spectra", checkpoint = NULL,
                          methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
                          archive = tempfile("nirs4all-tuning-", fileext = ".n4a"),
                          run_id = paste0("run:tuning:", basename(tempfile())),
                          cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive")) {
  if (length(trials) != 1L || !is.numeric(trials) || is.na(trials) ||
      trials < 1L || trials > 256L || trials != floor(trials))
    stop("trials must be a total budget between 1 and 256", call. = FALSE)
  if (inherits(checkpoint, "nirs4all_tuning_result")) checkpoint <- checkpoint$archive
  if (missing(cli) && inherits(data, "nirs4all_dataset")) cli <- data$core_cli
  cli <- .nirs4all_native_cli(cli)
  flags <- list(trials = as.character(trials), seed = .nirs4all_tuning_seed_decimal(seed), sampler = sampler,
                metric = metric, source_id = source_id, methods_library = methods_library,
                archive = archive, run_id = run_id)
  if (!is.null(checkpoint)) flags$checkpoint_archive <- checkpoint
  record <- if (inherits(data, "nirs4all_dataset")) data$record else data
  outcome <- .nirs4all_workflow_native_call("tuning-run", flags, record = record, cli = cli)
  structure(list(archive = normalizePath(archive, mustWork = TRUE), outcome = outcome,
                 config = list(source_id = source_id, trials = trials, seed = seed,
                               sampler = sampler, metric = metric), cli = cli), class = "nirs4all_tuning_result")
}

nirs4all_tuning_predict <- function(object, X, ...) {
  if (!inherits(object, "nirs4all_tuning_result")) stop("expected tuning result")
  options <- list(...)
  if (is.null(options$cli)) options$cli <- object$cli
  do.call(nirs4all_native_predict, c(list(object = object$archive, X = X), options))
}

nirs4all_resume_tuning <- function(object, data, trials, ...) {
  if (!inherits(object, "nirs4all_tuning_result")) stop("expected tuning result")
  options <- utils::modifyList(object$config, list(...))
  if (is.null(options$cli)) options$cli <- object$cli
  options$trials <- trials
  options$checkpoint <- object$archive
  do.call(nirs4all_tune, c(list(data = data), options))
}

nirs4all_tuning_export <- function(object, directory) {
  if (!inherits(object, "nirs4all_tuning_result")) stop("expected tuning result")
  .nirs4all_native_bundle(object$archive, directory,
    list(schema = "nirs4all.tuning.v1", config = object$config,
    training_outcome_fingerprint = object$outcome$training_outcome$outcome_fingerprint,
    archive_sha256 = object$outcome$archive_sha256), "tuning.json", object$cli)
}

nirs4all_load_tuning <- function(directory,
    cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive")) {
  cli <- .nirs4all_native_cli(cli)
  archive <- normalizePath(file.path(directory, "model.n4a"), mustWork = TRUE)
  saved <- jsonlite::fromJSON(file.path(directory, "tuning.json"), simplifyVector = FALSE, bigint_as_char = TRUE)
  native <- .nirs4all_workflow_native_call("tuning-load", list(archive = archive), cli = cli)
  .nirs4all_tuning_seed_decimal(native$config$seed)
  same_config <- identical(sort(names(saved$config)), sort(names(native$config))) &&
    all(vapply(names(native$config), function(name)
      isTRUE(all.equal(saved$config[[name]], native$config[[name]], tolerance = 0,
                       check.attributes = FALSE)), logical(1)))
  if (!identical(saved$schema, "nirs4all.tuning.v1") ||
      !same_config ||
      !identical(saved$training_outcome_fingerprint, native$training_outcome$outcome_fingerprint) ||
      !identical(saved$archive_sha256, native$archive_sha256)) stop("tuning metadata differs from native archive")
  structure(list(archive = archive, outcome = native, config = native$config, cli = cli), class = "nirs4all_tuning_result")
}
