library(nirs4all)

fixture <- Sys.getenv("NIRS4ALL_TUNING_DATASET")
if (nzchar(fixture) && nzchar(Sys.getenv("NIRS4ALL_CORE_CLI"))) {
  choices <- list(shape = list(c(2, 3), c(4, 5)), selection = list("one", "two"))
  constraints <- list(exclude = list(list(list(dimension = "shape", label = "choice:1"),
                                         list(dimension = "selection", label = "choice:1"))))
  stopifnot(length(nirs4all_generate(choices, constraints = constraints)) == 3L)
  variants <- nirs4all_generate(choices, "random", constraints = constraints, count = 2L, seed = 17L)
  stopifnot(length(variants) == 2L, identical(variants, nirs4all_generate(choices, "random", constraints = constraints, count = 2L, seed = 17L)))
  stopifnot(all(vapply(variants, function(variant)
    is.character(variant$seed) && length(variant$seed) == 1L && grepl("^[0-9]+$", variant$seed), logical(1))))
  data <- jsonlite::fromJSON(fixture, simplifyVector = FALSE)
  directory <- tempfile("nirs4all-r-tuning-"); dir.create(directory)
  for (seed in list(-1, 0.5, Inf, NA_real_, 2^53, c(1, 2), "100000")) {
    rejected <- file.path(directory, "invalid-seed.n4a")
    stopifnot(inherits(try(nirs4all_tune(data, trials = 1L, seed = seed, archive = rejected), silent = TRUE), "try-error"),
              !file.exists(rejected))
  }
  for (seed in c(1e5, 3e9)) {
    seed_path <- file.path(directory, paste0("seed-", format(seed, scientific = FALSE)))
    seeded <- nirs4all_tune(data, trials = 1L, seed = seed, archive = paste0(seed_path, ".n4a"))
    nirs4all_tuning_export(seeded, paste0(seed_path, "-export"))
    reopened_seed <- nirs4all_load_tuning(paste0(seed_path, "-export"))
    continued_seed <- nirs4all_resume_tuning(reopened_seed, data, 2L, archive = paste0(seed_path, "-resumed.n4a"))
    stopifnot(identical(as.numeric(reopened_seed$config$seed), seed),
      identical(seeded$outcome$training_outcome$methods_hpo_resume_state$terminal_trials,
                continued_seed$outcome$training_outcome$methods_hpo_resume_state$terminal_trials[1]))
  }
  first <- nirs4all_tune(data, trials = 2L, archive = file.path(directory, "first.n4a"),
                         methods_library = Sys.getenv("N4M_LIBRARY_PATH"))
  resumed <- nirs4all_resume_tuning(first, data, 4L, archive = file.path(directory, "resumed.n4a"),
                                   methods_library = Sys.getenv("N4M_LIBRARY_PATH"))
  old <- first$outcome$training_outcome$methods_hpo_resume_state$terminal_trials
  new <- resumed$outcome$training_outcome$methods_hpo_resume_state$terminal_trials
  stopifnot(identical(old, new[1:2]), length(new) == 4L)
  nirs4all_tuning_export(resumed, file.path(directory, "export"))
  loaded <- nirs4all_load_tuning(file.path(directory, "export"))
  x <- do.call(rbind, lapply(data$dataset$sources[[1]]$array$values[1:3], unlist))
  replay <- nirs4all_tuning_predict(loaded, x, sample_ids = c("cold:r:0", "cold:r:1", "cold:r:2"))
  stopifnot(identical(unlist(replay$outputs[[1]]$predictions[[1]]$sample_ids), c("cold:r:0", "cold:r:1", "cold:r:2")),
            all(vapply(replay$lineage, function(line) line$phase == "PREDICT", logical(1))))
  saved <- jsonlite::fromJSON(file.path(directory, "export", "tuning.json"), simplifyVector = FALSE)
  saved$config$seed <- saved$config$seed + 1
  jsonlite::write_json(saved, file.path(directory, "export", "tuning.json"), auto_unbox = TRUE)
  stopifnot(inherits(try(nirs4all_load_tuning(file.path(directory, "export")), silent = TRUE), "try-error"))
  unlink(directory, recursive = TRUE)
  cat("R_NATIVE_TUNING_RESUME_COLD_PREDICT_OK\n")
}
