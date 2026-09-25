strict <- identical(Sys.getenv("NIRS4ALL_REQUIRE_DAG_PARITY"), "1")
cli <- Sys.getenv("NIRS4ALL_DAGML_CLI")
if (!nzchar(cli)) cli <- Sys.which("dag-ml-cli")
available <- nzchar(cli) && file.exists(cli) && requireNamespace("dagml", quietly = TRUE)
if (!available && strict) stop("strict native DAG parity requires dagml and dag-ml-cli")

if (available) {
  library(nirs4all)
  X <- outer(seq_len(12L), seq_len(8L),
             function(i, j) sin(i * j / 7) + i * j / 50)
  y <- 2 + X[, 2L] - 0.3 * X[, 5L]
  ids <- sprintf("sample:%08d", seq_len(nrow(X)))
  cases <- list(
    pls = list(pipeline = nirs4all_pipeline(
      list(nirs4all_snv(), nirs4all_savgol(5L)), nirs4all_pls(2L)),
      X = X, tolerance = 1e-10),
    lm = list(pipeline = nirs4all_pipeline(learner = nirs4all_lm()),
              X = X[, c(2L, 5L), drop = FALSE], tolerance = 1e-10))
  if (requireNamespace("ranger", quietly = TRUE))
    cases$ranger <- list(pipeline = nirs4all_pipeline(
      learner = nirs4all_ranger(num.trees = 20L, seed = 10L,
                               num.threads = 1L)), X = X, tolerance = 1e-10)
  if (requireNamespace("glmnet", quietly = TRUE))
    cases$glmnet <- list(pipeline = nirs4all_pipeline(
      learner = nirs4all_glmnet(lambda = 0.01)), X = X, tolerance = 1e-10)
  if (requireNamespace("torch", quietly = TRUE) && torch::torch_is_installed())
    cases$torch <- list(pipeline = nirs4all_pipeline(
      learner = nirs4all_torch_mlp(hidden = 8L, epochs = 15L,
                                  learning_rate = 0.01, seed = 10L)),
      X = X, tolerance = 1e-5)
  if (strict && !setequal(names(cases),
                          c("pls", "lm", "ranger", "glmnet", "torch")))
    stop("strict native DAG parity requires ranger, glmnet and torch CPU")

  for (name in names(cases)) {
    case <- cases[[name]]
    outcome <- nirs4all_dag_cv_refit_predict(case$pipeline, case$X, y,
                                              folds = 3L, cli = cli)
    stopifnot(identical(as.integer(outcome$fit_cv_result_count), 3L),
              identical(as.integer(outcome$refit_result_count), 1L),
              length(outcome$replay_prediction_blocks) == 1L,
              length(outcome$oof_average_results) >= 1L)
    replay <- outcome$replay_prediction_blocks[[1L]]
    replay_ids <- as.character(unlist(replay$sample_ids))
    replay_values <- vapply(replay$values, function(value) as.numeric(value[[1L]]),
                            numeric(1))
    refit <- predict(nirs4all_fit(case$pipeline, case$X, y), case$X)
    stopifnot(length(replay_values) == nrow(case$X),
              max(abs(replay_values - refit[match(replay_ids, ids)])) <=
                case$tolerance)

    expected_oof <- numeric(nrow(case$X))
    for (fold in 0:2) {
      validation <- seq_len(nrow(case$X))[(seq_len(nrow(case$X)) - 1L) %% 3L == fold]
      training <- setdiff(seq_len(nrow(case$X)), validation)
      expected_oof[validation] <- predict(
        nirs4all_fit(case$pipeline, case$X[training, , drop = FALSE], y[training]),
        case$X[validation, , drop = FALSE])
    }
    for (average in outcome$oof_average_results) {
      block <- average$aggregated_predictions[[1L]]
      block_ids <- vapply(block$unit_ids, `[[`, "", "id")
      block_values <- vapply(block$values,
                             function(value) as.numeric(value[[1L]]), numeric(1))
      stopifnot(length(block_values) == nrow(case$X),
                max(abs(block_values - expected_oof[match(block_ids, ids)])) <=
                  case$tolerance)
    }
    message("native DAG parity: ", name, " CV/OOF/refit/replay passed")
  }
  verify_tamper_rejected <- function(workdir) {
    data_path <- file.path(workdir, "data.rds")
    tampered <- readRDS(data_path)
    tampered$y[[1L]] <- tampered$y[[1L]] + 100
    saveRDS(tampered, data_path)
    variables <- c("NIRS4ALL_DAG_DATA_RDS", "NIRS4ALL_DAG_DSL",
                   "NIRS4ALL_DAG_ENVELOPE", "NIRS4ALL_DAG_ARTIFACT_DIR")
    previous <- Sys.getenv(variables, unset = NA_character_)
    on.exit(for (index in seq_along(variables)) {
      if (is.na(previous[[index]])) Sys.unsetenv(variables[[index]])
      else do.call(Sys.setenv, stats::setNames(list(previous[[index]]),
                                            variables[[index]]))
    }, add = TRUE)
    do.call(Sys.setenv, stats::setNames(as.list(c(
      data_path, file.path(workdir, "dsl.json"),
      file.path(workdir, "envelope.json"), file.path(workdir, "artifacts"))),
      variables))
    launcher <- file.path(workdir, if (.Platform$OS.type == "windows")
      "run-r-adapter.cmd" else "run-r-adapter")
    rejected <- suppressWarnings(system2(launcher, "--verify",
                                         stdout = TRUE, stderr = TRUE))
    stopifnot(!is.null(attr(rejected, "status")),
              attr(rejected, "status") != 0L,
              grepl("does not attest R data", paste(rejected, collapse = "\n")))
  }
  verify_tamper_rejected(outcome$workdir)
}
