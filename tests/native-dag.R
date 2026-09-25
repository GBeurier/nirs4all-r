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
    n4m_ridge = list(pipeline = nirs4all_pipeline(
      list(nirs4all_snv()),
      nirs4all_n4m_method("ridge", params = list(ridge_lambda = 0.5))),
      X = X, tolerance = 1e-10),
    n4m_cppls = list(pipeline = nirs4all_pipeline(
      learner = nirs4all_n4m_method("cppls", n_components = 2L)),
      X = X, tolerance = 1e-10),
    n4m_preprocessing = list(pipeline = nirs4all_pipeline(
      list(nirs4all_local_snv(5L), nirs4all_robust_snv(),
           nirs4all_detrend(1L)), nirs4all_pls(2L)),
      X = X, tolerance = 1e-10),
    n4m_area = list(pipeline = nirs4all_pipeline(
      list(nirs4all_area_normalization("trapz")), nirs4all_pls(2L)),
      X = X, tolerance = 1e-10),
    n4m_msc = list(pipeline = nirs4all_pipeline(
      list(nirs4all_msc()), nirs4all_pls(2L)),
      X = X, tolerance = 1e-10),
    n4m_emsc = list(pipeline = nirs4all_pipeline(
      list(nirs4all_emsc(2L)), nirs4all_pls(2L)),
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
  if (requireNamespace("parsnip", quietly = TRUE))
    cases$parsnip <- list(pipeline = nirs4all_pipeline(
      learner = nirs4all_parsnip(parsnip::set_engine(parsnip::linear_reg(), "lm"))),
      X = X[, c(2L, 5L), drop = FALSE], tolerance = 1e-10)
  if (requireNamespace("mlr3", quietly = TRUE) &&
      requireNamespace("rpart", quietly = TRUE))
    cases$mlr3 <- list(pipeline = nirs4all_pipeline(
      learner = nirs4all_mlr3(mlr3::lrn("regr.rpart", minsplit = 3L,
                                              cp = 0))),
      X = X[, c(2L, 5L), drop = FALSE], tolerance = 1e-10)
  if (requireNamespace("torch", quietly = TRUE) && torch::torch_is_installed()) {
    cases$torch <- list(pipeline = nirs4all_pipeline(
      learner = nirs4all_torch_mlp(hidden = 8L, epochs = 15L,
                                  learning_rate = 0.01, seed = 10L)),
      X = X, tolerance = 1e-5)
    builder <- local({
      width <- 7L
      function(n_features) torch::nn_sequential(
        torch::nn_linear(n_features, width), torch::nn_tanh(),
        torch::nn_linear(width, 1L))
    })
    cases$torch_module <- list(pipeline = nirs4all_pipeline(
      learner = nirs4all_torch_module(builder, name = "custom",
        epochs = 15L, learning_rate = 0.01, seed = 10L)),
      X = X, tolerance = 1e-5)
  }
  if (strict && !setequal(names(cases),
                          c("pls", "n4m_ridge", "n4m_cppls",
                            "n4m_preprocessing", "n4m_area", "n4m_msc", "n4m_emsc", "lm",
                            "ranger", "glmnet", "parsnip", "mlr3", "torch",
                            "torch_module")))
    stop("strict native DAG parity requires ranger, glmnet, parsnip, mlr3 and torch CPU")

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
    new_X <- case$X[1:4, , drop = FALSE] + 0.01
    external <- nirs4all_dag_predict(outcome, new_X)
    manual_external <- predict(nirs4all_fit(case$pipeline, case$X, y), new_X)
    stopifnot(max(abs(external - manual_external)) <= case$tolerance)

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
  # Transform nodes must exchange their actual matrices between process workers.
  # MSC/EMSC are fitted on each fold's training rows, never on validation rows.
  for (steps in list(
    list(nirs4all_snv(), nirs4all_msc()),
    list(nirs4all_savgol(5L), nirs4all_emsc(2L)))) {
    graph_pipeline <- nirs4all_pipeline(steps, nirs4all_pls(2L))
    graph <- nirs4all_dag_cv_refit_predict(
      graph_pipeline, X, y, folds = 3L, cli = cli,
      split_steps = TRUE, process_workers = 2L)
    stopifnot(identical(as.integer(graph$fit_cv_result_count), 9L),
              identical(as.integer(graph$refit_result_count), 3L),
              length(graph$bundle$refit_artifacts) == 3L)
    graph_replay <- graph$replay_prediction_blocks[[1L]]
    replay_ids <- as.character(unlist(graph_replay$sample_ids))
    replay_values <- vapply(graph_replay$values,
                            function(value) as.numeric(value[[1L]]), numeric(1))
    fitted <- nirs4all_fit(graph_pipeline, X, y)
    stopifnot(max(abs(replay_values -
      predict(fitted, X)[match(replay_ids, ids)])) < 1e-10)
    external_X <- X[1:4, , drop = FALSE] + 0.01
    stopifnot(max(abs(nirs4all_dag_predict(graph, external_X) -
      predict(fitted, external_X))) < 1e-10)
    expected_oof <- numeric(nrow(X))
    for (fold in 0:2) {
      validation <- seq_len(nrow(X))[(seq_len(nrow(X)) - 1L) %% 3L == fold]
      training <- setdiff(seq_len(nrow(X)), validation)
      expected_oof[validation] <- predict(nirs4all_fit(graph_pipeline,
        X[training, , drop = FALSE], y[training]),
        X[validation, , drop = FALSE])
    }
    for (average in graph$oof_average_results) {
      block <- average$aggregated_predictions[[1L]]
      block_ids <- vapply(block$unit_ids, `[[`, "", "id")
      values <- vapply(block$values,
        function(value) as.numeric(value[[1L]]), numeric(1))
      stopifnot(max(abs(values - expected_oof[match(block_ids, ids)])) < 1e-10)
    }
    transform_artifact <- graph$bundle$refit_artifacts[[2L]]$artifact$uri
    state <- readRDS(transform_artifact)
    state$n_features <- state$n_features + 1L
    saveRDS(state, transform_artifact)
    rejected <- tryCatch(nirs4all_dag_predict(graph, external_X),
                         error = function(error) conditionMessage(error))
    stopifnot(is.character(rejected),
              grepl("artifact identity or content mismatch", rejected))
  }
  message("native multi-node n4m transform graph CV/OOF/refit/replay passed")
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

  if (requireNamespace("nirs4allformats", quietly = TRUE)) {
    path <- system.file("extdata", "formats_integration.csv",
                        package = "nirs4all", mustWork = TRUE)
    dataset <- nirs4all_from_formats(path, target = "protein")
    for (step in list(nirs4all_snv(), nirs4all_msc(), nirs4all_emsc(2L))) {
      pipeline <- nirs4all_pipeline(list(step), nirs4all_pls(2L))
      outcome <- nirs4all_dag_cv_refit_predict(pipeline, dataset,
                                                folds = 3L, cli = cli)
      block <- outcome$replay_prediction_blocks[[1L]]
      ids <- as.character(unlist(block$sample_ids, use.names = FALSE))
      values <- vapply(block$values, function(value) as.numeric(value[[1L]]),
                       numeric(1))
      expected <- predict(nirs4all_fit(pipeline, dataset), dataset)
      stopifnot(max(abs(values - expected[match(ids, dataset$sample_ids)])) < 1e-10)
      stopifnot(max(abs(nirs4all_dag_predict(outcome, dataset) - expected)) < 1e-10)
    }
    message("formats file → native DAG CV/refit/replay passed")
  }

  oracle <- jsonlite::fromJSON(system.file(
    "extdata", "python_oracle_n4m_examples.json", package = "nirs4all",
    mustWork = TRUE), simplifyVector = FALSE)
  oracle_X <- matrix(as.numeric(unlist(oracle$dataset$X)),
                     as.integer(oracle$dataset$rows),
                     as.integer(oracle$dataset$cols), byrow = TRUE)
  oracle_y <- as.numeric(unlist(oracle$dataset$y))
  candidates <- stats::setNames(lapply(c(2L, 4L, 6L, 8L, 10L), function(components)
    nirs4all_pipeline(list(nirs4all_snv(), nirs4all_savgol(11L)),
                     nirs4all_pls(components))),
    paste0("pls", c(2L, 4L, 6L, 8L, 10L)))
  selected <- nirs4all_dag_cv_refit_predict(candidates, oracle_X, oracle_y,
                                             folds = 5L, cli = cli)
  catalog <- selected$bundle$metadata$variant_catalog
  by_id <- stats::setNames(vapply(catalog, function(variant)
    variant$choices$nirs4all_r_pipeline$label, character(1)),
    vapply(catalog, `[[`, character(1), "variant_id"))
  stopifnot(length(catalog) == length(candidates),
            setequal(unname(by_id), names(candidates)),
            identical(as.integer(selected$fit_cv_result_count), 5L),
            identical(as.integer(selected$refit_result_count), 1L))
  manual_rmse <- numeric(length(candidates))
  names(manual_rmse) <- names(candidates)
  for (label in names(candidates)) {
    predictions <- numeric(nrow(oracle_X))
    variant_id <- names(by_id)[by_id == label]
    reports <- Filter(function(report)
      identical(report$variant_id, variant_id) &&
        identical(report$partition, "validation") &&
        is.character(report$fold_id) &&
        grepl("^fold:[0-9]+$", report$fold_id),
      selected$bundle$scores$reports)
    stopifnot(length(reports) == 5L)
    for (fold in 0:4) {
      validation <- which((seq_len(nrow(oracle_X)) - 1L) %% 5L == fold)
      training <- setdiff(seq_len(nrow(oracle_X)), validation)
      predictions[validation] <- predict(nirs4all_fit(
        candidates[[label]], oracle_X[training, , drop = FALSE],
        oracle_y[training]), oracle_X[validation, , drop = FALSE])
      report <- Filter(function(value) identical(value$fold_id,
                                                  paste0("fold:", fold)), reports)
      stopifnot(length(report) == 1L,
                abs(as.numeric(report[[1L]]$metrics$rmse) - sqrt(mean(
                  (predictions[validation] - oracle_y[validation])^2))) < 1e-10)
    }
    manual_rmse[[label]] <- sqrt(mean((predictions - oracle_y)^2))
  }
  averages <- Filter(function(report)
    identical(report$fold_id, "avg") &&
      identical(report$partition, "validation"),
    selected$bundle$scores$reports)
  stopifnot(length(averages) == length(candidates),
            max(abs(sort(vapply(averages, function(report)
              as.numeric(report$metrics$rmse), numeric(1))) -
              sort(manual_rmse))) < 1e-10)
  winner <- by_id[[selected$bundle$selected_variant_id]]
  stopifnot(identical(winner, names(which.min(manual_rmse))))
  replay <- selected$replay_prediction_blocks[[1L]]
  replay_ids <- as.character(unlist(replay$sample_ids, use.names = FALSE))
  replay_values <- vapply(replay$values,
                          function(value) as.numeric(value[[1L]]), numeric(1))
  expected <- predict(nirs4all_fit(candidates[[winner]], oracle_X, oracle_y),
                      oracle_X)
  row_ids <- sprintf("sample:%08d", seq_len(nrow(oracle_X)))
  stopifnot(max(abs(replay_values - expected[match(replay_ids, row_ids)])) < 1e-10)
  external_X <- oracle_X[1:5, , drop = FALSE] + 0.01
  external <- nirs4all_dag_predict(selected, external_X)
  manual_external <- predict(nirs4all_fit(candidates[[winner]], oracle_X,
                                         oracle_y), external_X)
  stopifnot(max(abs(external - manual_external)) < 1e-10)
  altered <- selected
  altered$bundle$refit_artifacts[[1L]]$artifact$content_fingerprint <-
    paste(rep("0", 64L), collapse = "")
  stopifnot(inherits(try(nirs4all_dag_predict(altered, external_X),
                         silent = TRUE), "try-error"))
  message("native DAG model selection on Python n4m example passed: ", winner)

  mixed <- list(
    pls = nirs4all_pipeline(list(nirs4all_snv()), nirs4all_pls(3L)),
    ridge = nirs4all_pipeline(list(nirs4all_snv()),
      nirs4all_n4m_method("ridge", params = list(ridge_lambda = 0.2))))
  if (requireNamespace("ranger", quietly = TRUE))
    mixed$forest <- nirs4all_pipeline(learner = nirs4all_ranger(
      num.trees = 20L, seed = 10L, num.threads = 1L))
  mixed_outcome <- nirs4all_dag_cv_refit_predict(mixed, oracle_X, oracle_y,
                                                  folds = 4L, cli = cli)
  mixed_catalog <- mixed_outcome$bundle$metadata$variant_catalog
  mixed_labels <- stats::setNames(vapply(mixed_catalog, function(variant)
    variant$choices$nirs4all_r_pipeline$label, character(1)),
    vapply(mixed_catalog, `[[`, character(1), "variant_id"))
  mixed_rmse <- vapply(names(mixed), function(label) {
    oof <- numeric(nrow(oracle_X))
    for (fold in 0:3) {
      valid <- which((seq_len(nrow(oracle_X)) - 1L) %% 4L == fold)
      train <- setdiff(seq_len(nrow(oracle_X)), valid)
      oof[valid] <- predict(nirs4all_fit(mixed[[label]],
        oracle_X[train, , drop = FALSE], oracle_y[train]),
        oracle_X[valid, , drop = FALSE])
    }
    sqrt(mean((oof - oracle_y)^2))
  }, numeric(1))
  mixed_winner <- mixed_labels[[mixed_outcome$bundle$selected_variant_id]]
  stopifnot(identical(mixed_winner, names(which.min(mixed_rmse))),
            identical(as.integer(mixed_outcome$refit_result_count), 1L))
  message("native DAG cross-family selection passed: ", mixed_winner)
}
