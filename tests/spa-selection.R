library(nirs4all)

X <- outer(seq_len(37L), seq_len(12L), function(i, j)
  sin(i * j / 11) + cos(i / 3 + j / 7) + i * j / 170)
y <- 0.9 + 0.6 * X[, 3L] - 0.4 * X[, 9L]
train <- seq_len(28L)
validation <- 29:37
step <- nirs4all_spa(top_k = 5L, n_components = 2L)
pipeline <- nirs4all_pipeline(list(step), nirs4all_pls(2L))
fitted <- nirs4all_fit(pipeline, X[train, , drop = FALSE], y[train])
selected <- fitted$step_states[[1L]]
native <- n4m::spa_select(X[train, , drop = FALSE], y[train], 2L, 5L)
stopifnot(identical(selected, native$selected_indices),
          length(selected) == 5L, !anyDuplicated(selected),
          identical(dim(nirs4all:::nirs4all_transform(
            X[validation, , drop = FALSE], fitted$steps,
            fitted$step_states)), c(length(validation), 5L)),
          identical(unname(nirs4all:::nirs4all_transform(
            X[validation, , drop = FALSE], fitted$steps,
            fitted$step_states)),
            unname(X[validation, sort(selected), drop = FALSE])))

# Refitting on training data must not use validation targets or feature rows.
with_other_validation <- X
with_other_validation[validation, ] <- with_other_validation[validation, ] + 100
refitted <- nirs4all_fit(pipeline, with_other_validation[train, , drop = FALSE],
                        y[train])
stopifnot(identical(refitted$step_states, fitted$step_states))

path <- tempfile(fileext = ".rds")
nirs4all_save(fitted, path)
restored <- nirs4all_load(path)
stopifnot(identical(restored$step_states, fitted$step_states),
          max(abs(predict(restored, X[validation, , drop = FALSE]) -
                  predict(fitted, X[validation, , drop = FALSE]))) < 1e-12)
unlink(path)

# A supervised selector in a parallel branch receives the same training y.
branched <- nirs4all_pipeline(list(nirs4all_concat(list(
  selected = list(step), original = list(nirs4all_snv())))),
  nirs4all_pls(2L))
branch_fit <- nirs4all_fit(branched, X[train, , drop = FALSE], y[train])
stopifnot(identical(branch_fit$step_states[[1L]]$selected[[1L]], selected),
          is.numeric(predict(branch_fit, X[validation, , drop = FALSE])))

# The same supervised step must survive both portable recipe formats.
for (format in c("json", "yaml")) {
  recipe <- nirs4all_export_pipeline(pipeline, format)
  imported <- nirs4all_pipeline_from_portable(recipe)
  stopifnot(identical(imported$steps[[1L]], step),
            identical(nirs4all_load_pipeline(recipe)$pipeline[[1L]]$class,
                      "n4m.SPA"))
  roundtrip <- nirs4all_fit(imported, X[train, , drop = FALSE], y[train])
  stopifnot(identical(roundtrip$step_states[[1L]], selected),
            max(abs(predict(roundtrip, X[validation, , drop = FALSE]) -
                    predict(fitted, X[validation, , drop = FALSE]))) < 1e-12)
}

recipe <- list(pipeline = list(
  list(class = "n4m.KennardStone", params = list(test_size = 0.25)),
  list(class = "n4m.SPA", params = list(top_k = 5L, n_components = 2L)),
  list(model = list(class = "n4m.PLS", params = list(n_components = 2L)))))
evaluated <- nirs4all_run_portable_pipeline(recipe, list(X = X, y = y))
split <- n4m::kennard_stone_split(X, test_size = 0.25, zero_based = TRUE)
held_fit <- nirs4all_fit(pipeline, X[split$train + 1L, , drop = FALSE],
                        y[split$train + 1L])
stopifnot(max(abs(evaluated$selected$predictions -
                  predict(held_fit, X[split$test + 1L, , drop = FALSE]))) < 1e-10,
          identical(evaluated$evaluation$scope, "selection_validation"))

for (bad in list(list(), list(top_k = 0L), list(top_k = 1.5),
                 list(top_k = "5"),
                 list(top_k = 5L, n_components = 0L),
                 list(top_k = 5L, n_components = 2L, unsupported = TRUE))) {
  invalid <- list(pipeline = list(list(class = "n4m.SPA", params = bad),
    list(model = list(class = "n4m.PLS", params = list(n_components = 2L)))))
  stopifnot(inherits(try(nirs4all_pipeline_from_portable(invalid),
                         silent = TRUE), "try-error"))
}

stopifnot(inherits(try(nirs4all_spa(0L), silent = TRUE), "try-error"),
          inherits(try(nirs4all_fit(nirs4all_pipeline(list(
            nirs4all_spa(13L))), X[train, , drop = FALSE], y[train]),
            silent = TRUE), "try-error"),
          inherits(try(nirs4all:::nirs4all_transform(
            X[validation, , drop = FALSE], list(step)),
            silent = TRUE), "try-error"))

python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON")
if (nzchar(python)) {
  helper <- if (file.exists("helpers/spa_selection_peer.py"))
    "helpers/spa_selection_peer.py" else "tests/helpers/spa_selection_peer.py"
  request <- tempfile(fileext = ".json")
  response <- tempfile(fileext = ".json")
  rows <- lapply(train, function(index) unname(as.numeric(X[index, ])))
  validation_rows <- lapply(validation, function(index)
    unname(as.numeric(X[index, ])))
  writeLines(as.character(jsonlite::toJSON(list(
    X = rows, validation = validation_rows,
    y = unname(as.numeric(y[train])), top_k = 5L,
    n_components = 2L), auto_unbox = TRUE, digits = 17)), request)
  output <- suppressWarnings(system2(python,
    c(shQuote(helper), shQuote(request), shQuote(response)),
    stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L)
    stop("Python n4m SPA oracle failed: ", paste(output, collapse = "\n"))
  oracle <- jsonlite::fromJSON(response)
  stopifnot(identical(selected, as.integer(oracle$selected_indices) + 1L),
            max(abs(X[train, sort(selected), drop = FALSE] -
                      oracle$train_selected)) < 1e-12,
            max(abs(X[validation, sort(selected), drop = FALSE] -
                      oracle$validation_selected)) < 1e-12,
            max(abs(predict(fitted, X[validation, , drop = FALSE]) -
                      oracle$predictions)) < 1e-8)
  unlink(c(request, response))
}

cli <- Sys.getenv("NIRS4ALL_DAGML_CLI")
if (nzchar(cli) && file.exists(cli) &&
    requireNamespace("dagml", quietly = TRUE)) {
  for (split_steps in c(FALSE, TRUE)) {
    outcome <- nirs4all_dag_cv_refit_predict(
      pipeline, X, y, folds = 3L, cli = cli, split_steps = split_steps)
    actual <- nirs4all_dag_predict(outcome, X[validation, , drop = FALSE])
    expected <- predict(nirs4all_fit(pipeline, X, y),
                        X[validation, , drop = FALSE])
    stopifnot(max(abs(actual - expected)) < 1e-10,
              length(outcome$oof_average_results) >= 1L)
    expected_oof <- numeric(nrow(X))
    for (fold in 0:2) {
      held <- seq_len(nrow(X))[(seq_len(nrow(X)) - 1L) %% 3L == fold]
      training <- setdiff(seq_len(nrow(X)), held)
      fold_fit <- nirs4all_fit(pipeline, X[training, , drop = FALSE], y[training])
      fold_native <- n4m::spa_select(X[training, , drop = FALSE],
                                    y[training], 2L, 5L)
      stopifnot(identical(fold_fit$step_states[[1L]],
                          fold_native$selected_indices))
      expected_oof[held] <- predict(fold_fit, X[held, , drop = FALSE])
    }
    ids <- sprintf("sample:%08d", seq_len(nrow(X)))
    for (average in outcome$oof_average_results) {
      block <- average$aggregated_predictions[[1L]]
      block_ids <- vapply(block$unit_ids, `[[`, "", "id")
      values <- vapply(block$values,
        function(value) as.numeric(value[[1L]]), numeric(1))
      stopifnot(length(values) == nrow(X),
                max(abs(values - expected_oof[match(block_ids, ids)])) < 1e-10)
    }
  }
}
