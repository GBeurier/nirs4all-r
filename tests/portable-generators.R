library(nirs4all)

# Python expand_spec() yields the four preprocessing sequences in this order:
# [SNV, SNV], [SNV, SG], [SG, SNV], [SG, SG]. The final PLS range then
# varies the component count inside each sequence.
recipe <- list(name = "cartesian_n4m", pipeline = list(
  list(`_cartesian_` = list(
    list(`_or_` = list(list(class = "n4m.SNV"),
                       list(class = "n4m.SavitzkyGolay",
                            params = list(window_length = 5L, polyorder = 2L)))),
    list(`_or_` = list(list(class = "n4m.SNV"),
                       list(class = "n4m.SavitzkyGolay",
                            params = list(window_length = 7L, polyorder = 2L)))))),
  list(model = list(class = "n4m.PLS"), `_range_` = list(1L, 2L, 1L),
       param = "n_components")))
expected_kinds <- list(c("snv", "snv"), c("snv", "savgol"),
                       c("savgol", "snv"), c("savgol", "savgol"))
X <- outer(seq_len(18L), seq_len(8L),
           function(i, j) sin(i * j / 7) + i * j / 50)
y <- 2 + X[, 2L] - 0.3 * X[, 5L]
for (source in list(recipe,
                    as.character(jsonlite::toJSON(recipe, auto_unbox = TRUE)),
                    yaml::as.yaml(recipe))) {
  variants <- nirs4all_expand_portable_pipelines(source)
  stopifnot(length(variants) == 8L,
            identical(names(variants), sprintf("variant_%04d", 1:8)))
  for (index in seq_along(variants)) {
    pipeline <- variants[[index]]
    sequence <- (index + 1L) %/% 2L
    components <- if (index %% 2L) 1L else 2L
    stopifnot(identical(vapply(pipeline$steps, `[[`, "", "kind"),
                        expected_kinds[[sequence]]),
              identical(pipeline$learner$spec$n_components, components))
    expected_steps <- lapply(seq_along(expected_kinds[[sequence]]), function(stage) {
      if (expected_kinds[[sequence]][[stage]] == "snv") nirs4all_snv()
      else nirs4all_savgol(if (stage == 1L) 5L else 7L, polyorder = 2L)
    })
    manual <- nirs4all_pipeline(expected_steps, nirs4all_pls(components))
    actual <- predict(nirs4all_fit(pipeline, X, y), X)
    expected <- predict(nirs4all_fit(manual, X, y), X)
    stopifnot(max(abs(actual - expected)) < 1e-12)
  }
}
python_repo <- Sys.getenv("NIRS4ALL_PYTHON_NIRS4ALL_REPO")
require_python <- identical(Sys.getenv("NIRS4ALL_REQUIRE_GENERATOR_PYTHON_PARITY"),
                            "1")
if (require_python && !nzchar(python_repo))
  stop("strict generator parity requires NIRS4ALL_PYTHON_NIRS4ALL_REPO")
if (nzchar(python_repo)) {
  python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON")
  if (!nzchar(python)) python <- Sys.which("python3")
  helper <- if (file.exists("helpers/portable_generator_oracle.py"))
    "helpers/portable_generator_oracle.py" else
    "tests/helpers/portable_generator_oracle.py"
  stopifnot(file.exists(helper), file.exists(python), dir.exists(python_repo))
  request <- tempfile(fileext = ".json")
  response <- tempfile(fileext = ".json")
  writeLines(as.character(jsonlite::toJSON(recipe, auto_unbox = TRUE)), request)
  output <- suppressWarnings(system2(python,
    c(shQuote(helper), shQuote(request), shQuote(response),
      shQuote(python_repo)), stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L)
    stop("Python generator oracle failed: ", paste(output, collapse = "\n"))
  oracle <- jsonlite::fromJSON(response, simplifyVector = FALSE)
  variants <- nirs4all_expand_portable_pipelines(recipe)
  stopifnot(length(oracle) == length(variants))
  for (index in seq_along(oracle)) {
    steps <- variants[[index]]$steps
    actual_classes <- vapply(steps, function(step)
      if (identical(step$kind, "snv")) "n4m.SNV" else "n4m.SavitzkyGolay",
      character(1))
    actual_windows <- lapply(steps, function(step)
      if (identical(step$kind, "savgol")) step$window_length else NULL)
    stopifnot(identical(actual_classes,
                        as.character(unlist(oracle[[index]]$classes))),
              identical(actual_windows, oracle[[index]]$windows),
              identical(variants[[index]]$learner$spec$n_components,
                        as.integer(oracle[[index]]$n_components)))
  }
  unlink(c(request, response))
}
bad <- recipe
bad$pipeline[[1L]]$count <- 2L
stopifnot(inherits(try(nirs4all_expand_portable_pipelines(bad), silent = TRUE),
                   "try-error"))
bad <- recipe
bad$pipeline[[2L]]$`_range_`[[3L]] <- 0L
stopifnot(inherits(try(nirs4all_expand_portable_pipelines(bad), silent = TRUE),
                   "try-error"))

cli <- Sys.getenv("NIRS4ALL_DAGML_CLI")
if (!nzchar(cli)) cli <- Sys.which("dag-ml-cli")
strict <- identical(Sys.getenv("NIRS4ALL_REQUIRE_DAG_PARITY"), "1")
available <- nzchar(cli) && file.exists(cli) &&
  requireNamespace("dagml", quietly = TRUE)
if (!available && strict)
  stop("strict portable-generator parity requires dagml and dag-ml-cli")
if (available) {
  candidates <- nirs4all_expand_portable_pipelines(recipe)
  outcome <- nirs4all_dag_cv_refit_predict(candidates, X, y,
                                           folds = 3L, cli = cli)
  catalog <- outcome$bundle$metadata$variant_catalog
  labels <- stats::setNames(vapply(catalog, function(variant)
    variant$choices$nirs4all_r_pipeline$label, character(1)),
    vapply(catalog, `[[`, character(1), "variant_id"))
  stopifnot(setequal(unname(labels), names(candidates)),
            identical(as.integer(outcome$fit_cv_result_count), 3L),
            identical(as.integer(outcome$refit_result_count), 1L))
  rmse <- vapply(names(candidates), function(label) {
    oof <- numeric(nrow(X))
    for (fold in 0:2) {
      validation <- which((seq_len(nrow(X)) - 1L) %% 3L == fold)
      training <- setdiff(seq_len(nrow(X)), validation)
      oof[validation] <- predict(nirs4all_fit(candidates[[label]],
        X[training, , drop = FALSE], y[training]),
        X[validation, , drop = FALSE])
    }
    sqrt(mean((oof - y)^2))
  }, numeric(1))
  winner <- labels[[outcome$bundle$selected_variant_id]]
  stopifnot(identical(winner, names(which.min(rmse))))
  external <- X[1:4, , drop = FALSE] + 0.01
  stopifnot(max(abs(nirs4all_dag_predict(outcome, external) -
                    predict(nirs4all_fit(candidates[[winner]], X, y),
                            external))) < 1e-10)
}
bad <- recipe
bad$pipeline[[1L]]$`_cartesian_`[[1L]]$`_or_`[[1L]] <-
  list(class = "sklearn.preprocessing.StandardScaler")
stopifnot(inherits(try(nirs4all_expand_portable_pipelines(bad), silent = TRUE),
                   "try-error"))
