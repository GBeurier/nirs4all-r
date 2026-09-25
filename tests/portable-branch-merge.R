library(nirs4all)

X <- outer(seq_len(12L), seq_len(8L),
           function(i, j) sin(i * j / 7) + i * j / 50)
y <- 2 + X[, 2L] - 0.3 * X[, 5L]
recipe <- list(pipeline = list(
  list(branch = list(
    derivative = list(list(class = "n4m.SNV"),
      list(class = "n4m.SavitzkyGolay",
           params = list(window_length = 5L, polyorder = 2L))),
    scatter = list(list(class = "n4m.MSC"),
      list(class = "n4m.Detrend", params = list(polyorder = 1L))))),
  list(merge = "features"),
  list(model = list(class = "n4m.PLS",
                    params = list(n_components = 2L)))))
manual <- nirs4all_pipeline(list(nirs4all_concat(list(
  derivative = list(nirs4all_snv(), nirs4all_savgol(5L)),
  scatter = list(nirs4all_msc(), nirs4all_detrend(1L))))),
  nirs4all_pls(2L))
for (source in list(recipe,
                    as.character(jsonlite::toJSON(recipe, auto_unbox = TRUE)),
                    yaml::as.yaml(recipe))) {
  imported <- nirs4all_pipeline_from_portable(source)
  stopifnot(isTRUE(all.equal(imported, manual)))
  expected <- predict(nirs4all_fit(manual, X[1:8, , drop = FALSE], y[1:8]),
                      X[9:12, , drop = FALSE])
  actual <- predict(nirs4all_fit(imported, X[1:8, , drop = FALSE], y[1:8]),
                    X[9:12, , drop = FALSE])
  stopifnot(max(abs(actual - expected)) < 1e-12)
}
anonymous <- recipe
anonymous$pipeline[[1L]]$branch <- unname(recipe$pipeline[[1L]]$branch)
anonymous_pipeline <- nirs4all_pipeline_from_portable(anonymous)
stopifnot(identical(names(anonymous_pipeline$steps[[1L]]$branches),
                    c("branch_0", "branch_1")),
  max(abs(predict(nirs4all_fit(anonymous_pipeline, X, y), X) -
          predict(nirs4all_fit(manual, X, y), X))) < 1e-12)
explicit <- recipe
explicit$pipeline[[1L]]$branch <- lapply(names(recipe$pipeline[[1L]]$branch),
  function(name) list(name = name,
    steps = recipe$pipeline[[1L]]$branch[[name]]))
stopifnot(isTRUE(all.equal(nirs4all_pipeline_from_portable(explicit), manual)))

evaluated <- nirs4all_run_portable_pipeline(recipe, list(X = X, y = y))
stopifnot(max(abs(evaluated$selected$predictions -
  predict(nirs4all_fit(manual, X, y), X))) < 1e-12)
expanded <- nirs4all_expand_portable_pipelines(recipe)
stopifnot(length(expanded) == 1L,
  max(abs(predict(nirs4all_fit(expanded[[1L]], X, y), X) -
          predict(nirs4all_fit(manual, X, y), X))) < 1e-12)
held_out <- recipe
held_out$pipeline <- c(list(list(class = "n4m.KennardStone",
  params = list(test_size = 0.25))), held_out$pipeline)
selection <- nirs4all_run_portable_pipeline(held_out, list(X = X, y = y))
split <- n4m::kennard_stone_split(X, test_size = 0.25, zero_based = TRUE)
expected_holdout <- predict(nirs4all_fit(manual,
  X[split$train + 1L, , drop = FALSE], y[split$train + 1L]),
  X[split$test + 1L, , drop = FALSE])
stopifnot(identical(selection$split$kind, "KennardStone"),
  max(abs(selection$selected$predictions - expected_holdout)) < 1e-10)

exportable <- nirs4all_pipeline(list(nirs4all_concat(list(
  snv = list(nirs4all_snv()),
  sg = list(nirs4all_savgol(5L))))), nirs4all_pls(2L))
for (format in c("json", "yaml")) {
  serialized <- nirs4all_export_pipeline(exportable, format = format)
  restored <- nirs4all_pipeline_from_portable(serialized)
  stopifnot(isTRUE(all.equal(restored, exportable)),
    max(abs(predict(nirs4all_fit(restored, X, y), X) -
            predict(nirs4all_fit(exportable, X, y), X))) < 1e-12)
}
for (format in c("json", "yaml")) {
  restored <- nirs4all_pipeline_from_portable(
    nirs4all_export_pipeline(manual, format = format))
  stopifnot(isTRUE(all.equal(restored, manual)),
    max(abs(predict(nirs4all_fit(restored, X, y), X) -
            predict(nirs4all_fit(manual, X, y), X))) < 1e-12)
}

python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON")
python_repo <- Sys.getenv("NIRS4ALL_PYTHON_NIRS4ALL_REPO")
if (nzchar(python) && nzchar(python_repo)) {
  helper <- if (file.exists("helpers/python_topology_peer.py"))
    "helpers/python_topology_peer.py" else
    "tests/helpers/python_topology_peer.py"
  recipe_path <- tempfile(fileext = ".json")
  response_path <- tempfile(fileext = ".json")
  writeLines(nirs4all_export_pipeline(exportable), recipe_path)
  output <- suppressWarnings(system2(python,
    c(shQuote(helper), shQuote(recipe_path), shQuote(response_path),
      shQuote(python_repo)), stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L)
    stop("Python nirs4all topology oracle failed: ",
         paste(output, collapse = "\n"))
  topology <- jsonlite::fromJSON(response_path)
  stopifnot(isTRUE(topology$feature_merge),
            !isTRUE(topology$stacking),
            !isTRUE(topology$branches_without_merge),
            identical(as.integer(topology$models), 1L))
  unlink(c(recipe_path, response_path))
}

invalid <- list(
  list(pipeline = recipe$pipeline[-2L]),
  list(pipeline = c(recipe$pipeline[1L], list(list(merge = "predictions")),
                    recipe$pipeline[3L])),
  list(pipeline = c(list(list(merge = "features")), recipe$pipeline[3L])),
  list(pipeline = list(list(branch = list(
    a = list(list(model = list(class = "n4m.PLS"))),
    b = list(list(class = "n4m.SNV")))),
    list(merge = "features"), recipe$pipeline[[3L]])))
for (source in invalid)
  stopifnot(inherits(try(nirs4all_pipeline_from_portable(source),
                       silent = TRUE), "try-error"))

cli <- Sys.getenv("NIRS4ALL_DAGML_CLI")
if (!nzchar(cli)) cli <- Sys.which("dag-ml-cli")
if (nzchar(cli) && file.exists(cli) && requireNamespace("dagml", quietly = TRUE)) {
  outcome <- nirs4all_dag_cv_refit_predict(
    nirs4all_pipeline_from_portable(recipe), X, y, folds = 3L,
    split_steps = TRUE, cli = cli)
  expected <- predict(nirs4all_fit(manual, X, y), X)
  block <- outcome$replay_prediction_blocks[[1L]]
  ids <- as.character(unlist(block$sample_ids))
  values <- vapply(block$values,
    function(value) as.numeric(value[[1L]]), numeric(1))
  stopifnot(max(abs(values - expected[match(ids,
    sprintf("sample:%08d", seq_len(nrow(X))))])) < 1e-10)
}
message("portable Python feature branch/merge recipe parity passed")
