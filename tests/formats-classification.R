strict <- identical(Sys.getenv("NIRS4ALL_STRICT_FORMATS"), "1") ||
  identical(Sys.getenv("NIRS4ALL_REQUIRE_FORMATS"), "1")
available <- requireNamespace("nirs4allformats", quietly = TRUE) &&
  requireNamespace("ranger", quietly = TRUE)
if (!available && strict)
  stop("strict categorical formats integration requires nirs4allformats and ranger")

if (available) {
  library(nirs4all)
  path <- system.file("extdata", "formats_classification.csv",
                      package = "nirs4all", mustWork = TRUE)
  raw <- nirs4allformats::nirs4allformats_open_dataset(path)
  stopifnot(!("species" %in% names(raw$targets)),
            identical(raw$metadata[[1L]]$species, "setosa"))
  dataset <- nirs4all_from_formats(raw, target = "species")
  from_path <- nirs4all_from_formats(path, target = "species")
  stopifnot(identical(dataset$X, from_path$X),
            identical(dataset$y, from_path$y),
            identical(names(dataset$y), dataset$sample_ids),
            identical(unname(dataset$y), rep(c("setosa", "versicolor", "virginica"), 4L)))
  column_target <- raw
  column_target$targets <- data.frame(species = unname(dataset$y))
  stopifnot(identical(nirs4all_from_formats(column_target, "species")$y,
                      dataset$y))
  pipeline <- nirs4all_pipeline(list(nirs4all_snv()),
    nirs4all_ranger_classifier(num.trees = 20L, seed = 7L,
                               num.threads = 1L))
  fitted <- nirs4all_fit(pipeline, dataset)
  labels <- nirs4all_predict(fitted, nirs4all_from_formats(path))
  stopifnot(is.factor(labels), identical(levels(labels), sort(unique(dataset$y))),
            identical(dim(nirs4all_predict_proba(fitted, dataset$X)), c(12L, 3L)))

  missing <- raw
  missing$metadata[[2L]]$species <- NULL
  stopifnot(inherits(try(nirs4all_from_formats(missing, "species"),
                         silent = TRUE), "try-error"))
  blank <- raw
  blank$metadata[[2L]]$species <- " "
  stopifnot(inherits(try(nirs4all_from_formats(blank, "species"),
                         silent = TRUE), "try-error"))
  mixed <- raw
  mixed$metadata[[2L]]$species <- 2L
  stopifnot(inherits(try(nirs4all_from_formats(mixed, "species"),
                         silent = TRUE), "try-error"))

  cli <- Sys.getenv("NIRS4ALL_DAGML_CLI")
  if (!nzchar(cli)) cli <- Sys.which("dag-ml-cli")
  dag_available <- nzchar(cli) && file.exists(cli) &&
    requireNamespace("dagml", quietly = TRUE)
  if (!dag_available && identical(Sys.getenv("NIRS4ALL_REQUIRE_DAG_PARITY"), "1"))
    stop("strict categorical formats integration requires dagml and dag-ml-cli")
  if (dag_available) {
    outcome <- nirs4all_dag_cv_refit_predict(
      pipeline, dataset, folds = 4L, split_steps = TRUE, cli = cli)
    stopifnot(identical(as.integer(outcome$fit_cv_result_count), 8L),
              identical(as.integer(outcome$refit_result_count), 2L),
              identical(nirs4all_dag_predict(outcome, nirs4all_from_formats(path)),
                        labels))
  }
}
