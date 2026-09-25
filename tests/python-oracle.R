oracle_path <- Sys.getenv("NIRS4ALL_R_PYTHON_ORACLE")
if (!nzchar(oracle_path))
  oracle_path <- system.file("extdata", "python_oracle_n4m_examples.json",
                             package = "nirs4all", mustWork = TRUE)
if (!requireNamespace("jsonlite", quietly = TRUE))
  stop("jsonlite is required for the mandatory Python oracle test")
library(nirs4all)
oracle <- jsonlite::fromJSON(oracle_path, simplifyVector = FALSE)
tol <- oracle$metadata$tolerances
if (is.null(tol)) tol <- oracle$provenance$tolerances
for (key in c("predictions_abs", "rmse_abs", "targets_abs")) {
  value <- tol[[key]]
  if (!is.numeric(value) || length(value) != 1L ||
      !is.finite(value) || value <= 0)
    stop(paste("Python oracle tolerance is missing or invalid:", key))
}
within_tolerance <- function(actual, expected, tolerance) {
  if (length(actual) != length(expected) || !length(actual) ||
      anyNA(actual) || anyNA(expected) ||
      any(!is.finite(actual)) || any(!is.finite(expected))) return(FALSE)
  max(abs(actual - expected)) <= tolerance
}
stopifnot(!within_tolerance(100, 0, tol$predictions_abs))

X <- matrix(as.numeric(unlist(oracle$dataset$X)),
            as.integer(oracle$dataset$rows),
            as.integer(oracle$dataset$cols), byrow = TRUE)
y <- as.numeric(unlist(oracle$dataset$y))
stopifnot(nrow(X) == length(y), length(oracle$cases) == 4L)

for (case in oracle$cases) {
  name <- case$name
  if (!(name %in% c("portable_snv_pls", "portable_savgol_pls",
                   "portable_kennard_stone_snv_pls", "portable_methods_pipeline")))
    stop(paste("unknown Python oracle case:", name))
  if (name %in% c("portable_kennard_stone_snv_pls", "portable_methods_pipeline")) {
    split <- n4m::kennard_stone_split(X, test_size = 0.3, zero_based = TRUE)
    train <- split$train + 1L
    validation <- split$test + 1L
    stopifnot(identical(as.integer(split$train),
                        as.integer(unlist(case$split$trainIndices))),
              identical(as.integer(split$test),
                        as.integer(unlist(case$split$testIndices))))
  } else {
    stopifnot(identical(case$split$kind, "all"))
    train <- seq_len(nrow(X))
    validation <- train
  }
  steps <- switch(name,
    portable_snv_pls = list(nirs4all_snv()),
    portable_savgol_pls = list(nirs4all_savgol(11L)),
    portable_kennard_stone_snv_pls = list(nirs4all_snv()),
    portable_methods_pipeline = list(nirs4all_snv(), nirs4all_savgol(11L)))
  target <- y[validation]
  stopifnot(within_tolerance(target, as.numeric(unlist(case$targets)),
                             tol$targets_abs))
  actual_variants <- lapply(case$variants, function(variant) {
    n_components <- as.integer(variant$n_components)
    pipeline <- nirs4all_pipeline(steps, nirs4all_pls(n_components))
    predictions <- predict(nirs4all_fit(pipeline, X[train, , drop = FALSE], y[train]),
                           X[validation, , drop = FALSE])
    rmse <- sqrt(mean((predictions - target)^2))
    stopifnot(within_tolerance(predictions,
      as.numeric(unlist(variant$predictions)), tol$predictions_abs),
      abs(rmse - as.numeric(variant$rmse)) <= tol$rmse_abs)
    list(n_components = n_components, predictions = predictions, rmse = rmse)
  })
  best <- actual_variants[[which.min(vapply(actual_variants, `[[`, numeric(1), "rmse"))]]
  stopifnot(identical(best$n_components, as.integer(case$selected$n_components)),
            within_tolerance(best$predictions,
              as.numeric(unlist(case$selected$predictions)), tol$predictions_abs),
            abs(best$rmse - as.numeric(case$selected$rmse)) <= tol$rmse_abs)
  message(name, ": ", length(actual_variants), " variant(s), selected ",
          best$n_components, ", max prediction delta ",
          format(max(abs(best$predictions -
            as.numeric(unlist(case$selected$predictions)))), digits = 8L))
}
