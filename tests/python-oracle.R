oracle_path <- Sys.getenv("NIRS4ALL_R_PYTHON_ORACLE")
if (!nzchar(oracle_path))
  oracle_path <- system.file("extdata", "python_oracle_snv_savgol.json",
                             package = "nirs4all", mustWork = TRUE)
if (!requireNamespace("jsonlite", quietly = TRUE))
  stop("jsonlite is required for the mandatory Python oracle test")
library(nirs4all)
oracle <- jsonlite::fromJSON(oracle_path, simplifyVector = FALSE)
X <- matrix(as.numeric(unlist(oracle$dataset$X)),
            as.integer(oracle$dataset$rows),
            as.integer(oracle$dataset$cols), byrow = TRUE)
y <- as.numeric(unlist(oracle$dataset$y))
tolerance <- oracle$metadata$tolerances$predictions_abs
if (is.null(tolerance)) tolerance <- oracle$provenance$tolerances$predictions_abs
if (!is.numeric(tolerance) || length(tolerance) != 1L ||
    !is.finite(tolerance) || tolerance <= 0)
  stop("Python oracle prediction tolerance is missing or invalid")
within_tolerance <- function(actual, expected) {
  if (length(actual) != length(expected) || !length(actual) ||
      anyNA(actual) || anyNA(expected) ||
      any(!is.finite(actual)) || any(!is.finite(expected))) return(FALSE)
  max(abs(actual - expected)) <= tolerance
}
stopifnot(!within_tolerance(c(100), c(0)))
for (index in 1:2) {
  step <- if (index == 1L) nirs4all_snv() else nirs4all_savgol(11L)
  pipeline <- nirs4all_pipeline(list(step), nirs4all_pls(4L))
  actual <- predict(nirs4all_fit(pipeline, X, y), X)
  expected <- if (!is.null(oracle$cases[[index]]$selected)) {
    as.numeric(unlist(oracle$cases[[index]]$selected$predictions))
  } else {
    as.numeric(unlist(oracle$cases[[index]]$predictions))
  }
  delta <- max(abs(actual - expected))
  message(oracle$cases[[index]]$name, " max absolute prediction delta = ",
          format(delta, digits = 8L))
  stopifnot(within_tolerance(actual, expected))
}
