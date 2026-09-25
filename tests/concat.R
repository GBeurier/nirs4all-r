library(nirs4all)

X <- outer(seq_len(12L), seq_len(8L),
           function(i, j) sin(i * j / 7) + i * j / 50)
y <- 2 + X[, 2L] - 0.3 * X[, 5L]
train <- seq_len(8L)
validation <- 9:12
step <- nirs4all_concat(list(
  derivative = list(nirs4all_snv(), nirs4all_savgol(5L)),
  scatter = list(nirs4all_msc(), nirs4all_detrend(1L))))
pipeline <- nirs4all_pipeline(list(step), nirs4all_pls(2L))
fitted <- nirs4all_fit(pipeline, X[train, , drop = FALSE], y[train])

manual_branch <- function(rows) {
  derivative <- n4m::savgol_transform(n4m::snv_transform(
    X[rows, , drop = FALSE]), 5L, 2L, 0L, 1, "interp", 0)
  reference <- n4m::msc_fit(X[train, , drop = FALSE])
  scatter <- n4m::detrend_transform(n4m::msc_transform(
    X[rows, , drop = FALSE], reference), 1L)
  colnames(derivative) <- paste0("derivative::feature:",
                                sprintf("%08d", seq_len(ncol(derivative))))
  colnames(scatter) <- paste0("scatter::feature:",
                             sprintf("%08d", seq_len(ncol(scatter))))
  cbind(derivative, scatter)
}
transformed_train <- nirs4all:::nirs4all_transform(
  X[train, , drop = FALSE], fitted$steps, fitted$step_states)
transformed_validation <- nirs4all:::nirs4all_transform(
  X[validation, , drop = FALSE], fitted$steps, fitted$step_states)
stopifnot(identical(dim(transformed_train), c(8L, 16L)),
          identical(colnames(transformed_train),
                    colnames(transformed_validation)),
          max(abs(transformed_train - manual_branch(train))) < 1e-10,
          max(abs(transformed_validation - manual_branch(validation))) < 1e-10)

python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON")
if (nzchar(python)) {
  helper <- if (file.exists("helpers/native_preprocessing_peer.py"))
    "helpers/native_preprocessing_peer.py" else
    "tests/helpers/native_preprocessing_peer.py"
  rows <- function(values) lapply(seq_len(nrow(values)), function(index)
    unname(as.numeric(values[index, ])))
  request <- tempfile(fileext = ".json")
  response <- tempfile(fileext = ".json")
  branches <- list(
    derivative = list(list(class = "n4m.SNV"),
                      list(class = "n4m.SavitzkyGolay",
                           params = list(window_length = 5L, mode = "interp"))),
    scatter = list(list(class = "n4m.MSC"),
                   list(class = "n4m.Detrend", params = list(polyorder = 1L))))
  writeLines(as.character(jsonlite::toJSON(list(
    train = rows(X[train, , drop = FALSE]),
    validation = rows(X[validation, , drop = FALSE]),
    branches = branches), auto_unbox = TRUE, digits = 17)), request)
  output <- suppressWarnings(system2(python,
    c(shQuote(helper), shQuote(request), shQuote(response)),
    stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L)
    stop("Python n4m concat oracle failed: ", paste(output, collapse = "\n"))
  oracle <- jsonlite::fromJSON(response)
  stopifnot(max(abs(transformed_train - oracle$train)) < 1e-10,
            max(abs(transformed_validation - oracle$validation)) < 1e-10)
  unlink(c(request, response))
}

path <- tempfile(fileext = ".rds")
nirs4all_save(fitted, path)
restored <- nirs4all_load(path)
stopifnot(max(abs(predict(fitted, X[validation, , drop = FALSE]) -
                  predict(restored, X[validation, , drop = FALSE]))) < 1e-12)
stopifnot(inherits(try(nirs4all_concat(list(
  same = list(nirs4all_snv()), same = list(nirs4all_msc()))),
  silent = TRUE), "try-error"))
message("local parallel n4m branch composition and fitted-state replay passed")
