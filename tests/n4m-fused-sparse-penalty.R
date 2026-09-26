# Corrected n4m applies l1_lambda to predictive coefficients before fusion.
# The old golden at lambda=0.05 accidentally described a no-op penalty and
# still holds for lambda=0; do not accept a no-op kernel at nonzero lambda.
library(nirs4all)

train <- outer(seq_len(21L), seq_len(12L), function(s, b)
  sin(s * b / 9) + cos(s + b / 7) + s * b / 100)
colnames(train) <- paste0("wl", seq_len(ncol(train)))
y <- 1.3 + 0.7 * train[, 2L] - 0.4 * train[, 6L]
held <- train[c(2L, 8L, 17L), , drop = FALSE] + 0.031
golden <- list(
  `0` = c(1.341067723610322, 2.011218914142026, 0.6903830575230807),
  `0.05` = c(1.3373539467794613, 1.9377148952044154,
             0.7544322586932719))

fits <- lapply(names(golden), function(lambda) {
  pipeline <- nirs4all_pipeline(learner = nirs4all_n4m_method(
    "fused_sparse_pls", 2L,
    list(l1_lambda = as.numeric(lambda), fusion_lambda = 0.05)))
  nirs4all_fit(pipeline, train, y)
})
names(fits) <- names(golden)
for (lambda in names(golden))
  stopifnot(max(abs(predict(fits[[lambda]], held) - golden[[lambda]])) < 1e-10)
stopifnot(max(abs(golden[["0"]] - golden[["0.05"]])) > 1e-3,
  max(abs(fits[["0"]]$state$coefficients -
    fits[["0.05"]]$state$coefficients)) > 1e-5)

python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON", "")
if (nzchar(python)) {
  helper <- if (file.exists("helpers/fused_sparse_peer.py"))
    "helpers/fused_sparse_peer.py" else "tests/helpers/fused_sparse_peer.py"
  rows <- function(X) lapply(seq_len(nrow(X)), function(i)
    unname(as.numeric(X[i, ])))
  request <- tempfile(fileext = ".json")
  response <- tempfile(fileext = ".json")
  for (lambda in names(golden)) {
    writeLines(as.character(jsonlite::toJSON(list(
      train = rows(train), held = rows(held), y = unname(as.numeric(y)),
      n_components = 2L, l1_lambda = as.numeric(lambda),
      fusion_lambda = 0.05), auto_unbox = TRUE, digits = 17L)), request)
    output <- suppressWarnings(system2(python,
      c(shQuote(helper), shQuote(request), shQuote(response)),
      stdout = TRUE, stderr = TRUE))
    status <- attr(output, "status")
    if (!is.null(status) && status != 0L)
      stop("Python FusedSparsePLS n4m peer failed: ",
        paste(output, collapse = "\n"))
    peer <- jsonlite::fromJSON(response)
    stopifnot(max(abs(peer$predictions - golden[[lambda]])) < 1e-10,
      max(abs(peer$coefficients -
        as.numeric(fits[[lambda]]$state$coefficients))) < 1e-10)
  }
  unlink(c(request, response))
}
