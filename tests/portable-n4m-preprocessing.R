library(nirs4all)

X <- outer(seq_len(18L), seq_len(8L),
           function(i, j) 3 + sin(i * j / 7) + i * j / 50)
y <- 2 + X[, 2L] - 0.3 * X[, 5L]
train <- 1:12
validation <- 13:18
cases <- list(
  local_snv = list(class = "n4m.LSNV",
                   params = list(window = 5L, pad_mode = "constant",
                                 constant_value = 0.25)),
  robust_snv = list(class = "n4m.RNV",
                    params = list(with_center = FALSE, with_scale = TRUE,
                                  k = 1.7)),
  area = list(class = "n4m.AreaNormalization",
              params = list(method = "trapz")),
  detrend = list(class = "n4m.Detrend",
                 params = list(polyorder = 2L)),
  msc = list(class = "n4m.MSC"),
  emsc = list(class = "n4m.EMSC", params = list(degree = 2L)))
constructors <- list(local_snv = nirs4all_local_snv,
                     robust_snv = nirs4all_robust_snv,
                     area = nirs4all_area_normalization,
                     detrend = nirs4all_detrend,
                     msc = nirs4all_msc,
                     emsc = nirs4all_emsc)
python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON")
for (name in names(cases)) {
  operation <- cases[[name]]
  recipe <- list(pipeline = list(operation,
    list(model = list(class = "n4m.PLS",
                      params = list(n_components = 2L)))))
  step <- do.call(constructors[[name]],
                  if (is.null(operation$params)) list() else operation$params)
  manual <- nirs4all_pipeline(list(step), nirs4all_pls(2L))
  for (source in list(recipe,
                      as.character(jsonlite::toJSON(recipe, auto_unbox = TRUE)),
                      yaml::as.yaml(recipe))) {
    imported <- nirs4all_pipeline_from_portable(source)
    stopifnot(identical(imported$steps[[1L]], step))
    actual <- predict(nirs4all_fit(imported, X[train, , drop = FALSE], y[train]),
                      X[validation, , drop = FALSE])
    expected <- predict(nirs4all_fit(manual, X[train, , drop = FALSE], y[train]),
                        X[validation, , drop = FALSE])
    stopifnot(max(abs(actual - expected)) < 1e-12)
  }
  held_out <- recipe
  held_out$pipeline <- c(list(list(class = "n4m.KennardStone",
                                   params = list(test_size = 0.3))),
                         held_out$pipeline)
  evaluated <- nirs4all_run_portable_pipeline(held_out,
                                               list(X = X, y = y))
  split <- n4m::kennard_stone_split(X, test_size = 0.3, zero_based = TRUE)
  held_out_expected <- predict(nirs4all_fit(manual,
    X[split$train + 1L, , drop = FALSE], y[split$train + 1L]),
    X[split$test + 1L, , drop = FALSE])
  stopifnot(max(abs(evaluated$selected$predictions - held_out_expected)) < 1e-10)
  if (nzchar(python)) {
    helper <- if (file.exists("helpers/native_preprocessing_peer.py"))
      "helpers/native_preprocessing_peer.py" else
      "tests/helpers/native_preprocessing_peer.py"
    stopifnot(file.exists(helper), file.exists(python))
    request <- tempfile(fileext = ".json")
    response <- tempfile(fileext = ".json")
    rows <- function(values) lapply(seq_len(nrow(values)), function(index)
      unname(as.numeric(values[index, ])))
    writeLines(as.character(jsonlite::toJSON(list(
      train = rows(X[train, , drop = FALSE]),
      validation = rows(X[validation, , drop = FALSE]),
      steps = list(operation)), auto_unbox = TRUE, digits = 17)), request)
    output <- suppressWarnings(system2(python,
      c(shQuote(helper), shQuote(request), shQuote(response)),
      stdout = TRUE, stderr = TRUE))
    status <- attr(output, "status")
    if (is.null(status)) status <- 0L
    if (status != 0L)
      stop("Python n4m preprocessing oracle failed: ",
           paste(output, collapse = "\n"))
    oracle <- jsonlite::fromJSON(response)
    fit_transform <- getFromNamespace("nirs4all_fit_transform", "nirs4all")
    transform <- getFromNamespace("nirs4all_transform", "nirs4all")
    fitted <- fit_transform(X[train, , drop = FALSE], list(step))
    predicted <- transform(X[validation, , drop = FALSE], list(step),
                           fitted$states)
    stopifnot(max(abs(fitted$X - oracle$train)) < 1e-10,
              max(abs(predicted - oracle$validation)) < 1e-10)
    unlink(c(request, response))
  }
}

for (invalid in list(
  list(class = "n4m.LSNV", params = list(window = 4L)),
  list(class = "n4m.RNV", params = list(k = 0)),
  list(class = "n4m.AreaNormalization", params = list(method = "unknown")),
  list(class = "n4m.Detrend", params = list(polyorder = -1L)),
  list(class = "n4m.MSC", params = list(reference = c(1, 2))),
  list(class = "n4m.EMSC", params = list(degree = 0L)))) {
  recipe <- list(pipeline = list(invalid,
    list(model = list(class = "n4m.PLS", params = list(n_components = 2L)))))
  stopifnot(inherits(try(nirs4all_pipeline_from_portable(recipe), silent = TRUE),
                     "try-error"))
}
