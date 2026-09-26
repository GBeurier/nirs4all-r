library(nirs4all)

X <- outer(seq_len(28L), seq_len(8L), function(i, j)
  sin(i * j / 10) + cos(i / 3 + j / 8) + i * j / 110)
colnames(X) <- paste0("wl", seq_len(ncol(X)))
y <- 1.2 + 0.65 * X[, 2L] - 0.3 * X[, 6L]
held <- X[c(3L, 11L, 23L), , drop = FALSE] + 0.047
groups <- stats::setNames(rep(c(2L, 9L), each = 4L), colnames(X))
python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON", "")
if (nzchar(python)) {
  helper <- if (file.exists("helpers/portable_groupsparse_peer.py"))
    "helpers/portable_groupsparse_peer.py" else
    "tests/helpers/portable_groupsparse_peer.py"
  stopifnot(file.exists(helper), file.exists(python))
}
rows <- function(matrix) lapply(seq_len(nrow(matrix)), function(i)
  unname(as.numeric(matrix[i, ])))

for (steps in list(list(), list(nirs4all_snv()))) {
  # SNV drops column names; the portable assignment is explicitly positional.
  assignment <- if (length(steps)) unname(groups) else groups
  pipeline <- nirs4all_pipeline(steps,
    nirs4all_group_sparse_pls(2L, assignment, 0.2))
  expected <- predict(nirs4all_fit(pipeline, X, y), held)
  for (format in c("json", "yaml")) {
    exported <- nirs4all_export_pipeline(pipeline, format)
    recipe <- if (identical(format, "json"))
      jsonlite::fromJSON(exported, simplifyVector = FALSE) else
      yaml::yaml.load(exported)
    model <- tail(recipe$pipeline, 1L)[[1L]]$model
    stopifnot(identical(model$class, "n4m.GroupSparsePLS"),
      identical(unlist(model$params$group_assignment, use.names = FALSE),
        unname(groups)),
      identical(as.numeric(model$params$group_lambda), 0.2),
      identical(as.integer(model$params$n_components), 2L),
      identical(tail(nirs4all_portable_class_names(
        nirs4all_load_pipeline(exported)), 1L), "n4m.GroupSparsePLS"))
    restored <- nirs4all_pipeline_from_portable(exported)
    stopifnot(identical(restored$learner$spec$params$group_assignment,
        unname(groups)),
      identical(restored$learner$spec$params$group_lambda, 0.2),
      max(abs(predict(nirs4all_fit(restored, X, y), held) - expected)) < 1e-10,
      max(abs(nirs4all_run_portable_pipeline(exported,
        list(X = X, y = y))$selected$predictions -
        predict(nirs4all_fit(restored, X, y), X))) < 1e-10)
    restored_fit <- nirs4all_fit(restored, X, y)
    stopifnot(inherits(try(predict(restored_fit,
      held[, rev(seq_len(ncol(held))), drop = FALSE]),
      silent = TRUE), "try-error"))
    if (nzchar(python)) {
      request <- tempfile(fileext = ".json")
      response <- tempfile(fileext = ".json")
      writeLines(as.character(jsonlite::toJSON(list(
        recipe = recipe, train = rows(X), held = rows(held),
        y = unname(as.numeric(y))), auto_unbox = TRUE, digits = 17L)),
        request)
      output <- suppressWarnings(system2(python,
        c(shQuote(helper), shQuote(request), shQuote(response)),
        stdout = TRUE, stderr = TRUE))
      status <- attr(output, "status")
      if (!is.null(status) && status != 0L)
        stop("Python GroupSparsePLS recipe peer failed: ",
          paste(output, collapse = "\n"))
      actual <- as.numeric(jsonlite::fromJSON(response)$predictions)
      stopifnot(length(actual) == length(expected),
        max(abs(actual - expected)) < 1e-10)
      unlink(c(request, response))
    }
  }
}

valid <- list(pipeline = list(list(model = list(
  class = "n4m.GroupSparsePLS", params = list(n_components = 2L,
    group_assignment = as.list(unname(groups)), group_lambda = 0.2)))))
for (change in list(
  function(recipe) { recipe$pipeline[[1L]]$model$params$group_assignment <- NULL; recipe },
  function(recipe) { recipe$pipeline[[1L]]$model$params$group_assignment <- as.list(unname(groups[-1L])); recipe },
  function(recipe) { recipe$pipeline[[1L]]$model$params$group_assignment <- as.list(groups); recipe },
  function(recipe) { recipe$pipeline[[1L]]$model$params$group_assignment[[1L]] <- -1L; recipe },
  function(recipe) { recipe$pipeline[[1L]]$model$params$group_assignment[[1L]] <- 0.5; recipe },
  function(recipe) { recipe$pipeline[[1L]]$model$params$group_lambda <- -0.1; recipe },
  function(recipe) { recipe$pipeline[[1L]]$model$params$group_lambda <- Inf; recipe },
  function(recipe) { recipe$pipeline[[1L]]$model$params$n_components <- 0L; recipe },
  function(recipe) { recipe$pipeline[[1L]]$model$params$n_components <- "2"; recipe },
  function(recipe) { recipe$pipeline[[1L]]$model$params$extra <- 1L; recipe })) {
  invalid <- change(valid)
  stopifnot(inherits(try(nirs4all_pipeline_from_portable(invalid),
    silent = TRUE), "try-error") ||
    inherits(try(nirs4all_fit(nirs4all_pipeline_from_portable(invalid),
      X, y), silent = TRUE), "try-error"))
}
stopifnot(inherits(try(nirs4all_fit(
  nirs4all_pipeline_from_portable(list(pipeline = list(
    list(class = "n4m.SPA", params = list(top_k = 4L)),
    valid$pipeline[[1L]]))), X, y), silent = TRUE), "try-error"))

message("portable GroupSparsePLS JSON/YAML R roundtrip and Python held-out oracle passed")
