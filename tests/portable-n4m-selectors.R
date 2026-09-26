library(nirs4all)

X <- outer(seq_len(37L), seq_len(12L), function(i, j)
  sin(i * j / 11) + cos(i / 3 + j / 7) + i * j / 170)
y <- 0.9 + 0.6 * X[, 3L] - 0.4 * X[, 9L]
train <- 1:28
held <- 29:37
cases <- list(
  wvc_threshold_select = list(),
  spa_select = list(top_k = 5L),
  wvc_select = list(top_k = 5L, normalize = FALSE),
  stability_select = list(top_k = 5L),
  cars_select = list(n_iterations = 8L, min_features = 3L),
  interval_select = list(interval_width = 3L, step = 1L),
  uve_select = list(noise_features = 12L, noise_seed = 7L),
  t2_select = list(alpha_thresholds = c(0.1, 0.3, 0.5), min_selected = 2L),
  random_frog_select = list(n_iterations = 8L, initial_size = 6L,
    min_size = 2L, max_size = 10L, top_k = 5L, seed = 7L),
  scars_select = list(n_iterations = 8L, min_features = 3L, seed = 7L),
  ga_select = list(n_generations = 5L, population_size = 8L,
    min_features = 2L, max_features = 10L, seed = 7L),
  pso_select = list(n_swarm = 8L, n_iterations = 5L, seed = 7L),
  vissa_select = list(n_iterations = 3L, n_submodels = 8L,
                      ratio_kept = 0.5, seed = 7L),
  shaving_select = list(n_steps = 3L, min_features = 3L),
  bve_select = list(n_steps = 3L, min_features = 3L),
  emcuve_select = list(noise_features = 12L, noise_seed = 7L,
                       n_ensembles = 3L),
  randomization_select = list(n_permutations = 100L,
                              randomization_seed = 7L, alpha = 0.5),
  bipls_select = list(interval_width = 3L, min_intervals = 1L),
  sipls_select = list(interval_width = 3L, combination_size = 2L),
  rep_select = list(n_steps = 3L, min_features = 3L, remove_count = 1L),
  ipw_select = list(n_iterations = 3L, top_k = 5L),
  st_select = list(thresholds = c(0.1, 0.5, 1), min_selected = 2L),
  iriv_select = list(max_rounds = 4L, seed = 7L),
  irf_select = list(n_iterations = 8L, window_size = 3L,
                    initial_intervals = 3L, top_k = 3L, seed = 7L),
  vip_spa_select = list(vip_threshold = 0.3, top_k = 5L))
stopifnot(length(cases) == 25L,
          setequal(names(cases), names(nirs4all:::.nirs4all_selector_params)))
fits <- list()
recipes <- list()

for (method in names(cases)) {
  step <- nirs4all_n4m_selector(method, 2L, cases[[method]])
  pipeline <- nirs4all_pipeline(list(step), nirs4all_pls(1L))
  source_fit <- nirs4all_fit(pipeline, X[train, , drop = FALSE], y[train])
  fits[[method]] <- source_fit
  expected <- predict(source_fit, X[held, , drop = FALSE])
  for (format in c("json", "yaml")) {
    recipe <- nirs4all_export_pipeline(pipeline, format)
    if (identical(format, "json"))
      recipes[[method]] <- jsonlite::fromJSON(recipe, simplifyVector = FALSE)
    imported <- nirs4all_pipeline_from_portable(recipe)
    fit <- nirs4all_fit(imported, X[train, , drop = FALSE], y[train])
    stopifnot(isTRUE(all.equal(imported$steps, pipeline$steps)),
      identical(nirs4all_portable_class_names(nirs4all_load_pipeline(recipe)),
                c("n4m.Selector", "n4m.PLS")),
      identical(fit$step_states, source_fit$step_states),
      identical(unname(nirs4all:::nirs4all_transform(
        X[held, , drop = FALSE], fit$steps, fit$step_states)),
        unname(X[held, sort(fit$step_states[[1L]]), drop = FALSE])),
      max(abs(predict(fit, X[held, , drop = FALSE]) - expected)) < 1e-10)
    if (identical(method, "wvc_threshold_select")) {
      definition <- nirs4all_load_pipeline(recipe)
      stopifnot(is.list(definition$pipeline[[1L]]$params$method_params),
        !length(definition$pipeline[[1L]]$params$method_params),
        !is.null(names(definition$pipeline[[1L]]$params$method_params)))
    }
  }
}

python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON")
python_root <- Sys.getenv("NIRS4ALL_PYTHON_FULL_ROOT")
if (nzchar(python) && nzchar(python_root)) {
  helper <- if (file.exists("helpers/portable_selector_peer.py"))
    "helpers/portable_selector_peer.py" else
    "tests/helpers/portable_selector_peer.py"
  request <- tempfile(fileext = ".json")
  response <- tempfile(fileext = ".json")
  matrix_rows <- function(value) lapply(seq_len(nrow(value)), function(index)
    unname(as.list(as.numeric(value[index, ]))))
  writeLines(as.character(jsonlite::toJSON(list(
    python_root = python_root, recipes = recipes,
    train = matrix_rows(X[train, , drop = FALSE]),
    held = matrix_rows(X[held, , drop = FALSE]),
    y = unname(as.list(y[train]))), auto_unbox = TRUE,
    digits = 17L)), request)
  output <- suppressWarnings(system2(python,
    c(shQuote(helper), shQuote(request), shQuote(response)),
    stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L)
    stop("Python selector recipe peer failed: ", paste(output, collapse = "\n"))
  peer <- jsonlite::fromJSON(response, simplifyVector = FALSE)
  for (method in names(cases)) {
    selected <- fits[[method]]$step_states[[1L]]
    projected <- X[held, sort(selected), drop = FALSE]
    result <- peer[[method]]
    held_selected <- do.call(rbind, lapply(result$held_selected, unlist))
    stopifnot(identical(as.integer(unlist(result$selected_indices)), selected - 1L),
      identical(dim(held_selected), dim(projected)),
      max(abs(held_selected - projected)) < 1e-12,
      max(abs(as.numeric(unlist(result$predictions)) -
        predict(fits[[method]], X[held, , drop = FALSE]))) < 1e-8)
  }
  unlink(c(request, response))
}

branch <- nirs4all_pipeline(list(nirs4all_concat(list(
  native = list(nirs4all_n4m_selector("wvc_select", 2L,
                                     list(top_k = 5L))),
  baseline = list(nirs4all_snv())))), nirs4all_pls(1L))
for (format in c("json", "yaml")) {
  imported <- nirs4all_pipeline_from_portable(
    nirs4all_export_pipeline(branch, format))
  stopifnot(isTRUE(all.equal(imported$steps, branch$steps)),
    max(abs(predict(nirs4all_fit(imported,
      X[train, , drop = FALSE], y[train]), X[held, , drop = FALSE]) -
      predict(nirs4all_fit(branch,
      X[train, , drop = FALSE], y[train]), X[held, , drop = FALSE]))) < 1e-10)
}

base <- list(pipeline = list(
  list(class = "n4m.Selector", params = list(
    method = "wvc_select", n_components = 2L,
    method_params = list(top_k = 5L))),
  list(model = list(class = "n4m.PLS", params = list(n_components = 1L)))))
bad <- list(
  list(method = "ridge", n_components = 2L,
       method_params = list(top_k = 5L)),
  list(method = "wvc_select", n_components = 2L,
       method_params = list()),
  list(method = "random_frog_select", n_components = 2L,
       method_params = list(top_k = 5L)),
  list(method = "t2_select", n_components = 2L,
       method_params = list()),
  list(method = "wvc_select", n_components = 2L,
       method_params = list(top_k = 5L, unknown = 1L)),
  list(method = "wvc_select", n_components = 2L,
       method_params = list(top_k = 5L, normalize = 0L)),
  list(method = "wvc_select", n_components = "2",
       method_params = list(top_k = 5L)))
for (params in bad) {
  candidate <- base
  candidate$pipeline[[1L]]$params <- params
  stopifnot(inherits(try(nirs4all_pipeline_from_portable(candidate),
                       silent = TRUE), "try-error"))
}
