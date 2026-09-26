library(nirs4all)

X <- outer(seq_len(37L), seq_len(12L), function(i, j)
  sin(i * j / 11) + cos(i / 3 + j / 7) + i * j / 170)
y <- 0.9 + 0.6 * X[, 3L] - 0.4 * X[, 9L]
train <- seq_len(28L)
validation <- 29:37
cases <- list(
  spa_select = list(top_k = 5L),
  wvc_select = list(top_k = 5L, normalize = FALSE),
  stability_select = list(top_k = 5L),
  interval_select = list(interval_width = 3L, step = 1L),
  cars_select = list(n_iterations = 8L, min_features = 3L),
  uve_select = list(noise_features = 12L, noise_seed = 7L),
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
  t2_select = list(alpha_thresholds = c(0.1, 0.3, 0.5), min_selected = 2L),
  wvc_threshold_select = list(normalize = TRUE, threshold = 0.1,
                              min_selected = 2L),
  emcuve_select = list(noise_features = 12L, noise_seed = 7L,
                       n_ensembles = 3L, vote_threshold = 0.5),
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

for (method in names(cases)) {
  step <- nirs4all_n4m_selector(method, 2L, cases[[method]])
  pipeline <- nirs4all_pipeline(list(step), nirs4all_pls(1L))
  fit <- nirs4all_fit(pipeline, X[train, , drop = FALSE], y[train])
  native <- n4m::n4m_method(method, X[train, , drop = FALSE], y[train],
                           2L, params = step$params)
  selected <- fit$step_states[[1L]]
  stopifnot(identical(selected, as.integer(native$selected_indices)),
            length(selected) > 0L, !anyDuplicated(selected),
            identical(unname(nirs4all:::nirs4all_transform(
              X[validation, , drop = FALSE], fit$steps, fit$step_states)),
              unname(X[validation, sort(selected), drop = FALSE])))
  path <- tempfile(fileext = ".rds")
  nirs4all_save(fit, path)
  restored <- nirs4all_load(path)
  stopifnot(identical(restored$step_states, fit$step_states),
            max(abs(predict(restored, X[validation, , drop = FALSE]) -
                    predict(fit, X[validation, , drop = FALSE]))) < 1e-12)
  unlink(path)
}

# Test target leakage and branch isolation with a stochastic native selector.
step <- nirs4all_n4m_selector("random_frog_select", 2L,
  cases$random_frog_select)
pipeline <- nirs4all_pipeline(list(nirs4all_concat(list(
  selected = list(step), original = list(nirs4all_snv())))), nirs4all_pls(1L))
fit <- nirs4all_fit(pipeline, X[train, , drop = FALSE], y[train])
changed_y <- y
changed_y[validation] <- changed_y[validation] + 1e6
again <- nirs4all_fit(pipeline, X[train, , drop = FALSE], changed_y[train])
stopifnot(identical(fit$step_states, again$step_states),
          identical(fit$step_states[[1L]]$selected[[1L]],
                    as.integer(n4m::n4m_method("random_frog_select",
                      X[train, , drop = FALSE], y[train], 2L,
                      params = step$params)$selected_indices)))

bad <- list(
  function() nirs4all_n4m_selector("ridge"),
  function() nirs4all_n4m_selector("wvc_select", params = list(unknown = 1)),
  function() nirs4all_n4m_selector("ga_select", params = list(seed = -1)),
  function() nirs4all_n4m_selector("wvc_select", params = list(top_k = 1.5)),
  function() nirs4all_n4m_selector("t2_select"),
  function() nirs4all_fit(nirs4all_pipeline(list(
    nirs4all_n4m_selector("randomization_select", 2L,
      list(n_permutations = 8L, randomization_seed = 7L, alpha = 0.05)))),
    X[train, , drop = FALSE], y[train]),
  function() nirs4all_fit(nirs4all_pipeline(list(
    nirs4all_n4m_selector("spa_select", params = list(top_k = 13L)))),
    X[train, , drop = FALSE], y[train]),
  function() nirs4all:::nirs4all_transform(X[validation, , drop = FALSE],
    list(step)))
stopifnot(all(vapply(bad, function(f)
  inherits(try(f(), silent = TRUE), "try-error"), logical(1))))

cli <- Sys.getenv("NIRS4ALL_DAGML_CLI")
if (nzchar(cli) && file.exists(cli) &&
    requireNamespace("dagml", quietly = TRUE)) {
  pipeline <- nirs4all_pipeline(list(
    nirs4all_n4m_selector("wvc_select", 2L,
                          list(top_k = 5L, normalize = FALSE))),
    nirs4all_pls(1L))
  for (split_steps in c(FALSE, TRUE)) {
    outcome <- nirs4all_dag_cv_refit_predict(pipeline, X, y, folds = 3L,
      cli = cli, split_steps = split_steps)
    expected <- predict(nirs4all_fit(pipeline, X, y),
                        X[validation, , drop = FALSE])
    stopifnot(max(abs(nirs4all_dag_predict(outcome,
      X[validation, , drop = FALSE]) - expected)) < 1e-10)
    expected_oof <- numeric(nrow(X))
    for (fold in 0:2) {
      held <- seq_len(nrow(X))[(seq_len(nrow(X)) - 1L) %% 3L == fold]
      training <- setdiff(seq_len(nrow(X)), held)
      local <- nirs4all_fit(pipeline, X[training, , drop = FALSE], y[training])
      expected_oof[held] <- predict(local, X[held, , drop = FALSE])
    }
    ids <- sprintf("sample:%08d", seq_len(nrow(X)))
    for (average in outcome$oof_average_results) {
      block <- average$aggregated_predictions[[1L]]
      block_ids <- vapply(block$unit_ids, `[[`, "", "id")
      values <- vapply(block$values, function(value)
        as.numeric(value[[1L]]), numeric(1))
      stopifnot(length(values) == nrow(X),
                max(abs(values - expected_oof[match(block_ids, ids)])) < 1e-10)
    }
  }
  # Numeric vector parameters survive the JSON adapter boundary in both DAG
  # layouts as well as scalar and integer parameters above.
  vector_pipeline <- nirs4all_pipeline(list(nirs4all_n4m_selector(
    "t2_select", 2L, cases$t2_select)), nirs4all_pls(1L))
  for (split_steps in c(FALSE, TRUE)) {
    outcome <- nirs4all_dag_cv_refit_predict(vector_pipeline, X, y,
      folds = 3L, cli = cli, split_steps = split_steps)
    expected <- predict(nirs4all_fit(vector_pipeline, X, y),
                        X[validation, , drop = FALSE])
    stopifnot(max(abs(nirs4all_dag_predict(outcome,
      X[validation, , drop = FALSE]) - expected)) < 1e-10)
  }
}
