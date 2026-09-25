library(nirs4all)
# Exact copies of nirs4all-core/tests/parity/fixtures at transfer time.
# Pinning their bytes makes later upstream fixture changes an explicit review.
fixture_md5 <- c(
  execution_contract_cases.json = "00529a1e435aebed79ca228335640812",
  portable_kennard_stone_snv_pls.json = "2fd94920f19bb5c6834676bbabc5836e",
  portable_methods_pipeline.json = "3b3960e41c925c17fb3dbf9470ee8789",
  portable_savgol_pls.json = "aa12b986359c3a05cfa057880292797b",
  portable_snv_pls.json = "4c0518fc5b6642101d9c2f6fe7bf9965",
  portable_kennard_stone_snv_pls.yaml = "a2fb736ad099381022d8641b061fe53b",
  portable_methods_pipeline.yaml = "fb05c91a82bad23b23dd71ec9f66bf52",
  portable_savgol_pls.yaml = "a51e2f41b2b2f539a658c5801392efbe",
  portable_snv_pls.yaml = "d841b61e99ecfea5308b660a6f475f45")
fixture_paths <- vapply(names(fixture_md5), function(name)
  system.file("extdata", name, package = "nirs4all", mustWork = TRUE), character(1))
stopifnot(identical(unname(tools::md5sum(fixture_paths)), unname(fixture_md5)))
contract <- jsonlite::fromJSON(system.file(
  "extdata", "execution_contract_cases.json", package = "nirs4all",
  mustWork = TRUE), simplifyVector = FALSE)
for (case in contract$invalid)
  stopifnot(inherits(try(nirs4all_parse_execution_plan(case), silent = TRUE),
                     "try-error"))
for (case in contract$valid)
  stopifnot(identical(nirs4all_parse_execution_plan(case)$n_components,
                      as.integer(unlist(case$components))))
oracle <- jsonlite::fromJSON(system.file(
  "extdata", "python_oracle_n4m_examples.json", package = "nirs4all",
  mustWork = TRUE), simplifyVector = FALSE)
dataset <- list(X = oracle$dataset$X, y = oracle$dataset$y,
                rows = oracle$dataset$rows, cols = oracle$dataset$cols)
classes <- list(
  ks = "nirs4all.operators.splitters.KennardStoneSplitter",
  snv = "nirs4all.operators.transforms.StandardNormalVariate",
  sg = "nirs4all.operators.transforms.SavitzkyGolay",
  pls = "sklearn.cross_decomposition.PLSRegression")
for (case in oracle$cases) {
  name <- case$name
  steps <- list()
  if (name %in% c("portable_kennard_stone_snv_pls", "portable_methods_pipeline"))
    steps[[length(steps) + 1L]] <- list(class = classes$ks,
                                       params = list(test_size = 0.3))
  if (name %in% c("portable_snv_pls", "portable_kennard_stone_snv_pls",
                  "portable_methods_pipeline"))
    steps[[length(steps) + 1L]] <- list(class = classes$snv)
  if (name %in% c("portable_savgol_pls", "portable_methods_pipeline"))
    steps[[length(steps) + 1L]] <- list(class = classes$sg,
      params = list(window_length = 11L, polyorder = 2L, deriv = 0L))
  components <- vapply(case$variants, function(item)
    as.integer(item$n_components), integer(1))
  model <- list(model = list(class = classes$pls))
  if (length(components) == 1L) {
    model$model$params <- list(n_components = components[[1L]])
  } else {
    model$`_range_` <- c(components[[1L]], components[[length(components)]],
                        components[[2L]] - components[[1L]])
    model$param <- "n_components"
  }
  steps[[length(steps) + 1L]] <- model
  definition <- list(name = name, random_state = 42L, pipeline = steps)
  json_path <- tempfile(fileext = ".json")
  yaml_path <- tempfile(fileext = ".yaml")
  writeLines(jsonlite::toJSON(definition, auto_unbox = TRUE, pretty = TRUE), json_path)
  writeLines(yaml::as.yaml(definition), yaml_path)
  installed_paths <- vapply(c("json", "yaml"), function(extension)
    system.file("extdata", paste0(name, ".", extension),
                package = "nirs4all", mustWork = TRUE), character(1))
  for (path in c(json_path, yaml_path, installed_paths)) {
    loaded <- nirs4all_load_pipeline(path)
    stopifnot(inherits(loaded, "nirs4all_pipeline_definition"),
              identical(loaded$name, name),
              identical(tail(nirs4all_portable_class_names(loaded), 1L), classes$pls))
    plan <- nirs4all_parse_execution_plan(loaded)
    stopifnot(identical(plan$n_components, components))
    result <- nirs4all_run_portable_pipeline(path, dataset)
    stopifnot(identical(result$split$kind, case$split$kind),
              identical(result$split$trainIndices,
                        as.integer(unlist(case$split$trainIndices))),
              identical(result$split$testIndices,
                        as.integer(unlist(case$split$testIndices))),
              length(result$variants) == length(case$variants),
              max(abs(result$targets - as.numeric(unlist(case$targets)))) < 1e-12)
    for (index in seq_along(result$variants)) {
      actual <- result$variants[[index]]
      expected <- case$variants[[index]]
      stopifnot(actual$n_components == expected$n_components,
                max(abs(actual$predictions -
                        as.numeric(unlist(expected$predictions)))) < 1e-10,
                abs(actual$rmse - expected$rmse) < 1e-10)
    }
    stopifnot(result$selected$n_components == case$selected$n_components,
              identical(result$evaluation$independent_test, FALSE))
  }
  unlink(c(json_path, yaml_path))
}

with_comment <- nirs4all_load_pipeline(paste(
  "steps:",
  "  - _comment: this is not an operator",
  "  - class: nirs4all.operators.transforms.SNV",
  "  - model:",
  "      class: sklearn.cross_decomposition.PLSRegression",
  sep = "\n"))
stopifnot(length(with_comment$pipeline) == 2L)
unsupported <- list(pipeline = list(list(class = "sklearn.ensemble.RandomForestRegressor"),
                                    list(model = list(class = classes$pls))))
stopifnot(inherits(try(nirs4all_parse_execution_plan(unsupported), silent = TRUE),
                   "try-error"))

# A Python n4m class name and a language-neutral method ID resolve to the
# same native R controller; non-default SNV settings must not be discarded.
native_alias <- list(pipeline = list(
  list(class = "n4m.SNV", params = list(ddof = 1L)),
  list(model = list(class = "n4m.PLS", params = list(
    n_components = 2L, algo = "pls_simpls")))))
semantic_alias <- native_alias
semantic_alias$pipeline[[1L]]$class <- "preprocessing.snv"
semantic_alias$pipeline[[2L]]$model$class <- "models.pls.pls_fit_simple"
for (definition in list(native_alias, semantic_alias)) {
  result <- nirs4all_run_portable_pipeline(definition, dataset)
  fitted <- nirs4all_fit(nirs4all_pipeline(
    list(nirs4all_snv(ddof = 1L)), nirs4all_pls(2L)),
    matrix(as.numeric(unlist(dataset$X)), dataset$rows, dataset$cols,
           byrow = TRUE), as.numeric(unlist(dataset$y)))
  stopifnot(max(abs(result$selected$predictions -
                    nirs4all_predict(fitted, matrix(
                      as.numeric(unlist(dataset$X)), dataset$rows,
                      dataset$cols, byrow = TRUE)))) < 1e-10)
}
bad_params <- native_alias
bad_params$pipeline[[1L]]$params$unknown <- 1L
stopifnot(inherits(try(nirs4all_parse_execution_plan(bad_params), silent = TRUE),
                   "try-error"))
