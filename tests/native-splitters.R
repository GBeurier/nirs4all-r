library(nirs4all)

samples <- seq_len(30L)
X <- cbind(as.double(samples), as.double((samples * 7L) %% 13L))
sample_ids <- sprintf("sample:%04d", samples)
rownames(X) <- sample_ids
y <- as.double(samples %% 11L)
names(y) <- sample_ids
groups <- rep(sprintf("batch:%02d", seq_len(15L)), each = 2L)
names(groups) <- sample_ids

specs <- list(
  kennard_stone = nirs4all_native_splitter("kennard_stone", test_size = .25),
  spxy = nirs4all_native_splitter("spxy", test_size = .25),
  spxy_fold = nirs4all_native_splitter("spxy_fold", n_splits = 3L, y_metric = 1L),
  spxy_group_fold = nirs4all_native_splitter("spxy_group_fold", n_splits = 3L,
                                             y_metric = 1L, aggregation = 0L),
  kmeans = nirs4all_native_splitter("kmeans", test_size = .25,
                                    seed = 42, max_iter = 100L),
  kbins_stratified = nirs4all_native_splitter("kbins_stratified",
                                              test_size = .25, seed = 42,
                                              n_bins = 2L, strategy = 0L),
  binned_strat_group_fold = nirs4all_native_splitter("binned_strat_group_fold",
                                                     n_splits = 3L, n_bins = 2L,
                                                     strategy = 0L, shuffle = TRUE,
                                                     seed = 42),
  systematic_circular = nirs4all_native_splitter("systematic_circular",
                                                  test_size = .25, seed = 42),
  data_twinning = nirs4all_native_splitter("data_twinning",
                                           test_size = .25, seed = 42)
)
group_kinds <- c("spxy_group_fold", "binned_strat_group_fold")
target_kinds <- c("spxy", "spxy_fold", "spxy_group_fold", "kbins_stratified",
                  "binned_strat_group_fold", "systematic_circular")
results <- lapply(names(specs), function(kind) {
  result <- nirs4all_native_split(specs[[kind]], X,
    y = if (kind %in% target_kinds) y else NULL,
    sample_ids = sample_ids,
    group_ids = if (kind %in% group_kinds) groups else NULL)
  stopifnot(identical(c(result$train_sample_ids, result$test_sample_ids),
                      sample_ids[c(result$train, result$test)]),
            identical(sort(c(result$train, result$test)), samples),
            identical(result, nirs4all_native_split(specs[[kind]], X,
              y = if (kind %in% target_kinds) y else NULL,
              sample_ids = sample_ids,
              group_ids = if (kind %in% group_kinds) groups else NULL)))
  if (kind %in% group_kinds)
    stopifnot(!length(intersect(groups[result$train], groups[result$test])))
  result
})
names(results) <- names(specs)

python <- Sys.getenv("NIRS4ALL_METHODS_PYTHON", "")
if (nzchar(python)) {
  helper <- if (file.exists("helpers/native_splitter_peer.py"))
    "helpers/native_splitter_peer.py" else "tests/helpers/native_splitter_peer.py"
  output <- suppressWarnings(system2(python, shQuote(helper),
                                      stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L)
    stop("Python splitter peer failed: ", paste(output, collapse = "\n"))
  oracle <- jsonlite::fromJSON(paste(output, collapse = "\n"),
                               simplifyVector = FALSE)
  for (kind in names(specs)) {
    stopifnot(identical(as.integer(unlist(oracle[[kind]]$train, use.names = FALSE)) + 1L,
                        results[[kind]]$train),
              identical(as.integer(unlist(oracle[[kind]]$test, use.names = FALSE)) + 1L,
                        results[[kind]]$test))
  }
}

wrong_groups <- groups
names(wrong_groups) <- rev(sample_ids)
stopifnot(inherits(try(nirs4all_native_split(specs$spxy_group_fold, X, y,
                                            sample_ids, wrong_groups), silent = TRUE),
                   "try-error"),
          inherits(try(nirs4all_native_split(specs$spxy_fold, X, y,
                                            sample_ids, groups), silent = TRUE),
                   "try-error"),
          inherits(try(nirs4all_native_splitter("spxy_fold", test_size = .25),
                       silent = TRUE), "try-error"),
          inherits(try(nirs4all_native_splitter("kennard_stone", n_splits = 3L),
                       silent = TRUE), "try-error"),
          inherits(try(nirs4all_native_split(specs$spxy, X, y[-1L]),
                       silent = TRUE), "try-error"))

message("nine native splitter partitions, identities and Python peer passed")
