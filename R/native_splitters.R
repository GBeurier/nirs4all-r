.nirs4all_native_splitter_methods <- list(
  kennard_stone = c("test_size"),
  spxy = c("test_size"),
  spxy_fold = c("n_splits", "y_metric"),
  spxy_group_fold = c("n_splits", "y_metric", "aggregation"),
  kmeans = c("test_size", "seed", "max_iter"),
  kbins_stratified = c("test_size", "n_bins", "strategy", "seed"),
  binned_strat_group_fold = c("n_splits", "n_bins", "strategy", "shuffle", "seed"),
  systematic_circular = c("test_size", "seed"),
  data_twinning = c("test_size", "seed")
)

.nirs4all_native_fold_kinds <- c("spxy_fold", "spxy_group_fold",
                                   "binned_strat_group_fold")
.nirs4all_native_group_kinds <- c("spxy_group_fold",
                                    "binned_strat_group_fold")
.nirs4all_native_y_kinds <- c("spxy", "spxy_fold", "spxy_group_fold",
                                "kbins_stratified", "binned_strat_group_fold",
                                "systematic_circular")
.nirs4all_native_x_kinds <- c("kennard_stone", "spxy", "spxy_fold",
                                "spxy_group_fold", "kmeans", "data_twinning")

#' Specify one of the nine n4m native sample splitters
#'
#' This object contains only a splitter name and its explicit parameters. The
#' numerical split is performed by n4m, never by the R product adapter. Fold
#' splitters may also be passed to [nirs4all_dag_cv_refit_predict()].
#'
#' @param kind Native splitter name; see `n4m::n4m_splitter_run()`.
#' @param ... Named parameters supported by this kind. Fold kinds require
#'   `n_splits`; other parameters retain n4m defaults.
#' @return A `nirs4all_native_splitter` specification.
#' @export
nirs4all_native_splitter <- function(kind, ...) {
  if (!is.character(kind) || length(kind) != 1L || is.na(kind) ||
      !(kind %in% names(.nirs4all_native_splitter_methods)))
    stop("unknown n4m native splitter kind", call. = FALSE)
  params <- list(...)
  if (length(params) && (is.null(names(params)) || anyNA(names(params)) ||
                         any(!nzchar(names(params))) || anyDuplicated(names(params))))
    stop("native splitter parameters must have unique names", call. = FALSE)
  if (!all(names(params) %in% .nirs4all_native_splitter_methods[[kind]]))
    stop("unsupported parameter for this native splitter kind", call. = FALSE)
  if (kind %in% .nirs4all_native_fold_kinds && is.null(params$n_splits))
    stop("native fold splitters require n_splits", call. = FALSE)
  structure(list(kind = kind, params = params),
            class = "nirs4all_native_splitter")
}

.nirs4all_native_ids <- function(X, sample_ids) {
  if (is.null(sample_ids)) sample_ids <- rownames(X)
  if (is.null(sample_ids)) sample_ids <- sprintf("sample:%08d", seq_len(nrow(X)))
  if (!is.character(sample_ids) || length(sample_ids) != nrow(X) ||
      anyNA(sample_ids) || any(!nzchar(trimws(sample_ids))) ||
      anyDuplicated(sample_ids) ||
      (!is.null(rownames(X)) && !identical(rownames(X), sample_ids)))
    stop("sample_ids must be unique strings aligned to X", call. = FALSE)
  sample_ids
}

.nirs4all_native_split <- function(splitter, X, y, sample_ids, group_ids,
                                   fold) {
  if (!inherits(splitter, "nirs4all_native_splitter") ||
      !is.list(splitter$params) ||
      !(splitter$kind %in% names(.nirs4all_native_splitter_methods)))
    stop("splitter must be a native splitter specification", call. = FALSE)
  X <- nirs4all_matrix(X)
  sample_ids <- .nirs4all_native_ids(X, sample_ids)
  kind <- splitter$kind
  needs_y <- kind %in% .nirs4all_native_y_kinds
  needs_group <- kind %in% .nirs4all_native_group_kinds
  if (!needs_y && !is.null(y))
    stop("y is unused by this native splitter kind", call. = FALSE)
  if (needs_y && (!is.numeric(y) || is.matrix(y) ||
                  length(y) != nrow(X) || anyNA(y) || any(!is.finite(y)) ||
                  (!is.null(names(y)) && !identical(names(y), sample_ids))))
    stop("native splitter y must be finite numeric and aligned to sample_ids",
         call. = FALSE)
  if (needs_group != !is.null(group_ids))
    stop("group_ids are required only for native group-fold splitters",
         call. = FALSE)
  if (needs_group) {
    if (is.factor(group_ids)) group_ids <- as.character(group_ids)
    if (!is.character(group_ids) || length(group_ids) != nrow(X) ||
        anyNA(group_ids) || any(!nzchar(trimws(group_ids))) ||
        (!is.null(names(group_ids)) && !identical(names(group_ids), sample_ids)))
      stop("group_ids must be non-empty strings aligned to sample_ids",
           call. = FALSE)
    # C receives exact integer category IDs; the original labels remain in
    # the DAG relations and fold set. Encoding does not choose a split.
    native_groups <- match(unname(group_ids), unique(unname(group_ids)))
  } else native_groups <- NULL
  fold_kind <- kind %in% .nirs4all_native_fold_kinds
  if (!is.numeric(fold) || length(fold) != 1L || is.na(fold) ||
      !is.finite(fold) || fold != floor(fold) ||
      fold < 1L || fold > .Machine$integer.max ||
      (!fold_kind && fold != 1L))
    stop("invalid native fold number", call. = FALSE)
  args <- c(list(kind = kind,
                 X = if (kind %in% .nirs4all_native_x_kinds) X else NULL,
                 Y = if (needs_y) matrix(as.double(y), ncol = 1L) else NULL,
                 groups = native_groups, fold = as.integer(fold)),
            splitter$params)
  if (!("n4m_splitter_run" %in% getNamespaceExports("n4m")))
    stop("n4m native splitter ABI is unavailable", call. = FALSE)
  indices <- do.call(n4m::n4m_splitter_run, args)
  train <- indices$train
  test <- indices$test
  n <- nrow(X)
  if (!is.integer(train) || !is.integer(test) || !length(train) ||
      !length(test) || anyNA(train) || anyNA(test) ||
      any(train < 1L | train > n) || any(test < 1L | test > n) ||
      anyDuplicated(train) || anyDuplicated(test) ||
      length(train) + length(test) != n ||
      length(intersect(train, test)))
    stop("native splitter did not return an exact sample partition", call. = FALSE)
  if (needs_group &&
      length(intersect(unique(group_ids[train]), unique(group_ids[test]))))
    stop("native group split crosses group boundaries", call. = FALSE)
  structure(list(train = train, test = test,
                 train_sample_ids = unname(sample_ids[train]),
                 test_sample_ids = unname(sample_ids[test]),
                 sample_ids = sample_ids, kind = kind, fold = as.integer(fold)),
            class = "nirs4all_native_split")
}

#' Split sample identities with the n4m native splitter engine
#'
#' Returns the native train/test index order and the corresponding sample IDs.
#' For fold kinds, `fold` is one-based. No sample assignment is computed in R.
#'
#' @param splitter A [nirs4all_native_splitter()] specification.
#' @param X Finite numeric samples-by-features matrix.
#' @param y Optional finite numeric target vector, required by target-aware kinds.
#' @param sample_ids Optional unique sample IDs, aligned to matrix row names.
#' @param group_ids Optional aligned character or factor group IDs, required by
#'   group-fold kinds and refused for non-group kinds.
#' @param fold One-based fold number for fold kinds.
#' @return Native index partition and corresponding sample IDs.
#' @export
nirs4all_native_split <- function(splitter, X, y = NULL, sample_ids = NULL,
                                 group_ids = NULL, fold = 1L) {
  .nirs4all_native_split(splitter, X, y, sample_ids, group_ids, fold)
}

.nirs4all_native_cv_assignment <- function(splitter, X, y, sample_ids,
                                            group_ids, folds) {
  splits <- lapply(seq_len(as.integer(folds)), function(index)
    .nirs4all_native_split(splitter, X, y, sample_ids, group_ids, index))
  assignment <- integer(nrow(X))
  for (index in seq_along(splits)) {
    test <- splits[[index]]$test
    if (any(assignment[test] != 0L))
      stop("native validation folds overlap", call. = FALSE)
    assignment[test] <- index
  }
  if (any(assignment == 0L))
    stop("native validation folds do not cover every sample", call. = FALSE)
  list(splits = splits, fold_number = assignment)
}
