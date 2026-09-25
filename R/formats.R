#' Convert a nirs4all-formats spectral dataset for modelling
#'
#' The Rust-backed `nirs4allformats` package owns file detection and decoding.
#' This function only checks the rectangular R view, records explicit sample
#' and spectral-axis identities, and selects one finite numeric target.
#'
#' @param source A file path or a `nirs4allformats_dataset`.
#' @param target Name of one numeric target column, or `NULL` for prediction.
#' @param signal Optional signal name when `source` is a file path.
#' @return A `nirs4all_dataset` with `X`, optional `y`, sample IDs, axis and
#'   source metadata. Pass it directly to [nirs4all_fit()] or [predict()].
#' @export
nirs4all_from_formats <- function(source, target = NULL, signal = NULL) {
  if (!requireNamespace("nirs4allformats", quietly = TRUE))
    stop("Install the optional 'nirs4allformats' package first", call. = FALSE)
  if (is.character(source) && length(source) == 1L && !is.na(source)) {
    dataset <- nirs4allformats::nirs4allformats_open_dataset(source, signal = signal)
  } else if (inherits(source, "nirs4allformats_dataset")) {
    if (!is.null(signal))
      stop("signal can only be selected while reading a file", call. = FALSE)
    dataset <- source
  } else {
    stop("source must be a file path or nirs4allformats_dataset", call. = FALSE)
  }
  X <- nirs4all_matrix(as.matrix(dataset))
  ids <- dataset$sample_ids
  wavelengths <- dataset$wavelengths
  if (!is.character(ids) || length(ids) != nrow(X) || anyNA(ids) ||
      any(!nzchar(trimws(ids))) || anyDuplicated(ids))
    stop("formats sample IDs must be unique non-empty strings", call. = FALSE)
  if (!is.numeric(wavelengths) || length(wavelengths) != ncol(X) ||
      anyNA(wavelengths) || any(!is.finite(wavelengths)))
    stop("formats spectral axis must contain one finite coordinate per feature",
         call. = FALSE)
  unit <- dataset$axis_unit
  kind <- if (is.null(dataset$axis_kind)) "unspecified" else dataset$axis_kind
  signal_type <- dataset$signal_type
  if (!is.character(unit) || length(unit) != 1L || is.na(unit) || !nzchar(unit) ||
      !is.character(kind) || length(kind) != 1L || is.na(kind) || !nzchar(kind) ||
      !is.character(signal_type) || length(signal_type) != 1L ||
      is.na(signal_type) || !nzchar(signal_type))
    stop("formats axis kind, unit and signal type are required", call. = FALSE)
  if (!is.null(target) && (!is.character(target) || length(target) != 1L ||
                          is.na(target) || !nzchar(target)))
    stop("target must name one numeric column", call. = FALSE)
  y <- NULL
  if (!is.null(target)) {
    if (!is.data.frame(dataset$targets) || !(target %in% names(dataset$targets)))
      stop("requested target is not present in the formats dataset", call. = FALSE)
    y <- dataset$targets[[target]]
    if (!is.numeric(y) || length(y) != nrow(X) || anyNA(y) ||
        any(!is.finite(y)))
      stop("requested formats target must be finite and numeric for every sample",
           call. = FALSE)
    y <- stats::setNames(as.numeric(y), ids)
  }
  rownames(X) <- ids
  colnames(X) <- paste0("axis:", signal_type, ":", kind, ":", unit, ":",
                        sprintf("%08d", seq_along(wavelengths)), ":",
                        sprintf("%.17g", wavelengths))
  structure(list(X = X, y = y, sample_ids = ids,
                 wavelengths = wavelengths, axis_kind = kind, axis_unit = unit,
                 signal_type = signal_type, target_name = target,
                 metadata = dataset$metadata, formats = dataset$formats,
                 provenance = dataset$provenance),
            class = "nirs4all_dataset")
}
