# Domain discovery moved from the former nirs4all-core R facade. Loading a
# namespace is optional; parsing and numerical methods remain owned upstream.
.nirs4all_upstream_registry <- list(
  dag_ml = list(package = "dagml", role = "Leakage-safe DAG/ML coordinator"),
  dag_ml_data = list(package = "dagmldata", role = "Sample-aligned DAG/ML data contracts"),
  formats = list(package = "nirs4allformats", role = "Spectroscopy file readers"),
  io = list(package = "nirs4allio", role = "Dataset assembly bridge"),
  datasets = list(package = "nirs4alldatasets", role = "NIRS dataset catalog"),
  methods = list(package = "n4m", role = "Portable NIRS methods engine")
)

#' Discover upstream R packages
#' @return Data frame with domain key, package, availability and role.
#' @export
nirs4all_upstreams <- function() {
  data.frame(
    key = names(.nirs4all_upstream_registry),
    package = vapply(.nirs4all_upstream_registry, `[[`, character(1), "package"),
    available = vapply(.nirs4all_upstream_registry, function(item)
      requireNamespace(item$package, quietly = TRUE), logical(1)),
    role = vapply(.nirs4all_upstream_registry, `[[`, character(1), "role"),
    row.names = NULL
  )
}

#' Load an upstream domain namespace
#' @param name One of the keys from [nirs4all_upstreams()].
#' @return The installed package namespace.
#' @export
nirs4all_require <- function(name) {
  if (!is.character(name) || length(name) != 1L || is.na(name) ||
      !name %in% names(.nirs4all_upstream_registry))
    stop("unknown nirs4all upstream domain", call. = FALSE)
  package <- .nirs4all_upstream_registry[[name]]$package
  if (!requireNamespace(package, quietly = TRUE))
    stop(sprintf("nirs4all upstream '%s' requires R package '%s'", name, package),
         call. = FALSE)
  asNamespace(package)
}

#' Access an upstream domain namespace
#' @return The loaded upstream R namespace.
#' @name nirs4all_domains
NULL

#' @rdname nirs4all_domains
#' @export
formats <- function() nirs4all_require("formats")
#' @rdname nirs4all_domains
#' @export
io <- function() nirs4all_require("io")
#' @rdname nirs4all_domains
#' @export
datasets <- function() nirs4all_require("datasets")
#' @rdname nirs4all_domains
#' @export
methods <- function() nirs4all_require("methods")
#' @rdname nirs4all_domains
#' @export
dag_ml <- function() nirs4all_require("dag_ml")
#' @rdname nirs4all_domains
#' @export
dag_ml_data <- function() nirs4all_require("dag_ml_data")

#' Use DAG-ML's process-local loss and metric registry
#' @return The upstream `dagml` registry; no registry is reimplemented here.
#' @export
nirs4all_local_implementation_registry <- function() {
  namespace <- nirs4all_require("dag_ml")
  factory <- get0("dagml_local_implementation_registry", envir = namespace,
                  inherits = FALSE)
  if (!is.function(factory))
    stop("installed dagml lacks dagml_local_implementation_registry", call. = FALSE)
  registry <- factory()
  required <- c("register_loss", "register_metric", "invoke_training_loss")
  if (!all(vapply(required, function(name) is.function(registry[[name]]), logical(1))))
    stop("installed dagml registry lacks required methods", call. = FALSE)
  registry
}
