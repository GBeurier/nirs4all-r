library(nirs4all)
upstreams <- nirs4all_upstreams()
stopifnot(identical(upstreams$key,
  c("dag_ml", "dag_ml_data", "formats", "io", "datasets", "methods")),
  identical(upstreams$package[[6L]], "n4m"),
  isTRUE(upstreams$available[[6L]]),
  identical(methods(), asNamespace("n4m")),
  inherits(try(nirs4all_require("unknown"), silent = TRUE), "try-error"))
if (requireNamespace("dagml", quietly = TRUE)) {
  registry <- nirs4all_local_implementation_registry()
  stopifnot(is.function(registry$register_loss),
            is.function(registry$invoke_training_loss))
}
