# Emit a deterministic R-trained v8 envelope (n4m role recipe) and its held-out oracle.
# Usage: Rscript tests/helpers/trained_roles_v8_fixture.R envelope.json oracle.json
library(nirs4all)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("expected envelope and oracle paths")
X <- outer(seq_len(30L), seq_len(18L), function(s, b)
  sin(s * b / 9) + cos(s + b / 7) + s * b / 100 + 2)
y <- 1.3 + 0.7 * X[, 2L] - 0.4 * X[, 6L]
held <- X[c(2L, 8L, 17L), , drop = FALSE] + 0.031
recipe <- list(pipeline = list(
  "n4m:preprocessing.scatter.msc",
  list(class = "n4m:filters.variance", params = list(top_k = 12L)),
  list(class = "n4m:models.regularized.robust_pls", params = list(n_components = 3L))))
fitted <- nirs4all_fit_role_recipe(recipe, X, y)
nirs4all_export_trained_pipeline(fitted, args[[1L]])
writeLines(as.character(jsonlite::toJSON(list(
  x_train = X, y_train = y, x_test = held,
  predict = nirs4all_predict(fitted, held)), digits = NA)), args[[2L]])
