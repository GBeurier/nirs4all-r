# Emit a deterministic R-trained v6 JSON envelope and its held-out oracle.
# Usage: Rscript tests/helpers/trained_n4mp_v6_fixture.R envelope.json oracle.json
library(nirs4all)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("expected envelope and oracle paths")
X <- outer(seq_len(24L), seq_len(17L), function(s, b)
  sin(s * b / 9) + cos(s + b / 7) + s * b / 100)
colnames(X) <- paste0("wl", seq_len(ncol(X)))
y <- 1.3 + 0.7 * X[, 2L] - 0.4 * X[, 6L]
held <- X[c(2L, 8L, 17L), , drop = FALSE] + 0.031
pipeline <- nirs4all_pipeline(list(nirs4all_snv(), nirs4all_msc(),
  nirs4all_savgol(7L, 2L, 1L)), nirs4all_n4m_method("ridge_pls", 2L))
fitted <- nirs4all_fit(pipeline, X, y, preprocessing = "native_n4mp")
nirs4all_export_trained_pipeline(fitted, args[[1L]])
oracle <- list(train = lapply(seq_len(nrow(X)), function(i)
    unname(as.list(as.numeric(X[i, ])))),
  heldout = lapply(seq_len(nrow(held)), function(i)
    unname(as.list(as.numeric(held[i, ])))),
  feature_names = unname(as.list(colnames(X))),
  y = unname(as.list(y)),
  predictions = unname(as.list(as.numeric(predict(fitted, held)))))
writeLines(as.character(jsonlite::toJSON(oracle, auto_unbox = TRUE,
  digits = 17L)), args[[2L]], useBytes = TRUE)
