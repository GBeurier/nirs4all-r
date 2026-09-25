if (requireNamespace("mlr3", quietly = TRUE) &&
    requireNamespace("rpart", quietly = TRUE)) {
  library(nirs4all)
  X <- outer(seq_len(18L), seq_len(4L),
             function(i, j) sin(i * j / 7) + i * j / 50)
  y <- 2 + 0.7 * X[, 2L] - 0.3 * X[, 4L]
  learner <- mlr3::lrn("regr.rpart", minsplit = 3L, cp = 0)
  pipeline <- nirs4all_pipeline(learner = nirs4all_mlr3(learner))
  fitted <- nirs4all_fit(pipeline, X[1:12, , drop = FALSE], y[1:12])
  stopifnot(is.null(learner$model))
  reference <- learner$clone(deep = TRUE)
  frame <- as.data.frame(X[1:12, , drop = FALSE])
  names(frame) <- paste0("x", seq_len(ncol(X)))
  frame$y <- y[1:12]
  reference$train(mlr3::TaskRegr$new("independent", frame, target = "y"))
  test_frame <- as.data.frame(X[13:18, , drop = FALSE])
  names(test_frame) <- paste0("x", seq_len(ncol(X)))
  expected <- reference$predict_newdata(test_frame)$response
  actual <- predict(fitted, X[13:18, , drop = FALSE])
  stopifnot(length(actual) == length(expected),
            max(abs(actual - expected)) < 1e-12)
  path <- tempfile(fileext = ".rds")
  nirs4all_save(fitted, path)
  stopifnot(max(abs(predict(nirs4all_load(path), X[13:18, , drop = FALSE]) -
                    expected)) < 1e-12)
  unlink(path)
  stopifnot(inherits(try(nirs4all_mlr3(reference), silent = TRUE), "try-error"))
}
