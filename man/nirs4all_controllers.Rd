\name{nirs4all_controller}
\alias{nirs4all_controller}
\alias{nirs4all_pls}
\alias{nirs4all_lm}
\alias{nirs4all_ranger}
\title{Define learner controllers}
\description{Provides an n4m PLS controller, a base-R linear-model controller, an optional ranger regression controller, and a custom-controller constructor. Only the n4m fitted model has a cross-language N4MM representation.}
\usage{
nirs4all_controller(fit, predict, name)
nirs4all_pls(n_components = 2L, algo = "pls_simpls",
             center_x = TRUE, scale_x = TRUE,
             center_y = TRUE, scale_y = TRUE)
nirs4all_lm()
nirs4all_ranger(num.trees = 500L, seed = 1L, ...)
}
\arguments{
\item{fit}{Function of transformed X and y returning fitted state.}
\item{predict}{Function of fitted state and transformed X returning numeric predictions.}
\item{name}{Non-empty controller label.}
\item{n_components}{Positive PLS component count.}
\item{algo}{n4m PLS solver name.}
\item{center_x,scale_x,center_y,scale_y}{Native n4m centering and scaling flags.}
\item{num.trees}{Positive forest size.}
\item{seed}{Non-negative ranger seed.}
\item{...}{Additional ranger arguments, excluding reserved fit arguments.}
}
\value{A learner controller accepted by \code{nirs4all_pipeline}.}
