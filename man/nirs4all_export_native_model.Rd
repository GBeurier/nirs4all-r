\name{nirs4all_export_native_model}
\alias{nirs4all_export_native_model}
\title{Export a fitted native pipeline as portable N4MM bytes}
\usage{nirs4all_export_native_model(object)}
\arguments{\item{object}{A fitted \code{nirs4all_fit()} result.}}
\value{Raw N4MM format-2 bytes.}
\description{Accepts the native SNV to Savitzky-Golay to SIMPLS profile
embedded in the n4m model. Export the unfitted recipe separately with
\code{nirs4all_export_pipeline()} when transferring it to another language.
These bytes are model state, not a full DAG-ML Archive V2/V3.}
