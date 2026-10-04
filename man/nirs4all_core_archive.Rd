\name{nirs4all_core_archive}
\alias{nirs4all_core_archive}
\alias{nirs4all_core_archive_predict}
\alias{nirs4all_core_archive_predict_methods}
\alias{nirs4all_core_multimodal_adapter}
\title{Replay a Core ZIP archive through native DAG and Methods processes}
\usage{
nirs4all_core_archive(path, core_cli = Sys.which("nirs4all-core-archive"))

nirs4all_core_archive_predict(archive, request, data_envelopes, adapter,
  trusted_controllers, dag_cli = Sys.which("dag-ml-cli"),
  workdir = tempfile("nirs4all-core-replay-"),
  outcome_id = "outcome:nirs4all-r.core.replay",
  run_id = "run:nirs4all-r.core.replay", process_timeout_ms = 0)

nirs4all_core_archive_predict_methods(archive, request, data_envelopes,
  methods_inputs, methods_library, methods_library_sha256,
  workdir = tempfile("nirs4all-core-methods-"),
  outcome_id = "outcome:nirs4all-r.core.methods",
  run_id = "run:nirs4all-r.core.methods")

nirs4all_core_multimodal_adapter(sources, operators, node_params, source_ids,
  controller_manifest, trusted_manifest, target_names = "y",
  workdir = tempfile("nirs4all-core-r-adapter-"))
}
\arguments{
  \item{path}{A Core Archive V2/V3 \code{.n4a} file.}
  \item{core_cli}{Installed native Core archive executable.}
  \item{archive}{Validated reference returned by \code{nirs4all_core_archive}.}
  \item{request}{Signed native TrainingReplayRequest list or exact JSON file.}
  \item{data_envelopes}{Native map of target-free prediction envelopes, as a list or exact JSON file.}
  \item{adapter}{Executable persistent R process adapter, or an adapter preparation result.}
  \item{trusted_controllers}{Independently installed controller manifests, as a list or exact JSON file.}
  \item{dag_cli}{Installed native DAG-ML executable.}
  \item{workdir}{New or empty directory for contracts and lifecycle/output logs.}
  \item{outcome_id, run_id}{Explicit native replay identifiers.}
  \item{process_timeout_ms}{Native worker timeout; zero is unbounded.}
  \item{methods_inputs}{Native map of explicit ordered sample IDs, numerical matrix x and target names. Target values y are refused.}
  \item{methods_library}{Exact native libn4m file for callback-free replay.}
  \item{methods_library_sha256}{Independently attested lowercase SHA-256 of the Methods library.}
  \item{sources}{Four named raw source declarations: nir, image, series, metadata.}
  \item{operators, node_params}{Current native operator and effective parameter maps.}
  \item{source_ids}{Explicit ordered native source identifiers.}
  \item{controller_manifest}{Expected signed producer manifest.}
  \item{trusted_manifest}{Independently installed current manifest for that producer.}
  \item{target_names}{One ordered target name for the closed raw native profile.}
}
\description{
Core owns ZIP limits, inventory closure and raw hashes. DAG-ML owns package
validation, signed replay scheduling and portable-artifact hydration/release.
The R adapter passes current buffers to installed Methods numerical operations.
No Python interpreter, R ZIP parser or R machine-learning coordinator is used.
}
\details{
The raw multimodal adapter reuses the closed U07 scaler, tensor-PCA,
mixed-column and Ridge profile with fixed declared shapes. It preserves the
original native Python/R/WASM/Octave producer owner. Fitting, REFIT, HPO and
target access are refused. Other independently trusted, installed native
process adapters can be supplied when they implement the archive's profile.

Requests and current data envelopes are native contracts supplied explicitly;
they are not constructed by reading or trusting the archive. An archive object
retains its raw SHA and prediction revalidates the original file. This POSIX
process surface requires the Core and DAG native executables and installed n4m.
It does not accept historical Python joblib bundle archives as Core ZIPs.

\code{nirs4all_core_archive_predict_methods} is the direct callback-free route
for supported N4MM, N4ME and RolePipeline packages produced by native Methods
controllers. It calls Core's existing V2/V3 replay, which delegates package
validation and scheduling to DAG-ML and numerical state import/prediction to
Methods. The explicit library hash pins an exact private loaded snapshot and
checks the Methods ABI. The raw U07 controller has a distinct identity and uses
the process route; it is not substituted for a native Methods controller.
}
\value{
Archive opening returns a validated inventory reference. Prediction returns the
native replay outcome and workdir. Adapter preparation returns its executable,
independent manifests, workdir and lifecycle log path.
}
