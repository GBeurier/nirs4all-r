\name{nirs4all_n4a}
\alias{nirs4all_n4a}
\alias{nirs4all_n4a_inspect}
\alias{nirs4all_n4a_produce}
\alias{nirs4all_n4a_predict}
\alias{nirs4all_n4a_retrain}
\alias{nirs4all_n4a_write}
\alias{nirs4all_n4a_read}
\title{Canonical V1 .n4a operations and owner-neutral exchange through the native Core CLI}
\usage{
nirs4all_n4a_inspect(path,
  core_cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive"),
  expected_sha256 = NULL, memory_budget_bytes = NULL)

nirs4all_n4a_produce(record_path, recipe, archive, source_id = "spectra",
  methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
  methods_library_sha256 = Sys.getenv("N4M_EXPECTED_LIBRARY_SHA256"),
  run_id = paste0("run:r:n4a:produce:", basename(tempfile())),
  container_id = paste0("r-n4a-", basename(tempfile())),
  embed_training_record = FALSE,
  core_cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive"),
  memory_budget_bytes = NULL)

nirs4all_n4a_predict(object, record_path = NULL, partition = NULL,
  methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
  methods_library_sha256 = Sys.getenv("N4M_EXPECTED_LIBRARY_SHA256"),
  source_id = NULL, entrypoint = NULL,
  request_id = paste0("req:r:n4a:predict:", basename(tempfile())),
  run_id = paste0("run:r:n4a:predict:", basename(tempfile())),
  core_cli = NULL, memory_budget_bytes = NULL)

nirs4all_n4a_retrain(object, record_path, archive,
  methods_library = Sys.getenv("N4M_LIBRARY_PATH", Sys.getenv("N4M_LIB_PATH")),
  methods_library_sha256 = Sys.getenv("N4M_EXPECTED_LIBRARY_SHA256"),
  run_id = paste0("run:r:n4a:retrain:", basename(tempfile())),
  container_id = paste0("r-n4a-", basename(tempfile())),
  embed_training_record = FALSE, core_cli = NULL, memory_budget_bytes = NULL)

nirs4all_n4a_write(manifest_path, members_path, destination, base = NULL,
  base_sha256 = NULL,
  core_cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive"),
  memory_budget_bytes = NULL)

nirs4all_n4a_read(path, directory, expected_sha256 = NULL, member = NULL,
  artifact = NULL,
  core_cli = Sys.getenv("NIRS4ALL_CORE_CLI", "nirs4all-core-archive"),
  memory_budget_bytes = NULL)
}
\arguments{
  \item{path}{Existing canonical V1 \code{.n4a} file.}
  \item{core_cli}{\code{nirs4all-core-archive} executable that provides the \code{n4a-*} commands. Objects remember the executable they were created with.}
  \item{memory_budget_bytes}{Optional allocation budget in bytes, forwarded to the CLI as the exact decimal text of \code{--memory-budget-bytes}; Core decides admission. \code{NULL} or \code{""} (the default) sends no flag and this package adds no cap, default or memory detection. Accepted: a nonnegative whole number up to \eqn{2^{53}-1}, or decimal digits (no sign or spaces; leading zeros are valid and forwarded as given) with a value up to \code{"18446744073709551615"}. A larger number may already have been rounded and is refused: pass such budgets as text. Malformed values are refused before any CLI is resolved.}
  \item{expected_sha256}{Optional lowercase SHA-256 of the archive bytes to verify while inspecting or reading.}
  \item{manifest_path}{Existing file holding the exact manifest JSON. Lists are refused so that no array is re-encoded.}
  \item{members_path}{Existing JSON file holding an array of \code{{path, file}} objects: the path of each member inside the container and the binary file with its exact bytes. Relative \code{file} values resolve against the directory containing this list.}
  \item{destination}{New container file to create; it is never overwritten.}
  \item{base}{Optional existing container to update. It is never modified; the update is a new container at \code{destination}. Members that \code{members_path} does not list come from \code{base}.}
  \item{base_sha256}{Independently known lowercase SHA-256 of \code{base}. Required with \code{base}.}
  \item{directory}{New directory to receive the extracted files; it must not exist and its parent must.}
  \item{member}{Optional path of one member inside the container. Mutually exclusive with \code{artifact}.}
  \item{artifact}{Optional artifact ID whose members are extracted. Mutually exclusive with \code{member}.}
  \item{record_path}{Path of an IO dataset JSON file. Datasets and lists are refused because re-encoding would change the record bytes. For prediction, \code{NULL} selects the training record embedded in the container.}
  \item{recipe}{Native recipe: an existing JSON file, JSON text, or a list with \code{steps} and \code{candidates}. The owner validates it.}
  \item{archive}{New file to create; it must not exist and its parent directory must.}
  \item{object}{Reference returned by \code{nirs4all_n4a_inspect}, \code{nirs4all_n4a_produce} or \code{nirs4all_n4a_retrain}.}
  \item{partition}{Dataset partition to predict; default \code{"predict"} for an external record and \code{"train"} for the embedded one.}
  \item{source_id}{Data source identifier for prediction; default \code{"spectra"} or the one stored in \code{object}. Not an argument of retrain: the owner takes it from the validated package.}
  \item{entrypoint}{Container entrypoint; default \code{"predict-external"} with a record and \code{"predict"} without.}
  \item{methods_library}{Exact native \code{libn4m} file. Required.}
  \item{methods_library_sha256}{Independently attested lowercase SHA-256 of that library. Required; there is no default hash.}
  \item{run_id, container_id, request_id}{Explicit native identifiers.}
  \item{embed_training_record}{If \code{TRUE}, the training record is sealed into the container so that it can be predicted in-sample.}
}
\value{
\code{nirs4all_n4a_write} and \code{nirs4all_n4a_read} return a
\code{nirs4all_n4a_exchange} list: \code{path} (written container) or
\code{directory}, \code{member_files} and \code{manifest_file}, the owner
\code{report} and the exact \code{native_json}.

\code{nirs4all_n4a_inspect}, \code{nirs4all_n4a_produce} and
\code{nirs4all_n4a_retrain} return a \code{nirs4all_n4a} reference holding the
path, the raw \code{archive_sha256} and the native report.
\code{nirs4all_n4a_predict} returns a \code{nirs4all_n4a_prediction} with
\code{sample_ids}, \code{target_names}, \code{target_units}, a numeric
\code{values} matrix (\code{rownames} = sample IDs, \code{colnames} = target
names), \code{source_id}, \code{partition}, \code{input_mode}, and the unchanged
owner \code{replay} and \code{receipt}.
}
\description{
\code{nirs4all_n4a_write} and \code{nirs4all_n4a_read} exchange arbitrary
owner-neutral content through the canonical container: data, recipes, results,
scores and opaque models of any host. See the Details section.

These functions drive the canonical V1 \code{.n4a} container (manifest schema
\code{nirs4all.n4a.container.v1}) through the \code{n4a-inspect},
\code{n4a-produce}, \code{n4a-predict} and \code{n4a-retrain} commands of
\code{nirs4all-core-archive}. They are separate from the native package
routes (\code{nirs4all_core_archive*}) and JSON Package routes and never fall back to
them. R only transports file paths and decodes JSON: the Core, DAG and Methods
owners decide which methods, targets and profiles are supported.
}
\details{
\code{nirs4all_n4a_inspect} reports readability only. No Methods runtime is
loaded and the \code{executability} field is always \code{"not_evaluated"}; a
readable container may still be refused by an operation.

\code{nirs4all_n4a_produce} trains once through the owner and writes its sealed
bytes. \code{nirs4all_n4a_predict} never refits. Its rows follow the physical
row order of the record, aligned exactly once by the owner-provided sample IDs;
a missing, repeated or extra ID is refused. Prediction on the embedded training
record is in-sample, not a held-out score.

\code{nirs4all_n4a_write} sends the manifest file and the member list to the
\code{n4a-write} command, which seals a container from those exact bytes.
With \code{base} and \code{base_sha256} it updates an immutable base: the base
is verified against the given hash, left untouched, and the result is a new
container. Nothing is overwritten. \code{nirs4all_n4a_read} extracts the bytes
of the whole container, one \code{member} or one \code{artifact} into a new
\code{directory} as \code{member-000000.bin}, \code{member-000001.bin}, ...
plus the exact \code{manifest.json}. Paths inside the container are never used
as operating-system paths; the owner report maps each numbered file to its
archive path. Both functions return the owner report unchanged in
\code{report} (decoded) and \code{native_json} (exact text), and do not rely on
any particular report key. \code{member_files} and \code{manifest_file} list
the extracted files. R reads no member, keeps no model and calls no Methods
code: bytes in, bytes out.

Reports are decoded with \code{jsonlite} using \code{bigint_as_char = TRUE}, so
integers beyond 53 bits are character strings in \code{report}; the exact
numbers are always in \code{native_json}. The optional
\code{memory_budget_bytes} is accepted by every \code{n4a-*} transport and only
forwarded; R measures nothing and enforces nothing.

Readable is not executable: storing, reading and updating need no Methods
runtime and no admission. A readable container, including one that holds an
opaque host model, may still be refused by any operation that would execute it,
and whether it can be executed is decided by its owner.

\code{nirs4all_n4a_retrain} calls the Core owner \code{retrain_n4a_native}: the
source ID and the validated stored recipe both come from the container, so
neither is an argument, and R runs no training. The source container is left
untouched and owner errors are signalled unchanged as
\code{nirs4all_n4a_refusal}.

Requirements: a \code{nirs4all-core-archive} that provides the \code{n4a-*}
commands (otherwise a \code{nirs4all_n4a_unavailable} condition is signalled),
and for produce, predict and retrain the real Methods library with its SHA-256.
Refusals are signalled as conditions of class \code{nirs4all_n4a_refusal} that
carry the native message in \code{detail}. The qualification test uses a
\code{standard_scale} plus \code{ridge} recipe with two ordered targets; that is
a profile witness, not universal support. Linux only; no Windows claim.
}
\examples{
\dontrun{
Sys.setenv(NIRS4ALL_CORE_CLI = "/path/to/nirs4all-core-archive")
model <- nirs4all_n4a_produce("train.json", "recipe.json", "model.n4a",
  methods_library = "/path/to/libn4m.so",
  methods_library_sha256 = "<attested sha256>", embed_training_record = TRUE)
nirs4all_n4a_inspect("model.n4a")$executability
pred <- nirs4all_n4a_predict(model, "future.json", partition = "predict",
  methods_library = "/path/to/libn4m.so",
  methods_library_sha256 = "<attested sha256>")
pred$values
}
}
