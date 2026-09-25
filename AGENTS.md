# nirs4all R product

This repository owns the end-user R package. Keep numerical kernels in `nirs4all-methods` (`n4m`), data contracts and phase execution in `dag-ml`, and dataset readers in their upstream packages. Do not duplicate native algorithms here. A local R fit/predict facade may compose upstream functions, but report it separately from DAG-ML parity until the native phase path is integrated. Preserve reproducibility, explicit sample alignment, and train/validation separation. Do not publish the package while `nirs4all-core` also owns the public R package name `nirs4all`.
