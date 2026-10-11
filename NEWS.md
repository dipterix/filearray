# filearray 0.2.4

* Partition files are written to a temporary file and then renamed into place, so other processes loading the same array no longer fail with "File size too small" while a partition is being created or filled
* `filearray_load` only treats files named `<n>.farr` as partitions
* Writes lock the partitions they change, so several processes (for example `parallel`, `callr` or `mirai` workers, or several applications) can write to one array without losing each other's data: sub-assignment (`x[...] <- value`), `fill_partition`, `initialize_partition`, `set_partition`, `fmap`, `fmap_element_wise` and `filearray_bind` wait while another process holds one of their partitions. Reading takes no locks, except that functions that must first create missing partitions (such as `max` or `sum`) lock the partitions they create
* Added `lock_partition(parts, timeout)` and `unlock_partition(parts)` methods to hold partitions over several operations (for example to check whether a partition has been computed and write it only if not); `lock_partition` waits and retries until the partitions are free or `timeout` seconds have passed
* Locks are byte-range locks on an empty file `.filearray.lock` in the array directory, released by the operating system when a process exits. `options(filearray.lock = FALSE)` (environment variable `FILEARRAY_LOCK`) turns off automatic locking; `options(filearray.lock.timeout = <seconds>)` (`FILEARRAY_LOCK_TIMEOUT`) limits how long writes wait, by default without limit
* A missing partition is created only if no other process has created it in the meantime, and every partition write uses its own temporary file, so concurrent writers no longer replace each other's partitions or share a temporary file
* `fmap` stops when the mapping function raises an error or is interrupted, instead of warning "cannot finish map" and returning a partly written result
* `delete()` releases the locks the session holds on the array before removing it
* Added `with_filelock(expr, lockfile, timeout, on_unsupported)` to evaluate code while holding an exclusive lock on any lock file, so that only one R session at a time runs it; other sessions wait. It is not tied to file arrays, so other packages can use it. `on_unsupported` chooses what happens when the lock cannot be taken: warn (default), stop, or ignore
* Partition locks never make a write fail when the lock itself cannot be taken: any error while locking (for example a stale network file handle or too many open files, not only file systems without lock support) now warns once and the write proceeds, and an invalid `filearray.lock.timeout` option or `FILEARRAY_LOCK_TIMEOUT` value warns once and is ignored instead of stopping every write
* Fixed `mapreduce` (and so `sum`, `max`, `min`, `range` and `fwhich`) reading only the first slice of every partition after the first when `partition_size` is greater than 1, which gave wrong results and wrong `fwhich` indices
* `fmap2` reports the error raised by the mapping function instead of "Unknown error."

# filearray 0.2.3

* `as_filearray` takes `RAVE` file array proxy

# filearray 0.2.2

* Fixed out-of-bound indexing leading to segfault along certain margins
* Fixed `ASAN` issue that may cause run-time error: signed integer overflow: 9223372036854775807 + 1 cannot be represented in type `int64_t` (`load.cpp:636:51`)

# filearray 0.2.1

* Fixed incorrect `NA` propagation in complex array `collapse`: `NaN == NaN` is always `FALSE` per IEEE 754, so `NA` detection now uses `ISNAN()` instead of `== NA_REAL`. Also added a missing "result already `NA`" short-circuit guard matching the non-complex path.
* Fixed `x[]` failure on `R-devel` (`>= 4.6.0`): replaced `tryCatch({ ...elt(1) })` in `fa_subset1` with `check_missing_dots()`, which correctly detects a missing first argument without relying on the error-throwing behavior of `...elt()` that changed in `R-devel`.
* Replaced deprecated `Rf_findVarInFrame` (removed in R 4.5.0) with a compatibility wrapper `farr_findVarInFrame` that uses `R_existsVarInFrame` + `R_getVarEx` on R >= 4.5.0 and falls back to `Rf_findVarInFrame` on older R.
* Fixed multiple unprotected `SEXP` variables flagged by `rchk`: `result` in `test_farr_findVarInFrame_`, `dots` in `check_missing_dots`, and `dims` in `dropDimension`.

# filearray 0.2.0

* Additional `audo_set_headers` to function `filearray_load_or_create` to avoid automatically setting headers, with default being `TRUE` for compatibility concerns

# filearray 0.1.9

* Fixed `CRAN` check issue brought by `testthat` update, which requires `bit64` when checking `int64` arrays. Added `bit64` to `Suggests` field to mitigate this check error

# filearray 0.1.8

* Fixed a bug where `C++` errors are not correctly thrown due to a recent `Rcpp` update. This bug may cause erroneous code to execute ignoring try-catch statement and imbalanced protecting stacks (`fwhich`, `mapreduce`)

# filearray 0.1.7

* Removed non-API R C interface
* Allows `subsetAssign` to assign when value length is 1 (thanks `@talegari` issue `#9`)
* Allows more flexible `fwhich` function with functional `val` input and options to include corresponding values as part of return (thanks `@talegari` issue `#10`)
* Fixed `mapreduce` bug when partition length is way larger than the buffer size, causing buffer to stop updating
* Fixed other `C++` issues, such as unused buffer allocation in subset-assign, and `min_` offset in `sub_vec_range`. These issues are not considered bugs since they don't affect the results, but the performance is slightly increased after fix.
* Replaced `dipsaus` with more updated `ravetools` in the vignette performance profiling
* Updated examples to get rid of warning messages and out-dated arguments.

# filearray 0.1.6

* Removed `c++11` from system requirement and `Makevars`
* Used `TinyThreads` instead of `OpenMP` to get parallel working on `OSX`
* Added `as_filearray` method, and support optional `float`
* Added array proxy class, allowing arrays to lazy-evaluate simple operators
* Allows user-defined temporary file array path
* Fixed `fmap` issues, using better guesses for default `.input_size`
* Fixed a memory bug caused when partition margin has elements greater than 1 and when `FARR_subset_sequential` is used
* Used `fastmap` to avoid environment look-up
* `fa_eval_ops` allows dimension names
* Larger default buffer size to allow `2^20` in single partition
* Disabled single indexing
* Fixed `endian` issue on `big-endian` platforms

# filearray 0.1.5

* Fixed a bug when trying to read array data sequentially. The bug is caused by buffer size being greater than the array length, making in a pointer that controls the partition number exceed the end of vector, resulting in undefined behavior. The functions affected are: `fmap`, `fmap2`. The bug has been fixed and passed `valgrind` memory check.

# filearray 0.1.4

* Fixed a bug when allocated memory is one byte short than requested. The bug would crash R when triggered in certain cases.
* Removed limit to the maximum number of partitions when writing. The previous implementation creates and opens related file descriptors all at once before writing. This setup will raise errors when the number of connections reach to certain limit, often defined by the operating systems. This update only opens the connection on demand. The performance might be impacted when writing to disk, but in return, the program will be more robust
* Fixed `subset` function environment not resolved correctly when using formula
* Added `filearray_load_or_create` as an alternative to `filearray_checkload` by automatically replace existing obsolete array files if the headers, dimensions, or data types don't match. Also `on_missing` argument is provided to allow array initialization if new array is created.

# filearray 0.1.3

* Automatically detect whether symbolic-link works and show warnings
* Warnings can be suppressed
* Allow extra headers to be set in `meta` file
* Added header signature method
* Fixed symbolic-link issues on `Windows` when partition sizes are 0
* Added check-load function `filearray_checkload` to validate header
* Fixed collapse method when `dimnames` are set
* Fixed an unprotected variable in `C++` code
* `filearray_bind` can use cache if the header signatures agree
* `filearray_bind` can choose to force overwrite
* Added package `digest` to `Imports`
* Fixed a typo and several small bugs


# filearray 0.1.2

* Removed `flush` in saving data to let system decide when to flush to hard drive
* Allowed array to expand along the partition margin
* Fixed dimension name getting dropped under certain situations
* Use 2 cores by default when `R CMD check` is detected

# filearray 0.1.1

* Added `OpenMP` flag in the `MakeVars`
* Fixed critical bugs that could cause `segfaults`
* Can store `complex` and `float` data types
* Re-implemented read/write functions to use memory map
* Allowed `dimnames` to be set
* Added generics `subset` to subset using `dimnames`
* Added vignette to compare performance
* Added speed comparisons in `README.md`
* Added `collapse` to calculate marginal summation with little memory overhead
* Added `fmap`, `fmap2` to apply functions to one or multiple file arrays with little memory overhead (also very fast)
* Fixed 'unprotected' issues warned by `rchk`

# filearray 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Initial implementation
