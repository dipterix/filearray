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
