# filearray (development version)

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
