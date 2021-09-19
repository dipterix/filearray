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
