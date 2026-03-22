# Set or get file array threads

Will enable/disable multi-threaded reading or writing at `C++` level.

## Usage

``` r
filearray_threads(n, ...)
```

## Arguments

- n:

  number of threads to set. If `n` is negative, then default to the
  number of cores that computer has.

- ...:

  internally used

## Value

An integer of current number of threads
