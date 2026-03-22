# A map-reduce method to iterate blocks of file-array data with little memory usage

A map-reduce method to iterate blocks of file-array data with little
memory usage

## Usage

``` r
mapreduce(x, map, reduce, ...)

# S4 method for class 'FileArray,ANY,function'
mapreduce(x, map, reduce, buffer_size = NA, ...)

# S4 method for class 'FileArray,ANY,NULL'
mapreduce(x, map, reduce, buffer_size = NA, ...)

# S4 method for class 'FileArray,ANY,missing'
mapreduce(x, map, reduce, buffer_size = NA, ...)
```

## Arguments

- x:

  a file array object

- map:

  mapping function that receives 3 arguments; see 'Details'

- reduce:

  `NULL`, or a function that takes a list as input

- ...:

  passed to other methods

- buffer_size:

  control how we split the array; see 'Details'

## Value

If `reduce` is `NULL`, return mapped results, otherwise return reduced
results from `reduce` function

## Details

When handling out-of-memory arrays, it is recommended to load a block of
array at a time and execute on block level. See
[`apply`](https://dipterix.org/filearray/reference/apply.md) for a
implementation. When an array is too large, and when there are too many
blocks, this operation will become very slow if computer memory is low.
This is because the R will perform garbage collection frequently.
Implemented in `C++`, `mapreduce` creates a buffer to store the block
data. By reusing the memory over and over again, it is possible to
iterate through the array with minimal garbage collections. Many
statistics, including `min`, `max`, `sum`, `mean`, ... These statistics
can be calculated in this way efficiently.

The function `map` contains three arguments: `data` (mandate), `size`
(optional), and `first_index` (optional). The `data` is the buffer,
whose length is consistent across iterations. `size` indicates the
effective size of the buffer. If the partition size is not divisible by
the buffer size, only first `size` elements of the data are from array,
and the rest elements will be `NA`. This situation could only occurs
when `buffer_size` is manually specified. By default, all of `data`
should belong to arrays. The last argument `first_index` is the index of
the first element `data[1]` in the whole array. It is useful when
positional data is needed.

The buffer size, specified by `buffer_size` is an additional optional
argument in `...`. Its default is `NA`, and will be calculated
automatically. If manually specified, a large buffer size would be
desired to speed up the calculation. The default buffer size will not
exceed \\nThreads x 2MB\\, where `nThreads` is the number of threads set
by
[`filearray_threads`](https://dipterix.org/filearray/reference/filearray_threads.md).
When partition length cannot be divided by the buffer size, instead of
trimming the buffer, `NA`s will be filled to the buffer, passed to `map`
function; see previous paragraph for treatments.

The function `mapreduce` ignores the missing partitions. That means if a
partition is missing, its data will not be read nor passed to `map`
function. Please run `x$initialize_partition()` to make sure partition
files exist.

## Examples

``` r

x <- filearray_create(tempfile(), c(100, 100, 10))
x[] <- rnorm(1e5)

## calculate summation
# identical to sum(x[]), but is more feasible in large cases

mapreduce(x, map = function(data, size){
    # make sure `data` is all from array
    if(length(data) != size){
        data <- data[1:size]
    }
    sum(data)
}, reduce = function(mapped_list){
    do.call(sum, mapped_list)
})
#> [1] -55.70441


## Find elements are less than -3
positions <- mapreduce(
    x,
    map = function(data, size, first_index) {
        if (length(data) != size) {
            data <- data[1:size]
        }
        which(data < -3) + (first_index - 1)
    },
    reduce = function(mapped_list) {
        do.call(c, mapped_list)
    }
)

if(length(positions)){
    x[[positions[1]]]
}
#> [1] -3.007064

```
