# Merge and bind homogeneous file arrays

The file arrays to be merged must be homogeneous: same data type,
partition size, and partition length

## Usage

``` r
filearray_bind(
  ...,
  .list = list(),
  filebase = tempfile(),
  symlink = FALSE,
  overwrite = FALSE,
  cache_ok = FALSE
)
```

## Arguments

- ..., .list:

  file array instances

- filebase:

  where to create merged array

- symlink:

  whether to use [`file.symlink`](https://rdrr.io/r/base/files.html); if
  true, then partition files will be symbolic-linked to the original
  arrays, otherwise the partition files will be copied over. If you want
  your data to be portable, do not use symbolic-links. The default value
  is `FALSE`

- overwrite:

  whether to overwrite when `filebase` already exists; default is false,
  which raises errors

- cache_ok:

  see 'Details', only used if `overwrite` is true.

## Value

A bound array in `'FileArray'` class.

## Details

The input arrays must share the same data type and partition size. The
dimension for each partition should also be the same. For example an
array `x1` has dimension \\100x20x30\\ with partition size `1`, then
each partition dimension is \\100x20x1\\, and there are `30` partitions.
`x1` can bind with another array of the same partition size. This means
if `x2` has dimension \\100x20x40\\ and each partition size is `1`, then
`x1` and `x2` can be merged.

If `filebase` exists and `overwrite` is `FALSE`, an error will always
raise. If `overwrite=TRUE` and `cache_ok=FALSE`, then the existing
`filebase` will be erased and any data stored within will be lost. If
both `overwrite` and `cache_ok` are `TRUE`, then , before erasing
`filebase`, the function validates the existing array header and compare
the header signatures. If the existing header signature is the same as
the array to be created, then the existing array will be returned. This
`cache_ok` could be extremely useful when binding large arrays with
`symlink=FALSE` as the cache might avoid moving files around. However,
`cache_ok` should be enabled with caution. This is because only the
header information will be compared, but the partition data will not be
compared. If the existing array was generated from an old versions of
the source arrays, but the data from the source arrays has been altered,
then the `cache_ok=TRUE` is rarely proper as the cache is outdated.

The `symlink` option should be used with extra caution. Creating
symbolic links is definitely faster than copying partition files.
However, since the partition files are simply linked to the original
partition files, changing to the input arrays will also affect the
merged arrays, and vice versa; see 'Examples'. Also for arrays created
from symbolic links, if the original arrays are deleted, while the
merged arrays will not be invalidated, the corresponding partitions will
no longer be accessible. Attempts to set deleted partitions will likely
result in failure. Therefore `symlink` should be set to true when
creating merged arrays are temporary for read-only purpose, and when
speed and disk space is in consideration. For extended reading, please
check [`files`](https://rdrr.io/r/base/files.html) for details.

## Examples

``` r
partition_size <- 1
type <- "double"
x1 <- filearray_create(
    tempfile(), c(2,2), type = type,
    partition_size = partition_size)
x1[] <- 1:4
x2 <- filearray_create(
    tempfile(), c(2,1), type = type,
    partition_size = partition_size)
x2[] <- 5:6

y1 <- filearray_bind(x1, x2, symlink = FALSE)
y2 <- filearray_bind(x1, x2)

# y1 copies partition files, and y2 simply creates links 
# if symlink is supported

y1[] - y2[]
#>      [,1] [,2] [,3]
#> [1,]    0    0    0
#> [2,]    0    0    0

# change x1
x1[1,1] <- NA

# y1 is not affected
y1[]
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]    2    4    6

# y2 changes 
y2[]
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]    2    4    6

```
