# Create or load existing file arrays

Create or load existing file arrays

## Usage

``` r
as_filearray(x, ...)

as_filearrayproxy(x, ...)

filearray_create(
  filebase,
  dimension,
  type = c("double", "float", "integer", "logical", "raw", "complex"),
  partition_size = NA,
  initialize = FALSE,
  ...
)

filearray_load(filebase, mode = c("readwrite", "readonly"))

filearray_checkload(
  filebase,
  mode = c("readonly", "readwrite"),
  ...,
  symlink_ok = TRUE
)

filearray_load_or_create(
  filebase,
  dimension,
  on_missing = NULL,
  type = NA,
  ...,
  mode = c("readonly", "readwrite"),
  symlink_ok = TRUE,
  initialize = FALSE,
  partition_size = NA,
  verbose = FALSE,
  auto_set_headers = TRUE
)
```

## Arguments

- x:

  R object such as array, file array proxy, or character that can be
  transformed into file array

- ...:

  additional headers to check used by `filearray_checkload` (see
  'Details'). This argument is ignored by `filearray_create`, reserved
  for future compatibility.

- filebase:

  a directory path to store arrays in the local file system. When
  creating an array, the path must not exist.

- dimension:

  dimension of the array, at least length of 2

- type:

  storage type of the array; default is `'double'`. Other options
  include `'integer'`, `'logical'`, and `'raw'`.

- partition_size:

  positive partition size for the last margin, or `NA` to automatically
  guess; see 'Details'.

- initialize:

  whether to initialize partition files; default is false for
  performance considerations. However, if the array is dense, it is
  recommended to set to true

- mode:

  whether allows writing to the file; choices are `'readwrite'` and
  `'readonly'`.

- symlink_ok:

  whether arrays with symbolic-link partitions can pass the test; this
  is usually used on bound arrays with symbolic-links; see
  [`filearray_bind`](https://dipterix.org/filearray/reference/filearray_bind.md);

- on_missing:

  function to handle file array (such as initialization) when a new
  array is created; must take only one argument, the array object

- verbose:

  whether to print out some debug messages

- auto_set_headers:

  whether to automatically set headers if array is missing or to be
  created; default is true

## Value

A
[`FileArray-class`](https://dipterix.org/filearray/reference/FileArray-class.md)
instance.

## Details

The file arrays partition out-of-memory array objects and store them
separately in local file systems. Since R stores matrices/arrays in
column-major style, file array uses the slowest margin (the last margin)
to slice the partitions. This helps to align the elements within the
files with the corresponding memory order. An array with dimension
`100x200x300x400` has 4 margins. The length of the last margin is 400,
which is also the maximum number of potential partitions. The number of
partitions are determined by the last margin size divided by
`partition_size`. For example, if the partition size is 1, then there
will be 400 partitions. If the partition size if 3, there will be 134
partitions. The default partition sizes are determined internally
following these priorities:

- 1\. :

  the file size of each partition does not exceed `1GB`

- 2\. :

  the number of partitions do not exceed 100

These two rules are not hard requirements. The goal is to reduce the
numbers of partitions as much as possible.

The arguments `...` in `filearray_checkload` should be named arguments
that provide additional checks for the header information. The check
will fail if at least one header is not identical. For example, if an
array contains header key-signature pair, one can use
`filearray_checkload(..., key = signature)` to validate the signature.
Note the comparison will be rigid, meaning the storage type of the
headers will be considered as well. If the signature stored in the array
is an integer while provided is a double, then the check will result in
failure.

## Author

Zhengjia Wang

## Examples

``` r

# Prepare
library(filearray)
filebase <- tempfile()
if(file.exists(filebase)){ unlink(filebase, TRUE) }

# create array
x <- filearray_create(filebase, dimension = c(200, 30, 8))
print(x)
#> Reference class object of class "FileArray"
#> Mode: readwrite 
#> Dimension: 200x30x8 
#> Partition count: 8 
#> Partition size: 1 
#> Storage type: double (internal size: 8)
#> Location: /tmp/Rtmp7JfRFj/file1c9a42891882 

# Assign values
x[] <- rnorm(48000)

# Subset
x[1,2,]
#> [1] -0.42938009  0.44742147  1.05363668 -0.97346749 -0.09327726 -1.14067120
#> [7]  1.45111737  0.47259544

# load existing array
filearray_load(filebase)
#> Reference class object of class "FileArray"
#> Mode: readwrite 
#> Dimension: 200x30x8 
#> Partition count: 8 
#> Partition size: 1 
#> Storage type: double (internal size: 8)
#> Location: /tmp/Rtmp7JfRFj/file1c9a42891882 

x$set_header("signature", "tom")
filearray_checkload(filebase, signature = "tom")
#> Reference class object of class "FileArray"
#> Mode: readonly 
#> Dimension: 200x30x8 
#> Partition count: 8 
#> Partition size: 1 
#> Storage type: double (internal size: 8)
#> Location: /tmp/Rtmp7JfRFj/file1c9a42891882 

if (FALSE) { # \dontrun{
# Trying to load with wrong signature
filearray_checkload(filebase, signature = "jerry")
} # }


# check-load, and create a new array if fail
x <- filearray_load_or_create(
    filebase = filebase, dimension = c(200, 30, 8),
    verbose = FALSE, signature = "henry"
)
x$get_header("signature")
#> [1] "henry"

# check-load with initialization
x <- filearray_load_or_create(
    filebase = filebase,
    dimension = c(3, 4, 5),
    verbose = FALSE, mode = "readonly",
    on_missing = function(array) {
        array[] <- seq_len(60)
    }
)

x[1:3,1,1]
#> [1] 1 2 3

# Clean up
unlink(filebase, recursive = TRUE)
```
