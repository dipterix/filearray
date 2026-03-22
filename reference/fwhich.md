# A generic function of `which` that is `'FileArray'` compatible

A generic function of `which` that is `'FileArray'` compatible

## Usage

``` r
fwhich(x, val, arr.ind = FALSE, ret.values = FALSE, ...)

# Default S3 method
fwhich(x, val, arr.ind = FALSE, ret.values = FALSE, ...)

# S3 method for class 'FileArray'
fwhich(x, val, arr.ind = FALSE, ret.values = FALSE, ...)
```

## Arguments

- x:

  any R vector, matrix, array or file-array

- val:

  values to find, or a function taking one argument (a slice of data
  vector) and returns either logical vector with the same length as the
  slice or index of the slice; see 'Examples'

- arr.ind:

  logical; should array indices be returned when `x` is an array?

- ret.values:

  whether to return the values of corresponding indices as an
  attributes; default is false

- ...:

  passed to `val` if `val` is a function

## Value

The indices of `x` elements that are listed in `val`.

## Examples

``` r

# ---- Default case ------------------------------------
x <- array(1:27 + 2, rep(3,3))

# find index of `x` equal to either 4 or 5
fwhich(x, c(4,5))
#> [1] 2 3
res <- fwhich(x, c(4,5), ret.values = TRUE)
res
#> [1] 2 3
#> attr(,"values")
#> [1] 4 5
attr(res, "values")
#> [1] 4 5

# ---- file-array case --------------------------------
arr <- filearray_create(tempfile(), dim(x))
arr[] <- x
fwhich(arr, c(4,5))
#> [1] 2 3
fwhich(arr, c(4,5), arr.ind = TRUE, ret.values = TRUE)
#>      [,1] [,2] [,3]
#> [1,]    2    1    1
#> [2,]    3    1    1
#> attr(,"values")
#> [1] 4 5

arr[2:3, 1, 1]
#> [1] 4 5

# Clean up this example
arr$delete()

# ---- `val` is a function ----------------------------
x <- as_filearray(c(sample(15), 15), dimension = c(4,4))

ret <- fwhich(x, val = which.max, 
              ret.values = TRUE, arr.ind = FALSE)

# ret is the index
ret == which.max(x[])
#> [1] TRUE

# attr(ret, "values") is the max value
max(x[]) == attr(ret, "values")
#> [1] TRUE

# customize `val`
fwhich(x, ret.values = TRUE, arr.ind = FALSE,
       val = function( slice ) {
           slice > 10 # or which(slice > 10)
       })
#> [1]  3  4  8 12 13 16
#> attr(,"values")
#> [1] 13 15 12 14 11 15

```
