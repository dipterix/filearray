# 'S3' methods for 'FileArray'

These are 'S3' methods for 'FileArray'

## Usage

``` r
# S3 method for class 'FileArray'
`[`(
  x,
  i,
  ...,
  drop = TRUE,
  reshape = NULL,
  strict = TRUE,
  dimnames = TRUE,
  split_dim = 0
)

# S3 method for class 'FileArray'
x[i, ..., lazy = FALSE] <- value

# S3 method for class 'FileArray'
x[[i]]

# S3 method for class 'FileArray'
as.array(x, reshape = NULL, drop = FALSE, ...)

# S3 method for class 'FileArray'
dim(x)

# S3 method for class 'FileArray'
dimnames(x)

# S3 method for class 'FileArray'
dimnames(x) <- value

# S3 method for class 'FileArray'
length(x)

# S3 method for class 'FileArray'
max(x, na.rm = FALSE, ...)

# S3 method for class 'FileArray'
min(x, na.rm = FALSE, ...)

# S3 method for class 'FileArray'
range(x, na.rm = FALSE, ...)

# S3 method for class 'FileArray'
sum(x, na.rm = FALSE, ...)

# S3 method for class 'FileArray'
subset(x, ..., drop = FALSE, .env = parent.frame())
```

## Arguments

- x:

  a file array

- i, ...:

  index set, or passed to other methods

- drop:

  whether to drop dimensions; see topic
  [`Extract`](https://rdrr.io/r/base/Extract.html)

- reshape:

  a new dimension to set before returning subset results; default is
  `NULL` (use default dimensions)

- strict:

  whether to allow indices to exceed bound; currently only accept `TRUE`

- dimnames:

  whether to preserve [`dimnames`](https://rdrr.io/r/base/dimnames.html)

- split_dim:

  internally used; split dimension and calculate indices to manually
  speed up the subset; value ranged from 0 to size of dimension minus
  one.

- lazy:

  whether to lazy-evaluate the method, only works when assigning arrays
  with logical array index

- value:

  value to substitute or set

- na.rm:

  whether to remove `NA` values during the calculation

- .env:

  environment to evaluate formula when evaluating subset margin indices.

## Functions

- `[`: subset array

- `` `[`(FileArray) <- value ``: subset assign array

- `[[`: get element by index

- `as.array(FileArray)`: converts file array to native array in R

- `dim(FileArray)`: get dimensions

- `dimnames(FileArray)`: get dimension names

- `dimnames(FileArray) <- value`: set dimension names

- `length(FileArray)`: get array length

- `max(FileArray)`: get max value

- `min(FileArray)`: get min value

- `range(FileArray)`: get value range

- `sum(FileArray)`: get summation

- `subset(FileArray)`: get subset file array with formulae
