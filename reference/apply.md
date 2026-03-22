# Apply functions over file array margins (extended)

Apply functions over file array margins (extended)

## Usage

``` r
apply(X, MARGIN, FUN, ..., simplify = TRUE)

# S4 method for class 'FileArray'
apply(X, MARGIN, FUN, ..., simplify = TRUE)

# S4 method for class 'FileArrayProxy'
apply(X, MARGIN, FUN, ..., simplify = TRUE)
```

## Arguments

- X:

  a file array

- MARGIN:

  scalar giving the subscripts which the function will be applied over.
  Current implementation only allows margin size to be one

- FUN:

  the function to be applied

- ...:

  optional arguments to `FUN`

- simplify:

  a logical indicating whether results should be simplified if possible

## Value

See Section 'Value' in [`apply`](https://rdrr.io/r/base/apply.html);
