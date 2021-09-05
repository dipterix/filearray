# Filematrix: File-Backed Matrix Class with Convenient Read and Write Access

Interface for working with large matrices stored in files,
not in computer memory. Supports multiple non-character
data types (double, integer, logical and raw) of
various sizes (e.g. 8 and 4 byte real values).
Access to parts of the matrix is done by indexing, 
exactly as with usual R matrices.
Supports very large matrices.
Tested on multi-terabyte matrices.
Allows for more than 2^32 rows or columns.
Allows for quick addition of extra columns to a filematrix.
Cross-platform as the package has R code only.

## Installation

### Install CRAN Version

To install the
[CRAN version](https://CRAN.R-project.org/package=filematrix)
of `filematrix`, run

```
install.packages("filematrix")
```

### Install GitHub Version

To install `filematrix` directly from GitHub, run

```
if(!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")
devtools::install_github("andreyshabalin/filematrix")
```
