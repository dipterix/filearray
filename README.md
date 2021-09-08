# File-Backed Array for Out-of-memory Computation


<!-- badges: start -->
[![R-check](https://github.com/dipterix/filearray/workflows/R-CMD-check/badge.svg)](https://github.com/dipterix/filearray/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/filearray)](https://CRAN.R-project.org/package=filearray)
[![Develop](https://dipterix.r-universe.dev/badges/filearray)](https://dipterix.r-universe.dev/ui#builds)
<!-- badges: end -->

Stores large arrays in files to avoid occupying large memories. Implemented with super fast gigabyte-level multi-threaded reading/writing via `OpenMP`. Supports multiple non-character data types (double, integer, complex, logical and raw).

## Installation

```r
install.packages("filearray")
```

### Install Develop Version

The internal functions are written in `C++`. To avoid compiling the packages, you can install from my personal repository. It's automatically updated every hour. Currently available on `Windows` and `osx (Intel chip)` only.

```r
options(repos = c(
    dipterix = 'https://dipterix.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))

# Install some packages
install.packages('dipsaus')
```

Alternatively, you can compile from Github repository. This requires proper compilers (`rtools` on `windows`, or `xcode-select --install` on `osx`, or `build-essentials` on `linux`).

```r
# install.packages("remotes")
remotes::install_github("dipterix/filearray")
```

## Basic Usage

#### Create/load file array

```r
library(filearray)
file <- tempfile()
x <- filearray_create(file, c(100, 100, 100, 100))

# load existing
x <- filearray_load(file)
```

See more: `help("filearray")`

#### Assign & subset array

```r
x[,,,1] <- rnorm(1e6)
x[1:10,1,1,1]
```

#### Generics

```r
typeof(x)
max(x, na.rm = TRUE)
apply(x, 3, min, na.rm = TRUE)

val = x[1,1,5,1]
fwhich(x, val, arr.ind = TRUE)
```

See more: `help("S3-filearray")`, `help("fwhich")`

#### Map-reduce

Process segments of array and reduce to save memories.

```
# Identical to sum(x, na.rm = TRUE)
mapreduce(x, 
          map = \(data){ sum(data, na.rm = TRUE) }, 
          reduce = \(mapped){ do.call(sum, mapped) })
```

See more: `help("mapreduce")`
