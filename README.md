# FileArray: File-Backed Array for Out-of-memory Computation

Stores large arrays in files to avoid occupying large memories. Implemented with super fast gigabyte-level multi-threaded reading/writing via 'OpenMP'. Supports multiple non-character data types (double, integer, logical and raw).

[CRAN](https://CRAN.R-project.org/package=filearray) | [Github](https://github.com/dipterix/filearray)

## Installation

```r
install.packages("filearray")
```

### Install Develop Version

```r
options(repos = c(
    dipterix = 'https://dipterix.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))

# Install some packages
install.packages('dipsaus')
```

## Basic Usage

```r
# -------------- Create/Load -------------- 
library(filearray)
file <- tempfile()
x <- filearray_create(file, c(100, 100, 100, 100))

# load existing
x <- filearray_load(file)

# -------------- Subset/Assign -------------- 
x[,,,1] <- rnorm(1e6)
x[1:10,1,1,1]

# -------------- Generics -------------- 
dim(x)
max(x, na.rm = TRUE)

# -------------- Map-Reduce -------------- 
# Identical to sum(x, na.rm = TRUE)
mapreduce(x, 
          map = \(data){ sum(data, na.rm = TRUE) }, 
          reduce = \(mapped){ do.call(sum, mapped) })
```
