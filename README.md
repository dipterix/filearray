# File-Backed Array for Out-of-memory Computation


<!-- badges: start -->
[![R-check](https://github.com/dipterix/filearray/workflows/R-CMD-check/badge.svg)](https://github.com/dipterix/filearray/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/filearray)](https://CRAN.R-project.org/package=filearray)
[![Develop](https://dipterix.r-universe.dev/badges/filearray)](https://dipterix.r-universe.dev/ui#builds)
<!-- badges: end -->

Stores large arrays in files to avoid occupying large memories. Implemented with super fast gigabyte-level multi-threaded reading/writing via `OpenMP`. Supports multiple non-character data types (double, float, integer, complex, logical and raw).

![](https://raw.githubusercontent.com/dipterix/filearray/main/adhoc/readme-speed.png)

<small> *Speed comparisons with `lazyarray` (`zstd`-compressed out-of-memory array), and in-memory operation. `filearray` is uniformly faster than `lazyarray`. Random access has almost the same speed as the native in-memory operation. The speed test was performed on an `MacBook Air (M1, 2020)` with 8GB memory* </small>

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

Alternatively, you can compile from `Github` repository. This requires proper compilers (`rtools` on `windows`, or `xcode-select --install` on `osx`, or `build-essentials` on `linux`).

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

#### Collapse

Transform data, and collapse (calculate sum or mean) along margins.

```
a <- x$collapse(keep = 4, method = "mean", transform = "asis")

# equivalent to
b <- apply(x[], 4, mean)

a[1] - b[1]
```

Available `transform` for double/integer numbers are:

* `asis`: no transform
* `10log10`: `10 * log10(v)`
* `square`: `v * v` 
* `sqrt`: `sqrt(v)`

For complex numbers, `transform` is a little bit different:

* `asis`: no transform
* `10log10`: `10 * log10(|x|^2)` (power to decibel unit)
* `square`: `|x|^2` 
* `sqrt`: `|x|` (modulus)
* `normalize`: `x / |x|` (unit length)

## Notes

#### I. 'OpenMP' support and Number of Threads

If `OpenMP` is not detected, then only single thread will be used. This is more likely to happen on recent Apple's system because the native support for 'OpenMP' was dropped. To enable 'OpenMP', please read [this link](https://mac.r-project.org/openmp/). Find your system build and replace `OMP` accordingly, then run the following commands line-by-line.

```
OMP="openmp-11.0.1-darwin20-Release.tar.gz"
xcode-select --install
curl -O https://mac.r-project.org/openmp/$OMP
    sudo tar fvx $OMP -C /
```

This is a one-time configuration. After the configuration, please run 

```r
install.packages('filearray', type = 'source')
```


If `OpenMP` is detected, then the number of threads the maximum number of `CPU` cores on your machine, or `8`, depending on whichever is smaller. The maximum number of threads is limited because the performance bottle-neck often comes from hard drive speed, not the total processing cores. 

Simultaneous file read/write operation is recommended on modern `NVMe` solid-state drives or server `RAIDs`. On traditional `HDD`, it is recommended to use single thread.

#### II. Notes on precision

1. `complex` numbers: In native `R`, complex numbers are combination of two `double` numbers - real and imaginary (total 16 bytes). In `filearray`, complex numbers are coerced to two `float` numbers and store each number in 8 bytes. This conversion will gain performance speed, but lose precision at around 8 decimal place. For example, `1.0000001` will be store as `1`, or `123456789` will be stored as `123456792` (first 7 digits are accurate).

2. `float` type: Native R does not have float type. All numeric values are stored in double precision. Since float numbers use half of the space, float arrays can be faster when hard drive speed is the bottle-neck (see [performance comparisons](https://dipterix.org/filearray/articles/performance.html)). However coercing double to float comes at costs:
  a). float number has less precision 
  b). float number has smaller range ($3.4\times 10^{38}$) than double ($1.7\times 10^{308}$)
hence use with caution when data needs high precision or the max is super large.

3. `collapse` function: when data range is large (say `x[[1]]=1`, but `x[[2]]=10^20`), `collapse` method might lose precision. This is `double` only uses 8 bytes of memory space. When calculating summations, R internally uses `long double` to prevent precision loss, but current `filearray` implementation uses `double`, causing floating error around 16 decimal place. 


