library(filearray)
set.seed(1)
file <- tempfile(); unlink(file, recursive = TRUE)
dim <- c(100, 100, 50, 100)
x <- filearray_create(file, dim, type = "complex")
xlen <- length(x)
y <- rnorm(xlen) + 1i * rnorm(xlen)

x$initialize_partition()

# converting 800MB to 400MB and write
system.time({
    x[] <- y
})
#   user  system elapsed 
#  0.268   0.770   0.368

y <- x[]

# Load 400MB, then convert to 800MB
system.time({
    x[]
})
#   user  system elapsed 
#  0.228   0.342   0.351
locs <- lapply(dim, function(d){
    sample(1:d, size = sample(1:d, size = 1))
})
microbenchmark::microbenchmark(
    filearray = {
        x[locs[[1]], locs[[2]], locs[[3]], locs[[4]]]
    },
    native = {
        y[locs[[1]], locs[[2]], locs[[3]], locs[[4]]]
    }, times = 10, unit = "ms"
)
# Unit: milliseconds
#       expr       min       lq      mean    median        uq      max neval
#  filearray  8.401187  8.48126  9.764138  8.759076  9.615115 16.98675    10
#     native 19.753554 19.82092 23.815748 20.394937 21.714994 53.37421    10

keep <- c(3, 4)
microbenchmark::microbenchmark(
    filearray = {
        x$collapse(keep = keep, method = "sum")
    },
    native = {
        apply(y, keep, sum)
    }, 
    dipsaus = {
        dipsaus::collapse(y, keep, average = FALSE)
    }, 
    times = 5, unit = "s"
)

# Unit: seconds
#       expr       min        lq      mean    median        uq       max neval
#  filearray 0.3544366 0.3599713 0.4557022 0.4204501 0.5650227 0.5786305     5
#     native 2.0568676 2.1961080 2.3898924 2.2768022 2.6102381 2.8094461     5
#    dipsaus 0.3548668 0.4553942 0.5307459 0.4588728 0.6577143 0.7268814     5


keep <- c(3, 2)
microbenchmark::microbenchmark(
    filearray = {
        x$collapse(keep = keep, method = "sum")
    },
    native = {
        apply(y, keep, sum)
    }, 
    dipsaus = {
        dipsaus::collapse(y, keep, average = FALSE)
    }, 
    times = 5, unit = "s"
)

# Unit: seconds
#       expr       min        lq      mean    median        uq       max neval
#  filearray 0.3574229 0.4281074 0.4975226 0.4827639 0.5061053 0.7132133     5
#     native 0.7374846 1.3993005 1.4811525 1.5285852 1.5995152 2.1408772     5
#    dipsaus 0.3303778 0.4295939 0.6026428 0.6484702 0.7453199 0.8594522     5




