as_int64 <- function(x){
    realToUint64(x, -2L^30, 2L^30, 0L)
}

test_that("C++: Utils", {
    
    misdot <- function(...){
        check_missing_dots(environment())
    }
    expect_equal(misdot(,,1), c(TRUE,TRUE,FALSE))
    expect_equal(misdot(,i=1,1), c(TRUE,FALSE,FALSE))
    expect_equal(misdot(), logical(0))
    
    # min buffer size is 16, otherwise round up to power of 2
    expect_equal(set_buffer_size(16), 16L)
    expect_equal(set_buffer_size(2), 16L)
    expect_equal(set_buffer_size(55.4), 64L)
    expect_error(set_buffer_size(0))
    expect_error(set_buffer_size(-1))
    expect_error(set_buffer_size(NA))
    expect_warning(expect_error(set_buffer_size(2^31)))
    
    
    expect_equal(kinda_sorted(as_int64(sample(1:10)), 1, 10), 1L)
    expect_equal(kinda_sorted(as_int64(c(sample(1:10),1)), 1, 10), 1L)
    expect_equal(kinda_sorted(as_int64(c(sample(1:10),1)), 1, 9), 0L)
    
    expect_equal(locationList(list(1:10), 10, 1)[[1]], as_int64(1:10))
    expect_error(locationList(list(1:11), 10, 1))
    expect_equal(locationList(list(1:11), 10, 0)[[1]], as_int64(c(1:10, NA_integer_)))
    
    x <- 1:10
    reshape_or_drop(x, c(2,5), 1)
    expect_equal(dim(x), c(2,5))
    
    reshape_or_drop(x, c(1,10), 1)
    expect_equal(dim(x), c(1,10))
    
    reshape_or_drop(x, NULL, 0)
    expect_equal(dim(x), c(1,10))
    
    reshape_or_drop(x, NULL, 1)
    expect_equal(dim(x), NULL)
    
    dim <- c(3,5,4)
    x <- array(1:prod(dim), dim)
    locs <- lapply(dim, function(x){
        sample(c(1:x, NA, NA), size = 20, replace = TRUE)
    })
    re1 <- x[locs[[1]], locs[[2]], locs[[3]]]
    locs <- lapply(locs, as_int64)
    expect_equal(loc2idx(locs, dim), as_int64(re1))
    
})


test_that("C++: IO - subset/assign", {
    
    set.seed(NULL)
    file <- tempfile()
    unlink(file, recursive = TRUE)
    dim <- 3:5
    x <- filearray_create(file, dim, partition_size = 2)
    
    expect_equal(x[[2]], x$.na)
    
    y <- array(x$.na, dim)
    x[3:1, , c(5,2,1,3,4)] <- 1:60
    y[3:1, , c(5,2,1,3,4)] <- 1:60
    
    expect_equal(x[], y)
    locs <-
        lapply(dim, function(d) {
            as.double(sample(c(1:d, NA, NA), size = 10, replace = TRUE))
        })
    expect_equal(
        x[locs[[1]], locs[[2]], locs[[3]]],
        y[locs[[1]], locs[[2]], locs[[3]]]
    )
    expect_equal(x[,,c(TRUE, NA, FALSE)], y[,,c(TRUE, NA, FALSE)])
    
    expect_error({
        x[locs[[1]], locs[[2]], locs[[3]]] <- 1:prod(sapply(locs, length))
    })
    
    locs <-
        lapply(dim, function(d) {
            as.double(sample(c(1:d, NA, NA), size = d, replace = FALSE))
        })
    expect_error({
        x[locs[[1]], locs[[2]], locs[[3]]] <- 1:prod(sapply(locs, length))
    })
    
    locs <-
        lapply(dim, function(d) {
            sample(c(1:d), size = d, replace = FALSE)
        })
    expect_error({
        x[locs[[1]], locs[[2]], locs[[3]]] <- 1
    })
    
    expect_true({
        x[locs[[1]], locs[[2]], locs[[3]]] <- 1:prod(sapply(locs, length))
        TRUE
    })
    y[locs[[1]], locs[[2]], locs[[3]]] <- 1:prod(sapply(locs, length))
    
    expect_equal(x[], y)
    
    expect_error(x[c(TRUE,TRUE,TRUE,FALSE),,])
    expect_error(x[c(TRUE,TRUE,TRUE,NA),,])
    
    unlink(file, recursive = TRUE)
    x <- filearray_create(file, dim, partition_size = 2)
    y <- array(x$.na, dim)
    x[3:1, , c(TRUE, FALSE, TRUE)] <- 1:36
    y[3:1, , c(TRUE, FALSE, TRUE)] <- 1:36
    expect_equal(x[], y)
    
    expect_error({
        x[3:1, , c(TRUE, FALSE, TRUE, TRUE, NA)] <- 1:36
    })
    
    unlink(file, recursive = TRUE)
})

test_that("C++: IO - type conversion", {
    
    set.seed(1)
    file <- tempfile()
    unlink(file, recursive = TRUE)
    dim <- 3:5
    x <- filearray_create(file, dim, type = 'integer', partition_size = 2)
    
    expect_equal(x[[2]], x$.na)
    
    y <- array(x$.na, dim)
    tmp <- rnorm(60) * 100
    x[3:1, , c(5,2,1,3,4)] <- tmp
    y[3:1, , c(5,2,1,3,4)] <- as.integer(tmp)
    
    expect_equal(x[], y)
    
    
    unlink(file, recursive = TRUE)
    x <- filearray_create(file, dim, type = 'logical', partition_size = 2)
    y <- array(x$.na, dim)
    tmp <- sample(c(1.1, 0, NA), replace = TRUE, size = 60)
    x[] <- tmp
    y[] <- as.logical(tmp)
    expect_equal(x[], y)
    
    unlink(file, recursive = TRUE)
    x <- filearray_create(file, dim, type = 'raw')
    y <- array(x$.na, dim)
    tmp <- sample(0:255, replace = TRUE, size = 60)
    tmp[sample(1:60, size = 10)] <- NA
    suppressWarnings({
        x[] <- tmp
        y[] <- as.raw(tmp)
    })
    
    expect_equal(x[], y)
    unlink(file, recursive = TRUE)
})
