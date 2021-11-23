
require(testthat)
as_int64 <- function(x){
    realToInt64(x, NA_real_, NA_real_, 0L)
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
    bsz <- get_buffer_size()
    on.exit({
        set_buffer_size(bsz)
        max_buffer_size(2097152)
    })
    set_buffer_size(16L)
    max_buffer_size(64L)
    
    set.seed(NULL)
    file <- tempfile()
    unlink(file, recursive = TRUE)
    dim <- 33:35
    x <- filearray_create(file, dim, partition_size = 2, initialize = FALSE)
    
    expect_equal(x[[2]], x$.na)
    
    y <- array(x$.na, dim)
    x[33:1, , c(35,2,1,3,4,5:34)] <- 1:prod(dim)
    y[33:1, , c(35,2,1,3,4,5:34)] <- 1:prod(dim)
    
    expect_equal(x[], y)
    locs <-
        lapply(dim, function(d) {
            d1 <- sample(c(1:d), 10, replace = TRUE)
            d2 <- c(NA, NA)
            as.double(sample(c(d1,d2)))
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
            d1 <- sample(c(1:d), 10, replace = TRUE)
            d2 <- c(NA, NA)
            as.double(sample(c(d1,d2)))
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
    
    expect_error(x[c(rep(c(TRUE,TRUE,TRUE, TRUE),8), TRUE, FALSE),,])
    expect_error(x[c(rep(c(TRUE,TRUE,TRUE, TRUE),8), TRUE, NA),,])
    
    unlink(file, recursive = TRUE)
    x <- filearray_create(file, dim, partition_size = 2, initialize = FALSE)
    y <- array(x$.na, dim)
    x[33:1, , c(35,2,1,3,4,5:34)] <- 1:prod(dim)
    y[33:1, , c(35,2,1,3,4,5:34)] <- 1:prod(dim)
    expect_equal(x[], y)
    
    expect_error({
        x[3:1, , c(TRUE, FALSE, TRUE, TRUE, rep(NA, 29))] <- 1:36
    })
    
    unlink(file, recursive = TRUE)
})

test_that("C++: IO - subset/assign - complex", {
    bsz <- get_buffer_size()
    on.exit({
        set_buffer_size(bsz)
        max_buffer_size(2097152)
    })
    set_buffer_size(16L)
    max_buffer_size(64L)
    
    set.seed(8)
    file <- tempfile()
    unlink(file, recursive = TRUE)
    dim <- 33:35
    x <- filearray_create(file, dim, partition_size = 2, type = "complex", initialize = FALSE)
    
    expect_equal(x[[2]], x$.na)
    
    y <- array(x$.na, dim)
    tmp <- rnorm(prod(dim)) + 1i*runif(prod(dim))
    x[33:1, , c(35,2,1,3,4,5:34)] <- tmp
    y[33:1, , c(35,2,1,3,4,5:34)] <- tmp
    
    expect_lt(max(Mod(x[] - y)), 1e-6)
    locs <-
        lapply(dim, function(d) {
            d1 <- sample(c(1:d), 10, replace = TRUE)
            d2 <- c(NA, NA)
            as.double(sample(c(d1,d2)))
        })
    max_dif <- max(Mod(
        x[locs[[1]], locs[[2]], locs[[3]]] -
            y[locs[[1]], locs[[2]], locs[[3]]]
    ), na.rm = TRUE)
    expect_lt(max_dif, 1e-5)
    if(max_dif > 1e-6){
        print(max_dif)
    }
    expect_equal(is.na(x[locs[[1]], locs[[2]], locs[[3]]]),
                 is.na(y[locs[[1]], locs[[2]], locs[[3]]]))
    expect_lt(
        max(Mod(
            x[,,c(TRUE, NA, FALSE)] -
                y[,,c(TRUE, NA, FALSE)]
        ), na.rm = TRUE),
        1e-6
    )
    expect_equal(is.na(x[,,c(TRUE, NA, FALSE)]),
                 is.na(y[,,c(TRUE, NA, FALSE)]))
    
    expect_error({
        x[locs[[1]], locs[[2]], locs[[3]]] <- tmp[1:prod(sapply(locs, length))]
    })
    
    locs <-
        lapply(dim, function(d) {
            d1 <- sample(c(1:d), 10, replace = TRUE)
            d2 <- c(NA, NA)
            as.double(sample(c(d1,d2)))
        })
    expect_error({
        x[locs[[1]], locs[[2]], locs[[3]]] <- tmp[1:prod(sapply(locs, length))]
    })
    
    locs <-
        lapply(dim, function(d) {
            sample(c(1:d), size = d, replace = FALSE)
        })
    expect_error({
        x[locs[[1]], locs[[2]], locs[[3]]] <- 1
    })
    
    expect_true({
        x[locs[[1]], locs[[2]], locs[[3]]] <- tmp[1:prod(sapply(locs, length))]
        TRUE
    })
    y[locs[[1]], locs[[2]], locs[[3]]] <- tmp[1:prod(sapply(locs, length))]
    
    expect_lt(max(Mod(x[] - y)), 1e-6)
    
    expect_error(x[c(rep(c(TRUE,TRUE,TRUE, TRUE),8), TRUE, FALSE),,])
    expect_error(x[c(rep(c(TRUE,TRUE,TRUE, TRUE),8), TRUE, NA),,])
    
    unlink(file, recursive = TRUE)
    x <- filearray_create(file, dim, partition_size = 2, type = 'complex', initialize = FALSE)
    y <- array(x$.na, dim)
    x[33:1, , c(35,2,1,3,4,5:34)] <- tmp
    y[33:1, , c(35,2,1,3,4,5:34)] <- tmp
    expect_lt(max(Mod(x[] - y)), 1e-6)
    
    expect_error({
        x[3:1, , c(TRUE, FALSE, TRUE, TRUE, rep(NA, 29))] <- tmp[1:36]
    })
    
    unlink(file, recursive = TRUE)
})

test_that("C++: IO - subset/assign - float", {
    bsz <- get_buffer_size()
    on.exit({
        set_buffer_size(bsz)
        max_buffer_size(2097152)
    })
    set_buffer_size(16L)
    max_buffer_size(64L)
    
    set.seed(NULL)
    file <- tempfile()
    unlink(file, recursive = TRUE)
    dim <- 33:35
    x <- filearray_create(file, dim, partition_size = 2, type = "float", initialize = FALSE)
    
    expect_equal(x[[2]], x$.na)
    
    y <- array(x$.na, dim)
    tmp <- rnorm(prod(dim))
    x[33:1, , c(35,2,1,3,4,5:34)] <- tmp
    y[33:1, , c(35,2,1,3,4,5:34)] <- tmp
    
    eps <- 10^(ceiling(log10(max(abs(y)))) - 7)
    expect_equal(x[], y, tolerance = eps)
    y[] <- x[]
    locs <-
        lapply(dim, function(d) {
            d1 <- sample(c(1:d), 10, replace = TRUE )
            d2 <- c(NA, NA)
            as.double(sample(c(d1,d2)))
        })
    
    a <- x[locs[[1]], locs[[2]], locs[[3]]]
    b <- y[locs[[1]], locs[[2]], locs[[3]]]
    expect_equal(is.na(a), is.na(b))
    
    sel <- !is.na(a-b) & (a-b) > eps
    if(length(a[sel])){
        # fail the test
        print(a[sel])
        print(b[sel])
        expect_length(object = (a-b)[sel], n = 0)
    }
    
    
    
    a <- x[c(1,1,2,2,1,1,2,2), 1, c(2,2)]
    b <- y[c(1,1,2,2,1,1,2,2), 1, c(2,2)]
    # a <- x[c(1,1,2,2), c(1,1,2,2), 1]
    # b <- y[c(1,1,2,2), c(1,1,2,2), 1]
    expect_equal(a, b, tolerance = eps)
    
    expect_equal(x[,,c(TRUE, NA, FALSE)], y[,,c(TRUE, NA, FALSE)])
    
    expect_error({
        x[locs[[1]], locs[[2]], locs[[3]]] <- 1:prod(sapply(locs, length))
    })
    
    locs <-
        lapply(dim, function(d) {
            d1 <- sample(c(1:d), 10, replace = TRUE)
            d2 <- c(NA, NA)
            as.double(sample(c(d1,d2)))
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
    
    expect_error(x[c(rep(c(TRUE,TRUE,TRUE, TRUE),8), TRUE, FALSE),,])
    expect_error(x[c(rep(c(TRUE,TRUE,TRUE, TRUE),8), TRUE, NA),,])
    
    unlink(file, recursive = TRUE)
    x <- filearray_create(file, dim, partition_size = 2, initialize = FALSE)
    y <- array(x$.na, dim)
    x[33:1, , c(35,2,1,3,4,5:34)] <- 1:prod(dim)
    y[33:1, , c(35,2,1,3,4,5:34)] <- 1:prod(dim)
    expect_equal(x[], y)
    
    expect_error({
        x[3:1, , c(TRUE, FALSE, TRUE, TRUE, rep(NA, 29))] <- 1:36
    })
    
    unlink(file, recursive = TRUE)
})

test_that("C++: IO - type conversion", {
    
    set.seed(1)
    file <- tempfile()
    unlink(file, recursive = TRUE)
    dim <- 3:5
    x <- filearray_create(file, dim, type = 'integer', partition_size = 2, initialize = FALSE)
    
    expect_equal(x[[2]], x$.na)
    
    y <- array(x$.na, dim)
    tmp <- rnorm(60) * 100
    x[3:1, , c(5,2,1,3,4)] <- tmp
    y[3:1, , c(5,2,1,3,4)] <- as.integer(tmp)
    
    expect_equal(x[], y)
    
    
    unlink(file, recursive = TRUE)
    x <- filearray_create(file, dim, type = 'logical', partition_size = 2, initialize = FALSE)
    y <- array(x$.na, dim)
    tmp <- sample(c(1.1, 0, NA), replace = TRUE, size = 60)
    x[] <- tmp
    y[] <- as.logical(tmp)
    expect_equal(x[], y)
    
    unlink(file, recursive = TRUE)
    x <- filearray_create(file, dim, type = 'raw', initialize = FALSE)
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

