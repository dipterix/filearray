library(testthat)


test_that("map with proxy", {
    set.seed(10)
    
    # A large array example
    x1 <- filearray_create(
        temp_path(check = TRUE),
        dimension = c(28, 100, 3, 4),
        initialize = FALSE,
        partition_size = 3L
    )
    x1[] <- rnorm(33600)
    x2 <- x1 + 1
    x3 <- x1 + x2
    x4 <- as_filearray(matrix(1:12, nrow = 4))
    
    x <- list(x1, x2, x3, x4)
    # check common input size
    
    bc <- 12
    re <- fmap2(x, function(input) {
        testthat::expect_length(input, 4)
        testthat::expect_length(input[[1]], length(x1) / bc)
        testthat::expect_length(input[[2]], length(x2) / bc)
        testthat::expect_length(input[[3]], length(x3) / bc)
        testthat::expect_length(input[[4]], length(x4) / bc)
        testthat::expect_equal(input[[1]] + 1, input[[2]])
        testthat::expect_equal(input[[3]], input[[2]] + input[[1]])
        
        sum(input[[4]]) + sum(input[[3]] - input[[2]] - input[[1]])
    }, .buffer_count = bc)
    expect_equal(re, colSums(matrix(x4[], ncol = bc)))
    
    
    bc <- 4
    re <- fmap2(x, function(input) {
        testthat::expect_length(input, 4)
        testthat::expect_length(input[[1]], length(x1) / bc)
        testthat::expect_length(input[[2]], length(x2) / bc)
        testthat::expect_length(input[[3]], length(x3) / bc)
        testthat::expect_length(input[[4]], length(x4) / bc)
        testthat::expect_equal(input[[1]] + 1, input[[2]])
        testthat::expect_equal(input[[3]], input[[2]] + input[[1]])
        
        sum(input[[4]]) + sum(input[[3]] - input[[2]] - input[[1]])
    }, .buffer_count = bc)
    expect_equal(re, colSums(matrix(x4[], ncol = bc)))
    
    bc <- 1
    re <- fmap2(x, function(input) {
        testthat::expect_length(input, 4)
        testthat::expect_length(input[[1]], length(x1) / bc)
        testthat::expect_length(input[[2]], length(x2) / bc)
        testthat::expect_length(input[[3]], length(x3) / bc)
        testthat::expect_length(input[[4]], length(x4) / bc)
        testthat::expect_equal(input[[1]] + 1, input[[2]])
        testthat::expect_equal(input[[3]], input[[2]] + input[[1]])
        
        sum(input[[4]]) + sum(input[[3]] - input[[2]] - input[[1]])
    }, .buffer_count = bc)
    expect_equal(re, colSums(matrix(x4[], ncol = bc)))
    
    
    # check fmap
    bc <- 12
    
    y <- filearray_create(temp_path(), dimension = c(12, 1))
    fmap(x, function(input) {
        testthat::expect_length(input, 4)
        testthat::expect_length(input[[1]], length(x1) / bc)
        testthat::expect_length(input[[2]], length(x2) / bc)
        testthat::expect_length(input[[3]], length(x3) / bc)
        testthat::expect_length(input[[4]], length(x4) / bc)
        testthat::expect_equal(input[[1]] + 1, input[[2]])
        testthat::expect_equal(input[[3]], input[[2]] + input[[1]])
        
        input[[4]] + sum(input[[3]] - input[[2]] - input[[1]])
    }, .buffer_count = bc, .y = y)
    expect_equal(as.vector(y[]), as.vector(x4[]))
    
    clear_cache()
})


test_that("map filearrays", {
    # A large array example
    x <- filearray_create(
        temp_path(check = TRUE),
        dimension = c(28, 100, 301, 4),
        initialize = FALSE,
        partition_size = 3L
    )
    dnames <- list(
        Trial = sample(c("A", "B"), 28, replace = TRUE),
        Marker = 1:100,
        Time = seq(-1, 2, 0.01),
        Location = 1:4
    )
    dimnames(x) <- dnames
    
    expect_equal(dimnames(x), dnames)
    
    y <- array(rnorm(length(x)), dim(x))
    x[] <- y
    
    output <- filearray_create(
        temp_path(check = TRUE),
        dimension = dim(x),
        initialize = FALSE,
        partition_size = 4L
    )
    
    f <- function(input) {
        # get locational data
        if (is.list(input)) {
            location_data <- input[[1]]
        } else {
            location_data <- input
        }
        
        dim(location_data) <- c(28, 100, 301)
        
        # collapse over first 50 time points for
        # each trial, and marker
        baseline <- apply(location_data[, , 1:50], c(1, 2), mean)
        
        # calibrate
        calibrated <- sweep(
            location_data,
            c(1, 2),
            baseline,
            FUN = function(data, bl) {
                (data / bl - 1) * 100
            }
        )
        return(calibrated)
    }
    
    fmap(x, f, .y = output, .buffer_count = 4)
    
    b <- apply(y, 4, f)
    dim(b) <- dim(y)
    
    expect_equal(output[], b)
    
    d <- fmap2(x, f, .buffer_count = 4, .simplify = TRUE)
    expect_equal(d, b)
    
    x$delete()
    output$delete()
    clear_cache()
})


test_that("fwhich", {
    x <- filearray_create(
        temp_path(check = TRUE),
        dimension = c(28, 100, 301, 4),
        initialize = FALSE,
        partition_size = 3L,
        type = "complex"
    )
    dnames <- list(
        Trial = sample(c("A", "B"), 28, replace = TRUE),
        Marker = 1:100,
        Time = seq(-1, 2, 0.01),
        Location = 1:4
    )
    dimnames(x) <- dnames
    
    y <- array(rnorm(length(x)), dim(x))
    x[] <- y
    x[1, c(1, 3), 301, 4] <- c(80 + 10i, 80 + 10i)
    y <- x[]
    
    idx <- fwhich(x,
                  val = 80 + 10i,
                  arr.ind = FALSE,
                  ret.values = FALSE)
    expect_equal(idx, which(y == (80 + 10i)))
    
    idx <- fwhich(x,
                  val = 80 + 10i,
                  arr.ind = FALSE,
                  ret.values = TRUE)
    expect_equal(attr(idx, "values"), rep(80 + 10i, length(idx)))
    
    idx1 <- fwhich(x,
                   val = 80 + 10i,
                   arr.ind = TRUE,
                   ret.values = TRUE)
    idx2 <- fwhich(y,
                   val = 80 + 10i,
                   arr.ind = TRUE,
                   ret.values = TRUE)
    expect_equal(idx1, idx2)
    
    # val is a function
    impl <- function(z) {
        Re(z) > 4
    }
    idx1 <- fwhich(x, impl, arr.ind = FALSE, ret.values = TRUE)
    idx2 <- fwhich(x[], impl, arr.ind = FALSE, ret.values = TRUE)
    expect_equal(idx1, idx2)
    
    impl <- function(z) {
        Im(z) < -100
    }
    idx1 <- fwhich(x, impl, arr.ind = TRUE, ret.values = TRUE)
    idx2 <- fwhich(x[], impl, arr.ind = TRUE, ret.values = TRUE)
    expect_equal(idx1, idx2)
    
    x$delete()
})

test_that("mapreduce visits every slice of partitions with several slices", {
    # 9 slices of 2 elements in partitions of 3 slices: elements 1-6, 7-12, 13-18
    x <- filearray_create(temp_path(check = TRUE), c(2, 9), partition_size = 3)
    on.exit(x$delete(force = TRUE), add = TRUE)
    x[] <- 1:18

    chunks <- mapreduce(x, map = function(data, size, first_index) {
        c(first_index = first_index, size = size, first = data[[1]])
    }, reduce = function(mapped) {
        do.call(rbind, mapped)
    })
    expect_equal(unname(chunks[, "first_index"]), c(1, 7, 13))
    expect_equal(unname(chunks[, "size"]), c(6, 6, 6))
    expect_equal(unname(chunks[, "first"]), c(1, 7, 13))

    expect_equal(sum(x), 171)
    expect_equal(sum(x, na.rm = TRUE), 171)
    expect_equal(max(x), 18)
    expect_equal(min(x), 1)
    expect_equal(range(x), c(1, 18))
    expect_equal(fwhich(x, c(5, 17)), c(5, 17))

    # a shorter last partition: elements 1-6, 7-12, 13-16
    y <- filearray_create(temp_path(check = TRUE), c(2, 8), partition_size = 3)
    on.exit(y$delete(force = TRUE), add = TRUE)
    y[] <- 1:16

    chunks <- mapreduce(y, map = function(data, size, first_index) {
        c(first_index = first_index, size = size)
    }, reduce = function(mapped) {
        do.call(rbind, mapped)
    })
    expect_equal(unname(chunks[, "first_index"]), c(1, 7, 13))
    expect_equal(unname(chunks[, "size"]), c(6, 6, 4))
    expect_equal(sum(y), 136)
    expect_equal(max(y), 16)
    expect_equal(fwhich(y, 15), 15)

    # mapreduce skips a missing partition; the next one keeps its position
    z <- filearray_create(temp_path(check = TRUE), c(2, 8), partition_size = 3)
    on.exit(z$delete(force = TRUE), add = TRUE)
    z[, 1:3] <- 1:6
    z[, 7:8] <- 13:16

    chunks <- mapreduce(z, map = function(data, size, first_index) {
        c(first_index = first_index, size = size)
    }, reduce = function(mapped) {
        do.call(rbind, mapped)
    })
    expect_equal(unname(chunks[, "first_index"]), c(1, 13))
    expect_equal(unname(chunks[, "size"]), c(6, 4))
    expect_equal(fwhich(z, 15), 15)
    expect_equal(sum(z, na.rm = TRUE), 79)
})

test_that("fmap2 passes on errors raised by the mapping function", {
    x <- filearray_create(temp_path(check = TRUE), c(2, 3, 2), partition_size = 1)
    on.exit(x$delete(force = TRUE), add = TRUE)
    x[] <- 1:12

    expect_error(fmap2(x, function(input) {
        stop("boom from fmap2")
    }), "boom from fmap2")

    # the condition itself reaches the caller, not only its message
    cond <- structure(
        class = c("fmap2_test_error", "error", "condition"),
        list(message = "custom condition", call = NULL)
    )
    expect_error(fmap2(x, function(input) {
        stop(cond)
    }), class = "fmap2_test_error")

    # and fmap2 still works afterwards
    expect_equal(fmap2(x, function(input) {
        sum(input[[1]])
    }, .buffer_count = 2), c(21, 57))
})
