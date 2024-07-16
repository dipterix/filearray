library(testthat)


test_that("map with proxy", {
    
    set.seed(10)
    
    # A large array example
    x1 <- filearray_create(temp_path(check = TRUE), dimension = c(28, 100, 3, 4), initialize = FALSE, partition_size = 3L)
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
    
    y <- filearray_create(temp_path(), dimension = c(12,1))
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
    x <- filearray_create(temp_path(check = TRUE), dimension = c(28, 100, 301, 4), initialize = FALSE, partition_size = 3L)
    dnames <- list(
        Trial = sample(c("A", "B"), 28, replace = TRUE),
        Marker = 1:100,
        Time = seq(-1,2,0.01),
        Location = 1:4
    )
    dimnames(x) <- dnames
    
    expect_equal(dimnames(x), dnames)
    
    y <- array(rnorm(length(x)), dim(x))
    x[] <- y
    
    output <- filearray_create(temp_path(check = TRUE), dimension = dim(x), initialize = FALSE, partition_size = 4L)
    
    f <- function(input){
        # get locational data
        if(is.list(input)){
            location_data <- input[[1]]
        } else {
            location_data <- input
        }
        
        dim(location_data) <- c(28, 100, 301)
        
        # collapse over first 50 time points for
        # each trial, and marker
        baseline <- apply(location_data[,,1:50], c(1,2), mean)
        
        # calibrate
        calibrated <- sweep(location_data, c(1,2), baseline,
                            FUN = function(data, bl){
                                (data / bl - 1) * 100
                            })
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
    x <- filearray_create(temp_path(check = TRUE), dimension = c(28, 100, 301, 4), initialize = FALSE, partition_size = 3L, type = "complex")
    dnames <- list(
        Trial = sample(c("A", "B"), 28, replace = TRUE),
        Marker = 1:100,
        Time = seq(-1,2,0.01),
        Location = 1:4
    )
    dimnames(x) <- dnames
    
    y <- array(rnorm(length(x)), dim(x))
    x[] <- y
    x[1,c(1,3),301,4] <- c(80 + 10i, 80 + 10i)
    y <- x[]
    
    idx <- fwhich(x, val = 80 + 10i, arr.ind = FALSE, ret.values = FALSE)
    expect_equal(idx, which(y == (80 + 10i)))
    
    idx <- fwhich(x, val = 80 + 10i, arr.ind = FALSE, ret.values = TRUE)
    expect_equal(attr(idx, "values"), rep(80 + 10i, length(idx)))
    
    idx1 <- fwhich(x, val = 80 + 10i, arr.ind = TRUE, ret.values = TRUE)
    idx2 <- fwhich(y, val = 80 + 10i, arr.ind = TRUE, ret.values = TRUE)
    expect_equal(idx1, idx2)
    
    # val is a function
    impl <- function(z) { Re(z) > 4 }
    idx1 <- fwhich(x, impl, arr.ind = FALSE, ret.values = TRUE)
    idx2 <- fwhich(x[], impl, arr.ind = FALSE, ret.values = TRUE)
    expect_equal(idx1, idx2)
    
    impl <- function(z) { Im(z) < -100 }
    idx1 <- fwhich(x, impl, arr.ind = TRUE, ret.values = TRUE)
    idx2 <- fwhich(x[], impl, arr.ind = TRUE, ret.values = TRUE)
    expect_equal(idx1, idx2)
    
    x$delete()
})



