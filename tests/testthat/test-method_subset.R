test_that("subset filearray-proxy", {

    # normal indexing
    x <- as_filearray(1:120, dimension = c(10,12))
    dimnames(x) <- list(A = 1:10, B = 1:12)
    y <- x + 1L
    z <- y + y
    
    idx1 <- c(3,3,4,5,5,4,3,2,1,1,2,3,3,2,2)
    idx2 <- idx1 * 2
    
    expect_equal(y[idx1, idx2], x[idx1, idx2] + 1)
    expect_equal(z[idx1, idx2], 2 * y[idx1, idx2])
    
    expect_equal(y[, idx2], x[, idx2] + 1)
    expect_equal(z[, idx2], 2 * y[, idx2])
    
    expect_equal(y[idx1, ], x[idx1, ] + 1)
    expect_equal(z[idx1, ], 2 * y[idx1, ])
    
    # filearray as index
    i <- array(FALSE, dim(x))
    i[idx1, idx2] <- TRUE
    j <- as_filearray(i)
    expect_equal(typeof(j), "logical")
    
    expect_equal(x[i], x[][i])
    expect_equal(x[i], x[][j[]])
    
    expect_equal(y[i], x[i] + 1L)
    expect_equal(y[j], y[i])
    
    expect_equal(z[i], y[i] * 2)
    expect_equal(z[j], z[i])
    
    # integer index
    idx <- rep(which(i), 2)
    
    # expect_equal(x[idx], x[][idx])
    # expect_equal(y[idx], y[idx])
    # expect_equal(z[idx], z[idx])
    expect_error(x[idx])
    
})

test_that("subset with empty selection (no matches)", {
    
    # Test case that previously caused segfault:
    # When subset formula evaluates to empty selection on non-first dimension
    x <- as_filearray(matrix(1:16, 4))
    dimnames(x) <- list(A = 1:4, B = 1:4)
    
    # Empty selection on second dimension (was causing segfault)
    result_B <- subset(x, B ~ B == 5)
    expect_equal(dim(result_B), c(4L, 0L))
    expect_equal(length(result_B), 0L)
    
    # Empty selection on first dimension (was working)
    result_A <- subset(x, A ~ A == 5)
    expect_equal(dim(result_A), c(0L, 4L))
    expect_equal(length(result_A), 0L)
    
    # Empty selection on both dimensions
    result_both <- subset(x, A ~ A == 5, B ~ B == 5)
    expect_equal(dim(result_both), c(0L, 0L))
    expect_equal(length(result_both), 0L)
    
    # Direct bracket indexing with empty indices
    expect_equal(dim(x[integer(0), ]), c(0L, 4L))
    expect_equal(dim(x[, integer(0)]), c(4L, 0L))
    expect_equal(dim(x[integer(0), integer(0)]), c(0L, 0L))
    
    # 3D array case
    y <- as_filearray(array(1:24, c(2, 3, 4)))
    dimnames(y) <- list(A = 1:2, B = 1:3, C = 1:4)
    
    # Empty on last dimension
    result_C <- subset(y, C ~ C == 10)
    expect_equal(dim(result_C), c(2L, 3L, 0L))
    
    # Empty on middle dimension  
    result_B3 <- subset(y, B ~ B == 10)
    expect_equal(dim(result_B3), c(2L, 0L, 4L))
    
    # Empty on first dimension
    result_A3 <- subset(y, A ~ A == 10)
    expect_equal(dim(result_A3), c(0L, 3L, 4L))
    
})
