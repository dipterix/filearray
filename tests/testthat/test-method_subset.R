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
