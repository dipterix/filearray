test_that("subset-assign filearray-proxy", {
    
    # normal indexing
    x0 <- array(1:120, dim = c(10,12))
    dimnames(x0) <- list(A = 1:10, B = 1:12)
    x <- as_filearray(x0)
    y <- x + 1L
    y[,1] <- rep(0, 10)
    y[1,] <- rep(0, 12)
    z <- y + y
    
    idx1 <- c(3,4,5,2,1)
    idx2 <- idx1 * 2
    
    expect_equal(x[idx1, idx2], x0[idx1, idx2])
    expect_equal(y[1,1:12, dimnames = NULL], rep(0L, 12))
    expect_equal(y[1:10,1, dimnames = NULL], rep(0L, 10))
    expect_equal(y[2:10,2:10], x0[2:10,2:10] + 1L)
    expect_equal(z[, idx2], 2 * y[, idx2])
    
    expect_equal(y[2:10, 2:12], x[2:10, 2:12] + 1)
    expect_equal(z[], 2 * y[])
    
    clear_cache()
    
    # Array indexing
    idx1 <- c(3,4,5,2,1)
    idx2 <- idx1 * 2
    
    i <- array(FALSE, dim(x))
    i[idx1, idx2] <- TRUE
    j <- as_filearray(i)
    expect_equal(typeof(j), "logical")
    
    x0 <- array(1:120, dim = c(10,12))
    dimnames(x0) <- list(A = 1:10, B = 1:12)
    x <- as_filearray(x0)
    y <- x + 1L
    y[j,lazy = TRUE] <- 0
    z <- y + y
    z[j, lazy = TRUE] <- 1
    
    expect_equal(y$.filebase, x$.filebase)
    expect_equal(z$.filebase, x$.filebase)
    
    # mature z
    expect_equal(z[j], rep(1, sum(j)))
    expect_equal(y[j], rep(0, sum(j)))
    
    expect_equal(x[!i], x0[!i])
    expect_equal(y[!i], x[!i] + 1L)
    expect_equal(z[!i], y[!i] * 2)
    
    expect_equal(x[!j], x0[!i])
    expect_equal(y[!j], x[!i] + 1L)
    expect_equal(z[!j], y[!i] * 2)
    
    clear_cache()
})
