test_that("Subset with dimnames", {
    x <- filearray_create(tempfile(), dimension = c(3,4,5,6))
    on.exit({
        x$delete()
    })
    y <- array(as.double(1:240), c(3,4,5,6))
    x[] <- y
    
    dnames1 <- list(
        A = 1:3,
        B = 1:4,
        NULL,
        D = 1:6
    )
    dnames2 <- list(
        A = 1:3
    )
    dnames3 <- list(
        A = 1:3,
        B = 1:4,
        C = NULL,
        D = NULL
    )
    dnames4 <- list(
        A = 1:3,
        B = 1:4,
        C = 1:5,
        D = 1:6
    )
    
    expect_error({
        dimnames(x) <- list(
            A = 1:4
        )
    })
    
    dimnames(y) <- dnames1
    dimnames(x) <- dnames1
    
    expect_identical(dimnames(x[]), dimnames(y))
    expect_identical(dimnames(x[1,1:2,2:3,1:4]), dimnames(y[1,1:2,2:3,1:4]))
    expect_identical(dimnames(x[1,1:2,2:3,1:4,drop=FALSE]),
                     dimnames(y[1,1:2,2:3,1:4,drop=FALSE]))
    expect_identical(names(x[2,1,1,1]), names(y[2,1,1,1]))
    
    dimnames(y) <- dnames2
    dimnames(x) <- dnames2
    
    expect_identical(dimnames(x[]), dimnames(y))
    expect_identical(dimnames(x[1,1:2,2:3,1:4]), dimnames(y[1,1:2,2:3,1:4]))
    expect_identical(dimnames(x[1,1:2,2:3,1:4,drop=FALSE]),
                     dimnames(y[1,1:2,2:3,1:4,drop=FALSE]))
    expect_identical(names(x[2,1,1,1]), names(y[2,1,1,1]))
    
    dimnames(y) <- dnames3
    dimnames(x) <- dnames3
    
    expect_identical(dimnames(x[]), dimnames(y))
    expect_identical(dimnames(x[1,1:2,2:3,1:4]), dimnames(y[1,1:2,2:3,1:4]))
    expect_identical(dimnames(x[1,1:2,2:3,1:4,drop=FALSE]),
                     dimnames(y[1,1:2,2:3,1:4,drop=FALSE]))
    expect_identical(names(x[2,1,1,1]), names(y[2,1,1,1]))
    
    
    dimnames(y) <- dnames4
    dimnames(x) <- dnames4
    
    expect_identical(dimnames(x[]), dimnames(y))
    expect_identical(dimnames(x[1,1:2,2:3,1:4]), dimnames(y[1,1:2,2:3,1:4]))
    expect_identical(dimnames(x[1,1:2,2:3,1:4,drop=FALSE]),
                     dimnames(y[1,1:2,2:3,1:4,drop=FALSE]))
    expect_identical(names(x[2,1,1,1]), names(y[2,1,1,1]))
    
    
    # expand
    dimnames(x) <- dnames4
    x$expand(10)
    expect_equal(dimnames(x)$D, c(dnames4$D, rep(NA_integer_, 4)))
    
})
