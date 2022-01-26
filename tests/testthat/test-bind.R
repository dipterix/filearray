test_that("bind", {
    
    
    x <- array(rnorm(120), 2:5)
    y <- filearray_create(tempfile(), dimension = c(2,3,4,10), partition_size = 3L)
    z <- filearray_create(tempfile(), dimension = c(2,3,4,10), partition_size = 3L)
    
    options("filearray.quiet" = FALSE)
    on.exit({
        options("filearray.quiet" = FALSE)
        y$delete(force = TRUE)
        z$delete(force = TRUE)
    })
    lapply(1:10, function(ii){
        if(ii %% 2 == 0){
            y[,,,ii] <- x[,,,ii / 2]
            z[,,,ii] <- x[,,,ii / 2]
        }
    })
    testthat::expect_warning({
        w <- filearray_bind(y, z, symlink = FALSE)
        w$delete()
    }, regexp = "^One or more arrays have last margin size.+")
    
    y$expand(n = 12)
    z$expand(n = 12)
    w <- filearray_bind(y, z, symlink = TRUE)
    l <- filearray_load(w$.filebase, mode = "readonly")
    
    expect_null({
        filearray_checkload(filebase = w$.filebase, symlink_ok = TRUE)
        filearray_checkload(filebase = w$.filebase, partition = 3)
        NULL
    })
    
    expect_error({
        filearray_checkload(filebase = w$.filebase, symlink_ok = FALSE)
    })
    
    expect_identical(w[], l[])
    expect_identical(w[,,,seq(2,10,2)], x)
    expect_identical(w[,,,seq(2,10,2) + 12], x)
    expect_identical(l[,,,seq(2,10,2)], x)
    expect_identical(l[,,,seq(2,10,2) + 12], x)
    
    expect_identical(l[,,,seq(2,10,2) + c(0, 12, 12, 0, NA)], x[,,,c(1:4, NA)])
    
    expect_equal(
        l$collapse(keep = c(2, 3), method = "sum", na.rm = TRUE),
        apply(x, c(2,3), sum) * 2
    )
    
    
    
    
})
