test_that("bind (FileArray)", {
    
    
    x <- array(rnorm(120), 2:5)
    y <- filearray_create(tempfile(), dimension = c(2,3,4,10), partition_size = 3L)
    z <- filearray_create(tempfile(), dimension = c(2,3,4,10), partition_size = 3L)
    
    options("filearray.quiet" = FALSE)
    on.exit({
        options("filearray.quiet" = FALSE)
        y$delete(force = TRUE)
        z$delete(force = TRUE)
    }, add = TRUE)
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
    
    
    
    options("filearray.quiet" = TRUE)
    y$expand(n = 12)
    z$expand(n = 12)
    w <- filearray_bind(y, z, symlink = TRUE)
    l <- filearray_load(w$.filebase, mode = "readonly")
    
    on.exit({
        w$.mode <- "readwrite"
        w$delete()
        l$.mode <- "readwrite"
        l$delete()
    }, add = TRUE)
    
    expect_null({
        filearray_checkload(filebase = w$.filebase, symlink_ok = TRUE)
        filearray_checkload(filebase = w$.filebase, partition = 3)
        NULL
    })
    
    if(w$.header$filearray_bind$symlink){
        expect_error({
            filearray_checkload(filebase = w$.filebase, symlink_ok = FALSE)
        })
    }
    
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
    
    # Check if cached bind works
    l <- filearray_bind(y, z, filebase = w$.filebase, symlink = w$.header$filearray_bind$symlink, overwrite = TRUE, cache_ok = TRUE)
    
    expect_true(attr(l, "cached_bind"))
    
})


test_that("bind (FileArrayProxy)", {
    
    
    y <- filearray_create(tempfile(), dimension = c(2,3,4,10), partition_size = 3L, type = "integer")
    z <- filearray_create(tempfile(), dimension = c(2,3,4,10), partition_size = 3L, type = "double")
    
    options("filearray.quiet" = FALSE)
    on.exit({
        options("filearray.quiet" = FALSE)
        y$delete(force = TRUE)
        z$delete(force = TRUE)
    }, add = TRUE)
    
    y[] <- rep(0L, 240)
    z[] <- rep(0L, 240)
    x <- array(rnorm(240), c(2,3,4,10))
    
    y <- y + x
    z <- z + x
    
    testthat::expect_error(y$expand(n = 12))
    
    options("filearray.quiet" = TRUE)
    w <- filearray_bind(y, z, symlink = TRUE)
    l <- filearray_load(w$.filebase, mode = "readonly")
    
    on.exit({
        w$.mode <- "readwrite"
        w$delete()
        l$.mode <- "readwrite"
        l$delete()
    }, add = TRUE)
    
    expect_null({
        filearray_checkload(filebase = w$.filebase, symlink_ok = TRUE)
        filearray_checkload(filebase = w$.filebase, partition = 3)
        NULL
    })
    
    if(w$.header$filearray_bind$symlink){
        expect_error({
            filearray_checkload(filebase = w$.filebase, symlink_ok = FALSE)
        })
    }
    
    expect_identical(w[], l[])
    expect_identical(w[,,,1:10, dimnames = NULL], x)
    expect_identical(w[,,,1:10 + 12, dimnames = NULL], x)
    expect_identical(l[,,,1:10, dimnames = NULL], x)
    expect_identical(l[,,,1:10 + 12, dimnames = NULL], x)
    
    expect_identical(l[,,,seq(2,10,2) + c(0, 12, 12, 0, NA), dimnames = NULL], x[,,,c(1:4*2, NA)])
    
    expect_equal(
        l$collapse(keep = c(2, 3), method = "sum", na.rm = TRUE),
        apply(x, c(2,3), sum) * 2
    )
    
    expect_equal(
        y$collapse(keep = c(2, 3), method = "sum", na.rm = TRUE),
        apply(x, c(2,3), sum)
    )
    expect_equal(
        z$collapse(keep = c(2, 3), method = "sum", na.rm = TRUE),
        apply(x, c(2,3), sum)
    )
    
    
    # Check if cached bind works
    # l <- filearray_bind(y, z, filebase = w$.filebase, symlink = w$.header$filearray_bind$symlink, overwrite = TRUE, cache_ok = TRUE)
    # 
    # expect_true(attr(l, "cached_bind"))
    
})
