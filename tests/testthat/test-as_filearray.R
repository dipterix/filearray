test_that("as_filearray", {
    options("filearray.operator.precision" = "float")
    on.exit({
        options("filearray.operator.precision" = NULL)
    }, add = TRUE)
    
    x <- rnorm(24)
    dim(x) <- c(2, 3, 4)
    
    # as_filearray.default
    y <- as_filearray(x)
    testthat::expect_equal(y$type(), getOption("filearray.operator.precision"))
    testthat::expect_equal(y[], x, tolerance = 1e-5, ignore_attr = TRUE)
    testthat::expect_equal(y$.mode, "readwrite")
    
    # as_filearray.character
    y <- as_filearray(y$.filebase)
    testthat::expect_equal(y[dimnames = FALSE], x, tolerance = 1e-5, ignore_attr = TRUE)
    testthat::expect_equal(y$.mode, "readonly")
    
    # as_filearray.FileArray(Proxy)
    z <- as_filearray(y)
    testthat::expect_identical(z, y)
    
    z <- as_filearray(as_filearrayproxy(y))
    testthat::expect_equal(typeof(z), getOption("filearray.operator.precision"))
    testthat::expect_equal(z[], x, tolerance = 1e-5, ignore_attr = TRUE)
    
    y$.mode <- "readwrite"
    y$delete()
    
    testthat::expect_false(z$valid())
    
})


test_that("as_filearrayproxy", {
    options("filearray.operator.precision" = "float")
    on.exit({
        options("filearray.operator.precision" = NULL)
    }, add = TRUE)
    
    x <- rnorm(24)
    dim(x) <- c(2, 3, 4)
    dimnames(x) <- list(A = 1:2, B = 1:3, C = letters[1:4])
    
    # as_filearrayproxy.default
    y <- as_filearrayproxy(x)
    testthat::expect_equal(typeof(y), getOption("filearray.operator.precision"))
    testthat::expect_equal(y[], x, tolerance = 1e-5)
    testthat::expect_equal(y$.mode, "readwrite")
    
    # as_filearrayproxy.character
    y <- as_filearrayproxy(y$.filebase)
    testthat::expect_equal(y[], x, tolerance = 1e-5)
    testthat::expect_equal(y$.mode, "readwrite")
    
    # as_filearrayproxy.FileArrayProxy
    z <- as_filearrayproxy(y)
    testthat::expect_false(identical(z, y))
    testthat::expect_equal(y[], z[])
    
    y$delete()
    
    testthat::expect_false(z$valid())
    
})
