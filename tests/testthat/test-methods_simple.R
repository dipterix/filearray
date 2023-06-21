test_that("Simple operators", {
  
    # logical
    x <- as_filearrayproxy(1:24, dimension = c(4,6), type = "double")
    x[3:4,3] <- rep(NA, 2)
    y <- !is.na(x)
    testthat::expect_s4_class(y, "FileArrayProxy")
    testthat::expect_equal(y[], !is.na(x[]))
    
    # math to double
    x0 <- x[]
    suppressWarnings({
        for(op in FILEARRAY_SIMPLE_OPS$math) {
            y <- do.call(op, list(x))
            testthat::expect_s4_class(y, "FileArrayProxy")
            testthat::expect_equal(y[], do.call(op, list(x0)))
        }
    })
    
    # math to complex
    x0 <- array(rnorm(20) + rnorm(20) * 1i, c(5,4))
    x <- as_filearrayproxy(x0)
    suppressWarnings({
        for(op in FILEARRAY_SIMPLE_OPS$math) {
            valid_op <- tryCatch({
                y0 <- do.call(op, list(x0))
                TRUE
            }, error = function(e) {
                FALSE
            })
            
            if( valid_op ) {
                y <- do.call(op, list(x))
                testthat::expect_s4_class(y, "FileArrayProxy")
                dif <- y[dimnames = NULL] - y0
                mag <- Mod(y[dimnames = NULL] + y0)
                mag[mag < 1] <- 1
                dif <- dif / Mod(mag)
                testthat::expect_lt(max(abs(Re(dif))), 1e-5, label = sprintf("Operator `%s` on complex", op))
                testthat::expect_lt(max(abs(Im(dif))), 1e-5, label = sprintf("Operator `%s` on complex", op))
            } else {
                testthat::expect_error(do.call(op, list(x)), label = sprintf("Operator `%s` on complex should fail, but", op))
            }
            
            
        }
    })

    # complex
    for(op in FILEARRAY_SIMPLE_OPS$complex) {
        y0 <- do.call(op, list(x0))
        y <- do.call(op, list(x))
        testthat::expect_s4_class(y, "FileArrayProxy")
        dif <- y[dimnames = NULL] - y0
        mag <- Mod(y[dimnames = NULL] + y0)
        mag[mag < 1] <- 1
        dif <- dif / Mod(mag)
        testthat::expect_lt(max(abs(Re(dif))), 1e-5, label = sprintf("Operator `%s` on complex", op))
        testthat::expect_lt(max(abs(Im(dif))), 1e-5, label = sprintf("Operator `%s` on complex", op))
    }
    
})
