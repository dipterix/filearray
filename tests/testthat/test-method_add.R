test_that("method: addition", {
    on.exit({
        options("filearray.operator.precision" = NULL)
        clear_cache()
    })
    
    dm <- c(2,3,4, 10)
    len <- prod(dm)
    
    x_dbl <- array(rnorm(len), dim = dm)
    x_int <- array(seq_len(len), dim = dm)
    x_lgl <- x_dbl > 0
        
    arr_dbl <- as_filearray(x_dbl, type = "double", partition_size = 2)
    arr_int <- as_filearray(x_int, type = "integer", partition_size = 3)
    arr_lgl <- as_filearray(x_lgl, type = "logical", partition_size = 4)
    
    
    proxy_dbl <- as_filearrayproxy(arr_dbl)
    proxy_int <- as_filearrayproxy(arr_int)
    proxy_lgl <- as_filearrayproxy(arr_lgl)
    
    # sanity
    expect_equal(arr_dbl[dimnames = NULL], x_dbl, tolerance = 1e-5)
    expect_equal(arr_int[dimnames = NULL], x_int)
    expect_equal(arr_lgl[dimnames = NULL], x_lgl)
    
    expect_equal(proxy_dbl[dimnames = NULL], x_dbl, tolerance = 1e-5)
    expect_equal(proxy_int[dimnames = NULL], x_int)
    expect_equal(proxy_lgl[dimnames = NULL], x_lgl)
    
    check_add <- function(e1, e2, d1, d2, type, tolerance = .Machine$double.eps) {
        y <- e1+e2
        res <- y[dimnames = NULL]
        
        if(!missing(type)) {
            expect_identical(typeof(y), type)
        }
        
        expect_equal(res, d1 + d2, tolerance = tolerance)
    }
    
    options("filearray.operator.precision" = "double")
    
    # filearrayproxy - Double
    check_add(proxy_dbl, proxy_dbl, x_dbl, x_dbl, "double", 1e-5)
    check_add(proxy_dbl, proxy_int, x_dbl, x_int, "double", 1e-5)
    check_add(proxy_dbl, proxy_lgl, x_dbl, x_lgl, "double", 1e-5)
    
    check_add(proxy_dbl, arr_dbl, x_dbl, x_dbl, "double", 1e-5)
    check_add(proxy_dbl, arr_int, x_dbl, x_int, "double", 1e-5)
    check_add(proxy_dbl, arr_lgl, x_dbl, x_lgl, "double", 1e-5)
    
    check_add(proxy_dbl, 1.0, x_dbl, 1.0, "double", 1e-5)
    check_add(proxy_dbl, 1L, x_dbl, 1L, "double", 1e-5)
    check_add(proxy_dbl, TRUE, x_dbl, TRUE, "double", 1e-5)
    
    # filearrayproxy - Int
    check_add(proxy_int, proxy_dbl, x_int, x_dbl, "double", 1e-5)
    check_add(proxy_int, proxy_int, x_int, x_int, "integer", 1e-5)
    check_add(proxy_int, proxy_lgl, x_int, x_lgl, "integer", 1e-5)
    
    check_add(proxy_int, arr_dbl, x_int, x_dbl, "double", 1e-5)
    check_add(proxy_int, arr_int, x_int, x_int, "integer", 1e-5)
    check_add(proxy_int, arr_lgl, x_int, x_lgl, "integer", 1e-5)
    
    check_add(proxy_int, 1.0, x_int, 1.0, "double", 1e-5)
    check_add(proxy_int, 1L, x_int, 1L, "integer", 1e-5)
    check_add(proxy_int, TRUE, x_int, TRUE, "integer", 1e-5)
    
    # filearrayproxy - Logical
    check_add(proxy_lgl, proxy_dbl, x_lgl, x_dbl, "double", 1e-5)
    check_add(proxy_lgl, proxy_int, x_lgl, x_int, "integer", 1e-5)
    check_add(proxy_lgl, proxy_lgl, x_lgl, x_lgl, "integer", 1e-5)
    
    check_add(proxy_lgl, arr_dbl, x_lgl, x_dbl, "double", 1e-5)
    check_add(proxy_lgl, arr_int, x_lgl, x_int, "integer", 1e-5)
    check_add(proxy_lgl, arr_lgl, x_lgl, x_lgl, "integer", 1e-5)
    
    check_add(proxy_lgl, 1.0, x_lgl, 1.0, "double", 1e-5)
    check_add(proxy_lgl, 1L, x_lgl, 1L, "integer", 1e-5)
    check_add(proxy_lgl, TRUE, x_lgl, TRUE, "integer", 1e-5)
    
    # filearray - Double
    check_add(arr_dbl, proxy_dbl, x_dbl, x_dbl, "double", 1e-5)
    check_add(arr_dbl, proxy_int, x_dbl, x_int, "double", 1e-5)
    check_add(arr_dbl, proxy_lgl, x_dbl, x_lgl, "double", 1e-5)
    
    check_add(arr_dbl, arr_dbl, x_dbl, x_dbl, "double", 1e-5)
    check_add(arr_dbl, arr_int, x_dbl, x_int, "double", 1e-5)
    check_add(arr_dbl, arr_lgl, x_dbl, x_lgl, "double", 1e-5)
    
    check_add(arr_dbl, 1.0, x_dbl, 1.0, "double", 1e-5)
    check_add(arr_dbl, 1L, x_dbl, 1L, "double", 1e-5)
    check_add(arr_dbl, TRUE, x_dbl, TRUE, "double", 1e-5)
    
    # filearray - Int
    check_add(arr_int, proxy_dbl, x_int, x_dbl, "double", 1e-5)
    check_add(arr_int, proxy_int, x_int, x_int, "integer", 1e-5)
    check_add(arr_int, proxy_lgl, x_int, x_lgl, "integer", 1e-5)
    
    check_add(arr_int, arr_dbl, x_int, x_dbl, "double", 1e-5)
    check_add(arr_int, arr_int, x_int, x_int, "integer", 1e-5)
    check_add(arr_int, arr_lgl, x_int, x_lgl, "integer", 1e-5)
    
    check_add(arr_int, 1.0, x_int, 1.0, "double", 1e-5)
    check_add(arr_int, 1L, x_int, 1L, "integer", 1e-5)
    check_add(arr_int, TRUE, x_int, TRUE, "integer", 1e-5)
    
    # filearray - Logical
    check_add(arr_lgl, proxy_dbl, x_lgl, x_dbl, "double", 1e-5)
    check_add(arr_lgl, proxy_int, x_lgl, x_int, "integer", 1e-5)
    check_add(arr_lgl, proxy_lgl, x_lgl, x_lgl, "integer", 1e-5)
    
    check_add(arr_lgl, arr_dbl, x_lgl, x_dbl, "double", 1e-5)
    check_add(arr_lgl, arr_int, x_lgl, x_int, "integer", 1e-5)
    check_add(arr_lgl, arr_lgl, x_lgl, x_lgl, "integer", 1e-5)
    
    check_add(arr_lgl, 1.0, x_lgl, 1.0, "double", 1e-5)
    check_add(arr_lgl, 1L, x_lgl, 1L, "integer", 1e-5)
    check_add(arr_lgl, TRUE, x_lgl, TRUE, "integer", 1e-5)
    
    
    # scalar - double
    check_add(1.0, proxy_dbl, 1.0, x_dbl, "double", 1e-5)
    check_add(1.0, proxy_int, 1.0, x_int, "double", 1e-5)
    check_add(1.0, proxy_lgl, 1.0, x_lgl, "double", 1e-5)
    
    check_add(1.0, arr_dbl, 1.0, x_dbl, "double", 1e-5)
    check_add(1.0, arr_int, 1.0, x_int, "double", 1e-5)
    check_add(1.0, arr_lgl, 1.0, x_lgl, "double", 1e-5)
    
    # scalar - int
    check_add(1L, proxy_dbl, 1L, x_dbl, "double", 1e-5)
    check_add(1L, proxy_int, 1L, x_int, "integer", 1e-5)
    check_add(1L, proxy_lgl, 1L, x_lgl, "integer", 1e-5)
    
    check_add(1L, arr_dbl, 1L, x_dbl, "double", 1e-5)
    check_add(1L, arr_int, 1L, x_int, "integer", 1e-5)
    check_add(1L, arr_lgl, 1L, x_lgl, "integer", 1e-5)
    
    # scalar - lgl
    check_add(FALSE, proxy_dbl, FALSE, x_dbl, "double", 1e-5)
    check_add(FALSE, proxy_int, FALSE, x_int, "integer", 1e-5)
    check_add(FALSE, proxy_lgl, FALSE, x_lgl, "integer", 1e-5)
    
    check_add(FALSE, arr_dbl, FALSE, x_dbl, "double", 1e-5)
    check_add(FALSE, arr_int, FALSE, x_int, "integer", 1e-5)
    check_add(FALSE, arr_lgl, FALSE, x_lgl, "integer", 1e-5)
    
    # wrong dimensions
    testthat::expect_error(arr_int + array(0L, c(10,20,1)))
    testthat::expect_error(arr_int + 1:10)
    tmp <- arr_int[]
    dm <- dim(arr_int)
    dm <- c(dm[1] * dm[2], dm[-c(1,2)])
    dim(tmp) <- dm
    tmp_arr <- as_filearray(tmp)
    testthat::expect_error( arr_int + tmp )
    testthat::expect_error( tmp + arr_int )
    testthat::expect_error( arr_int + tmp_arr )
    testthat::expect_error( tmp_arr + arr_int )
    testthat::expect_equal(
        (arr_int + 1:length(arr_int))[dimnames = NULL],
        arr_int[dimnames = NULL] + 1:length(arr_int)
    )
    testthat::expect_equal(
        (1:length(arr_int) + arr_int)[dimnames = NULL],
        1:length(arr_int) + arr_int[dimnames = NULL]
    )
    
})
