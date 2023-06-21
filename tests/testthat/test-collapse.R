
# Testing collapse is time consuming, skip if ran
skip_collapse <- Sys.getenv("FILEARRAY_SKIP_COLLAPSE", unset = "") == "TRUE"
testthat::skip_if(skip_collapse)

collapse_real <- function(y, keep, transform = c("asis", "10log10", "square", "sqrt", "normalize")){
    re <- switch (
        transform,
        'asis' = {
            apply(y, keep, function(x){
                mean(x)
            })
        },
        '10log10' = {
            apply(y, keep, function(x){
                mean(10* log10(x))
            })
        },
        'square' = {
            apply(y, keep, function(x){
                mean(x^2)
            })
        },
        'sqrt' = {
            apply(y, keep, function(x){
                mean(sqrt(x))
            })
        }, {
            stop("wrong transform")
        }
    )
    # if(storage.mode(re) != "double"){
    #     storage.mode(re) <- 'double'
    # }
    re
}

collapse_cplx <- function(y, keep, transform = c("asis", "10log10", "square", "sqrt", "normalize")){
    re <- switch (
        transform,
        'asis' = {
            apply(y, keep, mean)
        },
        '10log10' = {
            apply(y, keep, function(x){
                mean(20 * log10(Mod(x)))
            })
        },
        'square' = {
            apply(y, keep, function(x){
                mean(Mod(x)^2)
            })
        },
        'sqrt' = {
            apply(y, keep, function(x){
                mean(Mod(x))
            })
        },
        'normalize' = {
            apply(y, keep, function(x){
                mean(x / Mod(x))
            })
        },
        {
            stop("wrong transform")
        }
    )
    # if(storage.mode(re) != "double"){
    #     storage.mode(re) <- 'double'
    # }
    re
}

expect_equivalent_cplx <- function(x, y, eps = 1e-6){
    expect_equal(is.na(x), is.na(y))
    if(is.complex(x)){
        expect_lte(max(abs(Re(x - y)), na.rm = TRUE), eps)
        expect_lte(max(abs(Im(x - y)), na.rm = TRUE), eps)
    } else {
        expect_lte(max(abs(x - y), na.rm = TRUE), eps)
    }
}

test_that("R/C++ - Collapse", {
    testthat::skip_on_cran()
    bsz <- get_buffer_size()
    on.exit({
        set_buffer_size(bsz)
        max_buffer_size(2097152)
    })
    set_buffer_size(16L)
    max_buffer_size(64L)

    # dim <- c(287, 100, 301, 7)
    dim <- c(33:36)
    set.seed(5)
    file <- tempfile()
    unlink(file, recursive = TRUE)
    x <- filearray_create(file, dim, type = "integer", partition_size = 2, initialize = FALSE)
    y <- array(1:(prod(dim)), dim)
    y[[20, 3, 3, 3]] <- NA
    storage.mode(y) <- "integer"
    x[] <- y

    # make sure x[] == y
    expect_equal(x[], y)



    # collapse
    keep <- c(1,2,3,4)
    for(transform in c("asis", "10log10", "square", "sqrt")){
        expect_equal(
            x$collapse(keep = keep, transform = transform, method = 'mean'),
            collapse_real(y, keep, transform = transform)
        )
    }

    keep <- c(1,4,3,2)
    expect_equal(
        x$collapse(keep = keep, transform = 'asis', method = 'mean'),
        collapse_real(y, keep, transform = 'asis')
    )

    keep <- c(4,2,3,1)
    expect_equal(
        x$collapse(keep = keep, transform = 'asis', method = 'mean'),
        collapse_real(y, keep, transform = 'asis')
    )

    keep <- c(3,1)
    for(transform in c("asis", "10log10", "square", "sqrt")){
        expect_equal(
            x$collapse(keep = keep, transform = transform, method = 'mean'),
            collapse_real(y, keep, transform = transform)
        )
    }

    keep <- c(4,1)
    for(transform in c("asis", "10log10", "square", "sqrt")){
        expect_equal(
            x$collapse(keep = keep, transform = transform, method = 'mean'),
            collapse_real(y, keep, transform = transform)
        )
    }

    keep <- c(4,2)
    for(transform in c("asis", "10log10", "square", "sqrt")){
        expect_equal(
            x$collapse(keep = keep, transform = transform, method = 'mean'),
            collapse_real(y, keep, transform = transform)
        )
    }
    keep <- c(4,3)
    for(transform in c("asis", "10log10", "square", "sqrt")){
        expect_equal(
            x$collapse(keep = keep, transform = transform, method = 'mean'),
            collapse_real(y, keep, transform = transform)
        )
    }
    keep <- c(4)
    for(transform in c("asis", "10log10", "square", "sqrt")){
        k <- x$collapse(keep = keep, transform = transform, method = 'mean')
        s <- collapse_real(y, keep, transform = transform)
        diff <- max(abs(1-s / k), na.rm = TRUE)
        # cat(transform, diff, "\n")
        expect_lt(diff, 1e-6)
    }
    keep <- c(4,1,3)
    for(transform in c("asis", "10log10", "square", "sqrt")){
        expect_equal(
            x$collapse(keep = keep, transform = transform, method = 'mean'),
            collapse_real(y, keep, transform = transform)
        )
    }
    keep <- c(3)
    for(transform in c("asis", "10log10", "square", "sqrt")){
        k <- x$collapse(keep = keep, transform = transform, method = 'mean')
        s <- collapse_real(y, keep, transform = transform)
        diff <- max(abs(1-s / k), na.rm = TRUE)
        # cat(transform, diff, "\n")
        expect_lt(diff, 1e-6)
    }
    keep <- c(1)
    for(transform in c("asis", "10log10", "square", "sqrt")){
        k <- x$collapse(keep = keep, transform = transform, method = 'mean')
        s <- collapse_real(y, keep, transform = transform)
        diff <- max(abs(1-s / k), na.rm = TRUE)
        # cat(transform, diff, "\n")
        expect_lt(diff, 1e-6)
    }

})

test_that("R/C++ - Float", {
    testthat::skip_on_cran()
    bsz <- get_buffer_size()
    on.exit({
        set_buffer_size(bsz)
        max_buffer_size(2097152)
    })
    set_buffer_size(16L)
    max_buffer_size(64L)

    # dim <- c(287, 100, 301, 7)
    dim <- c(33:36)
    set.seed(5)
    file <- tempfile()
    unlink(file, recursive = TRUE)
    x <- filearray_create(file, dim, type = "float", partition_size = 2, initialize = FALSE)
    y <- array(rnorm(length(x))^2, dim)
    y[[20, 3, 3, 3]] <- NA
    x[] <- y

    # make sure x[] == y
    eps <- 10^(ceiling(log10(max(abs(y), na.rm = TRUE))) - 7)
    expect_equal(x[], y, tolerance = eps)
    y <- x[]



    # collapse
    keep <- c(1,2,3,4)
    for(transform in c("asis", "10log10", "square", "sqrt")){
        expect_equal(
            x$collapse(keep = keep, transform = transform, method = 'mean'),
            collapse_real(y, keep, transform = transform)
        )
    }

    keep <- c(1,4,3,2)
    expect_equal(
        x$collapse(keep = keep, transform = 'asis', method = 'mean'),
        collapse_real(y, keep, transform = 'asis')
    )

    keep <- c(4,2,3,1)
    expect_equal(
        x$collapse(keep = keep, transform = 'asis', method = 'mean'),
        collapse_real(y, keep, transform = 'asis')
    )

    keep <- c(3,1)
    for(transform in c("asis", "10log10", "square", "sqrt")){
        expect_equal(
            x$collapse(keep = keep, transform = transform, method = 'mean'),
            collapse_real(y, keep, transform = transform)
        )
    }

    keep <- c(4,1)
    for(transform in c("asis", "10log10", "square", "sqrt")){
        expect_equal(
            x$collapse(keep = keep, transform = transform, method = 'mean'),
            collapse_real(y, keep, transform = transform)
        )
    }

    keep <- c(4,2)
    for(transform in c("asis", "10log10", "square", "sqrt")){
        expect_equal(
            x$collapse(keep = keep, transform = transform, method = 'mean'),
            collapse_real(y, keep, transform = transform)
        )
    }
    keep <- c(4,3)
    for(transform in c("asis", "10log10", "square", "sqrt")){
        expect_equal(
            x$collapse(keep = keep, transform = transform, method = 'mean'),
            collapse_real(y, keep, transform = transform)
        )
    }
    keep <- c(4)
    for(transform in c("asis", "10log10", "square", "sqrt")){
        k <- x$collapse(keep = keep, transform = transform, method = 'mean')
        s <- collapse_real(y, keep, transform = transform)
        diff <- max(abs(1-s / k), na.rm = TRUE)
        # cat(transform, diff, "\n")
        expect_lt(diff, 1e-6)
    }
    keep <- c(4,1,3)
    for(transform in c("asis", "10log10", "square", "sqrt")){
        expect_equal(
            x$collapse(keep = keep, transform = transform, method = 'mean'),
            collapse_real(y, keep, transform = transform)
        )
    }
    keep <- c(3)
    for(transform in c("asis", "10log10", "square", "sqrt")){
        k <- x$collapse(keep = keep, transform = transform, method = 'mean')
        s <- collapse_real(y, keep, transform = transform)
        diff <- max(abs(1-s / k), na.rm = TRUE)
        # cat(transform, diff, "\n")
        expect_lt(diff, 1e-6)
    }
    keep <- c(1)
    for(transform in c("asis", "10log10", "square", "sqrt")){
        k <- x$collapse(keep = keep, transform = transform, method = 'mean')
        s <- collapse_real(y, keep, transform = transform)
        diff <- max(abs(1-s / k), na.rm = TRUE)
        # cat(transform, diff, "\n")
        expect_lt(diff, 1e-6)
    }

})

test_that("R/C++ - Collapse (complex)", {
    testthat::skip_on_cran()
    bsz <- get_buffer_size()
    on.exit({
        set_buffer_size(bsz)
        max_buffer_size(2097152)
    })
    set_buffer_size(16L)
    max_buffer_size(64L)

    # dim <- c(287, 100, 301, 7)
    dim <- c(33:36)
    set.seed(5)
    file <- tempfile()
    unlink(file, recursive = TRUE)
    x <- filearray_create(file, dim, type = "complex", partition_size = 2, initialize = FALSE)
    y <- array(rnorm(length(x)) + rnorm(length(x)) * 1i, dim)
    y[[20, 3, 3, 3]] <- NA
    x[] <- y

    # make sure x[] == y
    expect_equivalent_cplx(x[], y)

    y <- x[]

    # collapse
    keep <- c(1,2,3,4)
    for(transform in c("asis", "10log10", "square", "sqrt", "normalize")){
        expect_equal(
            x$collapse(keep = keep, transform = transform, method = 'mean'),
            collapse_cplx(y, keep, transform = transform)
        )
    }

    keep <- c(1,4,3,2)
    expect_equal(
        x$collapse(keep = keep, transform = 'asis', method = 'mean'),
        collapse_cplx(y, keep, transform = 'asis')
    )

    keep <- c(4,2,3,1)
    expect_equal(
        x$collapse(keep = keep, transform = 'asis', method = 'mean'),
        collapse_cplx(y, keep, transform = 'asis')
    )

    keep <- c(3,1)
    for(transform in c("asis", "10log10", "square", "sqrt", "normalize")){
        expect_equal(
            x$collapse(keep = keep, transform = transform, method = 'mean'),
            collapse_cplx(y, keep, transform = transform)
        )
    }

    keep <- c(4,1)
    for(transform in c("asis", "10log10", "square", "sqrt", "normalize")){
        expect_equal(
            x$collapse(keep = keep, transform = transform, method = 'mean'),
            collapse_cplx(y, keep, transform = transform)
        )
    }

    keep <- c(4,2)
    for(transform in c("asis", "10log10", "square", "sqrt", "normalize")){
        expect_equal(
            x$collapse(keep = keep, transform = transform, method = 'mean'),
            collapse_cplx(y, keep, transform = transform)
        )
    }
    keep <- c(4,3)
    for(transform in c("asis", "10log10", "square", "sqrt", "normalize")){
        expect_equal(
            x$collapse(keep = keep, transform = transform, method = 'mean'),
            collapse_cplx(y, keep, transform = transform)
        )
    }
    keep <- c(4)
    for(transform in c("asis", "10log10", "square", "sqrt", "normalize")){
        expect_equal(
            max(abs(x$collapse(keep = keep, transform = transform, method = 'mean')-
                        collapse_cplx(y, keep, transform = transform)), na.rm = TRUE),
            0
        )
    }
    keep <- c(4,1,3)
    for(transform in c("asis", "10log10", "square", "sqrt", "normalize")){
        expect_equal(
            x$collapse(keep = keep, transform = transform, method = 'mean'),
            collapse_cplx(y, keep, transform = transform)
        )
    }
    keep <- c(3)
    for(transform in c("asis", "10log10", "square", "sqrt", "normalize")){
        expect_equal(
            max(abs(x$collapse(keep = keep, transform = transform, method = 'mean')-
                        collapse_cplx(y, keep, transform = transform)), na.rm = TRUE),
            0
        )
    }
    keep <- c(1)
    for(transform in c("asis", "10log10", "square", "sqrt", "normalize")){
        expect_equal(
            max(abs(x$collapse(keep = keep, transform = transform, method = 'mean')-
                        collapse_cplx(y, keep, transform = transform)), na.rm = TRUE),
            0
        )
    }

})


Sys.setenv("FILEARRAY_SKIP_COLLAPSE" = "TRUE")
