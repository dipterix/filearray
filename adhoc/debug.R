f <- function(seed){
    require(testthat)
    require(bit64)
    bsz <- get_buffer_size()
    on.exit({
        set_buffer_size(bsz)
        max_buffer_size(2097152)
    })
    set_buffer_size(16L)
    max_buffer_size(64L)
    
    set.seed(seed)
    file <- tempfile()
    unlink(file, recursive = TRUE)
    dim <- 33:35
    x <- filearray_create(file, dim, partition_size = 1, type = "double")
    
    expect_equal(x[[2]], x$.na)
    
    y <- array(x$.na, dim)
    tmp <- 1:(prod(dim))
    x[] <- tmp
    y[] <- tmp
    
    eps <- 10^(ceiling(log10(max(abs(y)))) - 7)
    # y[] <- x[]
    locs <-
        lapply(dim, function(d) {
            d1 <- sample(c(1,3:d), 10, replace = TRUE )
            d2 <- NULL#c(NA, NA)
            sort(as.double(sample(c(d1,d2))), decreasing = TRUE)
        })
    assign('locs', locs, envir = globalenv())
    
    a <- x[locs[[1]], locs[[2]], locs[[3]]]
    b <- y[locs[[1]], locs[[2]], locs[[3]]]
    assign('x', x, envir = globalenv())
    assign('y', y, envir = globalenv())
    eps <- 1e-6
    sel <- !is.na(a-b) & (a-b) > eps
    
    assign('sel', sel, envir = globalenv())
    assign('a', a, envir = globalenv())
    assign('b', b, envir = globalenv())
    
    expect_equal(is.na(a), is.na(b))
    
    
    if(length(a[sel])){
        # fail the test
        print(a[sel])
        print(b[sel])
        expect_length(object = (a-b)[sel], n = 0)
        stop()
    }
    
    unlink(file, recursive = TRUE)
}
# setThreads(1)
for(i in 1:1000){
    print(i)
    f(i)
}

