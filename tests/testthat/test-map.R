library(testthat)
test_that("map arrays", {
    
    set.seed(10)
    path <- tempfile()
    unlink(path, recursive = TRUE)
    
    # A large array example
    x <- filearray_create(path, dimension = c(28, 100, 301, 4))
    dnames <- list(
        Trial = sample(c("A", "B"), 28, replace = TRUE),
        Marker = 1:100,
        Time = seq(-1,2,0.01),
        Location = 1:4
    )
    dimnames(x) <- dnames
    
    expect_equal(dimnames(x), dnames)
    
    y <- array(rnorm(length(x)), dim(x))
    x[] <- y
    
    output <- filearray_create(tempfile(), dimension = dim(x))
    
    f <- function(input){
        # get locational data
        if(is.list(input)){
            location_data <- input[[1]]
        } else {
            location_data <- input
        }
        
        dim(location_data) <- c(28, 100, 301)
        
        # collapse over first 50 time points for
        # each trial, and marker
        baseline <- apply(location_data[,,1:50], c(1,2), mean)
        
        # calibrate
        calibrated <- sweep(location_data, c(1,2), baseline,
                            FUN = function(data, bl){
                                (data / bl - 1) * 100
                            })
        return(calibrated)
    }
    
    fmap(x, f, .y = output, .input_size = 842800, .output_size = 842800)
    
    b <- apply(y, 4, f)
    dim(b) <- dim(y)
    
    expect_equal(output[], b)
    
    
    d <- fmap2(x, f, .input_size = 842800, .simplify = TRUE)
    expect_equal(d, b)
    
    x$delete()
    output$delete()
    
})



