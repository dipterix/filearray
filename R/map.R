#' @title Map multiple file arrays and save results
#' @description Advanced mapping function for multiple file arrays. \code{fmap}
#' runs the mapping functions and stores the results in file arrays. 
#' \code{fmap2} stores results in memory. This 
#' feature is experimental. There are several constraints to the input. 
#' Failure to meet these constraints may result in undefined results, or 
#' even crashes. Please read Section 'Details' carefully before using 
#' this function.
#' @param x a list of file arrays to map; each element of \code{x} must 
#' share the same dimensions. 
#' @param fun function that takes one list
#' @param .y a file array object, used to save results
#' @param .input_size number of elements to read from each array of \code{x}
#' @param .output_size \code{fun} output vector length
#' @param .simplify whether to apply \code{\link[base]{simplify2array}} to 
#' the result
#' @param ... other arguments passing to \code{fun}
#' @return File array instance \code{.y}
#' @details 
#' 
#' Denote the first argument of \code{fun} as \code{input}, The length 
#' of \code{input} equals the length of \code{x}. The size of each
#' element of \code{input} is defined by \code{.input_size}, except for the
#' last loop. For example, given dimension of each input array as 
#' \eqn{10x10x10x10}, if \code{.input_size=100}, then 
#' \code{length(input[[1]])=100}. The total number of runs equals to
#' \code{length(x[[1]])/100}. If \code{.input_size=300}, then 
#' \code{length(input[[1]])} will be \code{300} except for the last run. 
#' This is because \eqn{10000} cannot be divided by \code{300}. 
#' The element length of the last run will be \code{100}. 
#' 
#' The returned variable length of \code{fun} will be checked by 
#' \code{.output_size}. If the output length exceed \code{.output_size}, 
#' an error will be raised.
#' 
#' Please make sure that \code{length(.y)/length(x[[1]])} equals to 
#' \code{.output_size/.input_size}.
#' 
#' For \code{fmap_element_wise}, the \code{input[[1]]} and output length 
#' must be the consistent.
#' 
#' @examples 
#' 
#' 
#' set.seed(1)
#' x1 <- filearray_create(tempfile(), dimension = c(100,20,3))
#' x1[] <- rnorm(6000)
#' x2 <- filearray_create(tempfile(), dimension = c(100,20,3))
#' x2[] <- rnorm(6000)
#' 
#' # Add two arrays
#' output <- filearray_create(tempfile(), dimension = c(100,20,3))
#' fmap(list(x1, x2), function(input){
#'     input[[1]] + input[[2]]
#' }, output)
#' 
#' # check
#' range(output[] - (x1[] + x2[]))
#' 
#' output$delete()
#' 
#' # Calculate the maximum of x1/x2 for every 100 elements
#' output <- filearray_create(tempfile(), dimension = c(20,3))
#' fmap(list(x1, x2), function(input){
#'     max(input[[1]] / input[[2]])
#' }, output, .input_size = 100, .output_size = 1)
#' 
#' # check
#' range(output[] - apply(x1[] / x2[], c(2,3), max))
#' 
#' output$delete()
#' 
#' # A large array example
#' if(interactive()){
#'     x <- filearray_create(tempfile(), dimension = c(287, 100, 301, 4))
#'     dimnames(x) <- list(
#'         Trial = 1:287,
#'         Marker = 1:100,
#'         Time = 1:301,
#'         Location = 1:4
#'     )
#'     
#'     for(i in 1:4){
#'         x[,,,i] <- runif(8638700)
#'     }
#'     # Step 1:
#'     # for each location, trial, and marker, calibrate (baseline)
#'     # according to first 50 time-points
#'     
#'     output <- filearray_create(tempfile(), dimension = dim(x))
#'     
#'     # baseline-percentage change
#'     fmap(
#'         list(x), 
#'         function(input){
#'             # get locational data
#'             location_data <- input[[1]]
#'             dim(location_data) <- c(287, 100, 301)
#'             
#'             # collapse over first 50 time points for 
#'             # each trial, and marker
#'             baseline <- apply(location_data[,,1:50], c(1,2), mean)
#'             
#'             # calibrate
#'             calibrated <- sweep(location_data, c(1,2), baseline, 
#'                                 FUN = function(data, bl){
#'                                     (data / bl - 1) * 100
#'                                 })
#'             return(calibrated)
#'         }, 
#'         
#'         .y = output,
#'         
#'         # input dimension is 287 x 100 x 301 for each location
#'         .input_size = 8638700,
#'         
#'         # output dimension is 287 x 100 x 301
#'         .output_size = 8638700
#'     )
#'     
#'     # cleanup
#'     x$delete()
#'     
#' }
#' 
#' # cleanup
#' x1$delete()
#' x2$delete()
#' output$delete()
#' 
#' @export
fmap <- function(x, fun, .y, .input_size = NA, .output_size = NA, ...){
    if(!length(x)){
        stop("`x` must be a list of file arrays")
    }
    
    if(inherits(x, "FileArray")){
        x <- list(x)
    }
    
    dims <- sapply(x, dim)
    dim <- dims[,1, drop = TRUE]
    
    if(any(dims - dim != 0)){
        stop("Input `x` array dimensions must match")
    }
    
    fbases <- sapply(x, function(el){
        if( !is_filearray(el) ){
            stop("Input `x` must only contains file arrays")
        }
        el$initialize_partition()
        el$.filebase
    })
    
    if(missing(.y)){
        .y <- filearray_create(tempfile(), dim)
    } else {
        stopifnot(is_filearray(.y))
    }
    .y$initialize_partition()
    
    if(is.na(.input_size)){
        .input_size <- get_buffer_size() / .y$element_size()
    }
    if(is.na(.output_size)){
        .output_size <- 0L
    }
    .output_size <- as.integer(.output_size)
    if(.input_size <= 0){
        stop("`.input_size` must be postive")
    }
    .input_size <- as.integer(.input_size)
    if(.output_size < 0){
        stop("`.output_size` must be non-negative")
    }
    
    args <- list(quote(input), ...)
    map <- function(input){
        do.call(fun, args)
    }
    
    if(.output_size / .input_size * length(x[[1]]) > length(.y) ){
        stop("Inconsistent input and output length")
    }
    
    FARR_buffer_map(
        input_filebases = fbases,
        output_filebase = .y$.filebase,
        map = map,
        buffer_nelems = .input_size,
        result_nelems = .output_size
    )
    .y
}

#' @rdname fmap
#' @export
fmap2 <- function(x, fun, .input_size = NA, .simplify = TRUE, ...){
    if(!length(x)){
        stop("`x` must be a list of file arrays")
    }
    
    if(inherits(x, "FileArray")){
        x <- list(x)
    }
    
    dims <- sapply(x, dim)
    dim <- dims[,1, drop = TRUE]
    
    if(any(dims - dim != 0)){
        stop("Input `x` array dimensions must match")
    }
    
    fbases <- sapply(x, function(el){
        if( !is_filearray(el) ){
            stop("Input `x` must only contains file arrays")
        }
        el$initialize_partition()
        el$.filebase
    })
    
    if(is.na(.input_size)){
        .input_size <- get_buffer_size() / 8L
    }
    if(.input_size <= 0){
        stop("`.input_size` must be postive")
    }
    .input_size <- as.integer(.input_size)
    
    args <- list(quote(input), ...)
    map <- function(input){
        do.call(fun, args)
    }
    
    res <- FARR_buffer_map2(
        input_filebases = fbases,
        map = map,
        buffer_nelems = .input_size
    )
    if(.simplify){
        res <- simplify2array(res)
    }
    res
}

is_filearray <- function(object){
    if(!isS4(object)){ return(FALSE) }
    cls <- class(object)
    if(!"FileArray" %in% cls){ return(FALSE) }
    return(TRUE)
}

#' @rdname fmap
#' @export
fmap_element_wise <- function(x, fun, .y, ..., .input_size = NA){
    if(!is.list(x)){
        if(!is_filearray(x)){ stop("`x` must be a list of file arrays") }
        x <- list(x)
    } else {
        lapply(x, function(x){
            if(!is_filearray(x)){ stop("`x` must be a list of file arrays") }
        })
    }
    if(!length(x)){ stop("`x` length must be positive") }
    dims <- sapply(x, dim)
    dim <- dims[,1, drop = TRUE]
    
    if(any(dims - dim != 0)){
        stop("Input `x` array dimensions must match")
    }
    
    miss_y <- missing(.y)
    if(miss_y){
        .y <- filearray_create(tempfile(), dim, type = x[[1]]$type())
    } else {
        if(length(dim) != length(.y$dimension()) ||
           any(.y$dimension() - dim != 0)){
            stop("Dimensions of x[[1]] and .y mismatch")
        }
    }
    .y$initialize_partition()
    
    if(is.na(.input_size)){
        .input_size <- get_buffer_size() / .y$element_size()
    }
    if(.input_size <= 0){
        stop("`.input_size` must be postive")
    }
    .input_size <- as.integer(.input_size)
    
    fbases <- sapply(x, function(el){
        if( !inherits(el, "FileArray") ){
            stop("Input `x` must only contains file arrays")
        }
        el$initialize_partition()
        el$.filebase
    })
    
    args <- list(quote(input), ...)
    map <- function(input){
        do.call(fun, args)
    }
    
    if(length(x[[1]]) > length(.y) ){
        stop("Inconsistent input and output length")
    }
    
    FARR_buffer_map(
        input_filebases = fbases,
        output_filebase = .y$.filebase,
        map = map,
        buffer_nelems = .input_size,
        result_nelems = .input_size,
        ...
    )
    # fmap(xs, fun = fun, .y = .y, .input_size = buf_size, .output_size = buf_size, ...)
    if(miss_y){
        return(.y)
    } else {
        return(invisible(.y))
    }
}
