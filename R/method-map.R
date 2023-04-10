#' @name fmap
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
NULL

# DIPSAUS DEBUG START
# x1 <- filearray_create(tempfile(), dimension = c(2,3,12), partition_size = 5L)
# x1[] <- rnorm(length(x1))
# x2 <- filearray_create(tempfile(), dimension = c(4,3,4), partition_size = 3L)
# x2[] <- rnorm(length(x2))
# # Add two arrays
# y <- filearray_create(tempfile(), dimension = c(3,4))
# x <- list(x1, x2)
# .input_size <- 12
# fun <- function(input){
#    # print(input)
#     colSums(matrix(input[[1]], nrow = 6)) + colSums(matrix(input[[2]], nrow = 4))
# }
# y <- filearray_map_internal(x, fun, y)
# # print(y[] - (colSums(matrix(x1[], nrow = 6)) + colSums(matrix(x2[], nrow = 4))))
# print(filearray_map_internal(x, fun, output_is_filearray = FALSE))


filearray_map_internal <- function(x, fun, y = NULL, .input_size = NA_integer_, 
                                   output_is_filearray = TRUE, .simplify = FALSE, ...) {
    if(!length(x)){
        stop("`x` must be a list of file arrays")
    }
    if(is_filearray(x)) {
        x <- list(x)
    }
    
    x <- lapply(x, function(arr) {
        if(!is_filearray(arr)) {
            return(as_filearray(arr))
        }
        fa_eval_ops(arr)
    })
    
    fbases <- vapply(x, function(el){
        el$initialize_partition()
        el$.filebase
    }, "")
    input_types <- vapply(x, typeof, "")
    output_type <- Reduce(operation_output_type, input_types)
    
    if( output_is_filearray ) {
        if(missing(y) || is.null(y)) {
            y <- filearray_create(filebase = temp_path(check = TRUE), dimension = dim(x[[1]]), type = output_type)
        } else {
            if(!is_filearray(y)) {
                stop("Output `y` must be a filearray or a proxy array")
            }
        }
        
        all_lens <- c(vapply(x, length, 0L), length(y))
        
        if(!validate_fmap_buffer_size(.input_size, all_lens)) {
            .input_size <- common_fmap_buffer_size(dim(y), .list = lapply(x, dim))
        }
        buffer_size_is_valid <- validate_fmap_buffer_size(.input_size, all_lens)
        if(!buffer_size_is_valid) {
            stop("Input & output arrays have inconsistent lengths. Cannot map functions")
        }
        buffer_nelems <- attr(buffer_size_is_valid, "buffer_nelems")
        
        # prepare
        args <- list(quote(input), ...)
        mapping_func <- function(input){
            do.call(fun, args)
        }
        y$initialize_partition()
        
        FARR_buffer_map(
            input_filebases = fbases,
            output_filebase = y$.filebase,
            map = mapping_func,
            buffer_nelems = buffer_nelems[-length(buffer_nelems)],
            result_nelems = buffer_nelems[[length(buffer_nelems)]]
        )
        return(y)
    } else {
        all_lens <- vapply(x, length, 0L)
        
        if(!validate_fmap_buffer_size(.input_size, all_lens)) {
            .input_size <- common_fmap_buffer_size(.list = lapply(x, dim))
        }
        buffer_size_is_valid <- validate_fmap_buffer_size(.input_size, all_lens)
        if(!buffer_size_is_valid) {
            stop("Input & output arrays have inconsistent lengths. Cannot map functions")
        }
        buffer_nelems <- attr(buffer_size_is_valid, "buffer_nelems")
        
        # prepare
        args <- list(quote(input), ...)
        mapping_func <- function(input){
            do.call(fun, args)
        }
        
        res <- FARR_buffer_map2(
            input_filebases = fbases,
            map = mapping_func,
            buffer_nelems = buffer_nelems
        )
        if(.simplify){
            res <- simplify2array(res)
        }
        return(res)
    }
    
    
}


#' @rdname fmap
#' @export
fmap <- function(x, fun, .y = NULL, .input_size = NA_integer_, .output_size = NA_integer_, ...){
    
    # TODO mature arrays, ignore output size
    
    filearray_map_internal(x = x, fun = fun, y = .y, .input_size = .input_size, ...)
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
        # .input_size <- get_buffer_size() / 8L
        .input_size <- guess_fmap_buffer_size(dim(x[[1]]))
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

# only use values on hard disk
fmap_element_wise_internal <- function(x, fun, .y, ..., .input_size = NA){
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
    
    miss_y <- missing(.y) || is.null(.y)
    if(miss_y){
        .y <- filearray_create(temp_path(), dim, type = typeof(x[[1]]))
    } else {
        if(length(dim) != length(.y$dimension()) ||
           any(.y$dimension() - dim != 0)){
            stop("Dimensions of x[[1]] and .y mismatch")
        }
    }
    .y$initialize_partition()
    
    if(is.na(.input_size)){
        .input_size <- guess_fmap_buffer_size(dim(.y), .y$element_size())
        # .input_size <- get_buffer_size() / .y$element_size()
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
        buffer_nelems = rep(.input_size, length(x)),
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

#' @rdname fmap
#' @export
fmap_element_wise <- function(x, fun, .y, ..., .input_size = NA) {
    if(!is_fileproxy(x)) {
        return(fmap_element_wise_internal(x = x, fun = fun, .y = .y, ..., .input_size = .input_size))
    } else{
        # proxy!
        if(missing(.y) || is.null(.y)) {
            filebase <- NULL
        } else {
            filebase <- .y$.filebase
        }
        fa_eval_ops(
            x = x, filebase = filebase, use_cache = FALSE,
            addon = function(env, dlist, uuid) {
                env[[ uuid ]] <- fun(dlist)
            }
        )
        
    }
}
