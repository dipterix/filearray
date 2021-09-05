#' @title A map-reduce method to iterate blocks of file-array data with little memory usage
#' @param x a file array object
#' @param map mapping function that receives 3 arguments; see 'Details'
#' @param reduce \code{NULL}, or a function that takes a list as input
#' @param buffer_size control how we split the array; see 'Details'
#' @param ... passed to other methods
#' @return If \code{reduce} is \code{NULL}, return mapped results, otherwise
#' return reduced results from \code{reduce} function
#' 
#' @details When handling out-of-memory arrays, it is recommended to load
#' a block of array at a time and execute on block level. See 
#' \code{\link{apply}} for a implementation. When an array is too large,
#' and when there are too many blocks, this operation will become 
#' very slow if computer memory is low. 
#' This is because the R will perform garbage collection frequently. 
#' Implemented in \code{C++}, \code{mapreduce} creates a buffer to store
#' the block data. By reusing the memory over and over again, it is possible
#' to iterate through the array with minimal garbage collections. Many 
#' statistics, including \code{min}, \code{max}, \code{sum}, 
#' \code{mean}, ... These statistics can be calculated in this 
#' way efficiently.
#' 
#' The function \code{map} contains three arguments: \code{data} (mandate), 
#' \code{size} (optional), and \code{first_index} (optional). 
#' The \code{data} is the buffer,
#' whose length is consistent across iterations. \code{size} indicates
#' the effective size of the buffer. If the partition size
#' is not divisible by the buffer size, only first \code{size} elements of
#' the data are from array, and the rest elements will be \code{NA}. 
#' This situation could only occurs when \code{buffer_size} is manually 
#' specified. By default, all of \code{data} should belong to arrays.
#' The last argument \code{first_index} is the index of the first element
#' \code{data[1]} in the whole array. It is useful when positional data 
#' is needed. 
#' 
#' The buffer size, specified by \code{buffer_size} is an 
#' additional optional argument in \code{...}. Its default is \code{NA},
#' and will be calculated automatically. If manually specified, a
#' large buffer size would be desired to speed up the calculation.
#' The default buffer size will not exceed \eqn{nThreads x 2MB}, where 
#' \code{nThreads} is the number of threads set by \code{\link{filearray_threads}}.
#' When partition length cannot be divided by the buffer size, instead of
#' trimming the buffer, \code{NA}s will be filled to the buffer, 
#' passed to \code{map} function; see previous paragraph for treatments.
#' 
#' The function \code{mapreduce} ignores the missing partitions. That means
#' if a partition is missing, its data will not be read nor passed to 
#' \code{map} function. Please run \code{x$initialize_partition()} to make sure
#' partition files exist.
#' 
#' @examples 
#' 
#' 
#' x <- filearray_create(tempfile(), c(100, 100, 10))
#' x[] <- rnorm(1e5)
#' 
#' ## calculate summation
#' # identical to sum(x[]), but is more feasible in large cases
#' 
#' mapreduce(x, map = function(data, size){
#'     # make sure `data` is all from array
#'     if(length(data) != size){
#'         data <- data[1:size]
#'     }
#'     sum(data)
#' }, reduce = function(mapped_list){
#'     do.call(sum, mapped_list)
#' })
#' 
#' 
#' ## Find elements are less than -3
#' positions <- mapreduce(
#'     x,
#'     map = function(data, size, first_index) {
#'         if (length(data) != size) {
#'             data <- data[1:size]
#'         }
#'         which(data < -3) + (first_index - 1)
#'     },
#'     reduce = function(mapped_list) {
#'         do.call(c, mapped_list)
#'     }
#' )
#' 
#' if(length(positions)){
#'     x[[positions[1]]]
#' }
#' 
#' 
#' @export
setGeneric("mapreduce", function(x, map, reduce, ...){
    standardGeneric("mapreduce")
})

buffer_mapreduce <- function(x, map, reduce = NULL, buffer_size = NA){
    if(!x$valid()){
        stop("Invalid file array")
    }
    
    filebase <- paste0(x$.filebase, x$.sep)
    set_buffer_size(max_buffer_size())
    
    argnames <- names(formals(map))
    if(length(argnames) < 3 && !'...' %in% argnames){
        if( length(argnames) == 1 ){
            map_ <- function(data, size, idx){
                map(data)
            }
        } else {
            map_ <- function(data, size, idx){
                map(data, size)
            }
        }
    } else {
        map_ <- function(data, size, idx){
            map(data, size, idx)
        }
    }
    
    if(is.function(reduce) && !length(formals(reduce))){
        stop("Reduce function must contain at least one argument")
    }
    
    dim <- x$dimension()
    cum_partlen <- x$.partition_info[,3]
    sexp_type <- x$sexp_type()
    
    if(is.na(buffer_size)){
        elem_size <- get_elem_size(x$type())
        mbsz <- max_buffer_size() * getThreads() / elem_size
        
        sel <- cumprod(dim) <= mbsz
        if(any(sel)){
            buffer_size <- prod(dim[sel])
        } else {
            buffer_size <- dim[[1]]
        }
    }
    
    if(buffer_size < 1){
        buffer_size <- 1
    }
    
    FARR_buffer_mapreduce(filebase, map_, reduce, dim, 
                          cum_partlen, buffer_size, sexp_type)
}

#' @rdname mapreduce
#' @export
setMethod(
    mapreduce, signature(x = "FileArray", reduce = "function"),
    function(x, map, reduce, buffer_size = NA, ...){
        buffer_mapreduce(x, map, reduce, buffer_size)
    }
)

#' @rdname mapreduce
#' @export
setMethod(
    mapreduce, signature(x = "FileArray", reduce = "NULL"),
    function(x, map, reduce, buffer_size = NA, ...){
        buffer_mapreduce(x, map, NULL, buffer_size, ...)
    }
)

#' @rdname mapreduce
#' @export
setMethod(
    mapreduce, signature(x = "FileArray", reduce = "missing"),
    function(x, map, reduce, buffer_size = NA, ...){
        buffer_mapreduce(x, map, NULL, buffer_size, ...)
    }
)
