guess_partition <- function(dim, elem_size){
    last_margin <- dim[[length(dim)]]
    unit_size <- prod(dim) / last_margin * elem_size
    
    # 1: partition size cannot go beyond 1GB
    max_ <- floor(2^30 / unit_size)
    if(max_ <= 1L){
        return(1L)
    }
    # 2: n partitions <= 100
    if(last_margin <= 100){
        return(1L)
    }
    # 3: at most max_ units, at least fct units
    fct <- ceiling(last_margin / max_)
    if(fct > 50){
        return(max_)
    }
    while(fct <= 50){
        max_ <- max_ - 1L
        fct <- ceiling(last_margin / max_)
        if(max_ <= 1L){
            return(1L)
        }
    }
    return(max_)
}

#' @title Create or load existing file arrays
#' @name filearray
#' @author Zhengjia Wang
#' @param filebase a directory path to store arrays in the local file 
#' system. When creating an array, the path must not exist.
#' @param dimension dimension of the array, at least length of 2
#' @param type storage type of the array; default is \code{'double'}. Other
#' options include \code{'integer'}, \code{'logical'}, and \code{'raw'}.
#' @param mode whether allows writing to the file; choices are 
#' \code{'readwrite'} and \code{'readonly'}.
#' @param partition_size positive partition size for the last margin, or
#' \code{NA} to automatically guess; see 'Details'.
#' @return A \code{\link{FileArray-class}} instance.
#' 
#' @details The file arrays partition out-of-memory array objects and store them 
#' separately in local file systems. Since R stores matrices/arrays 
#' in column-major style, file array uses the slowest margin (the 
#' last margin) to slice the partitions. This helps to align the elements
#' within the files with the corresponding memory order. An array with 
#' dimension \code{100x200x300x400} has 4 margins. The length of the 
#' last margin is 400, which is also the maximum number of potential
#' partitions. The number of partitions are determined by the last margin
#' size divided by \code{partition_size}. For example, if the partition
#' size is 1, then there will be 400 partitions. If the partition size 
#' if 3, there will be 134 partitions. The default partition sizes 
#' are determined internally following these priorities:
#' \describe{
#' \item{1. }{the file size of each partition does not exceed \code{1GB}}
#' \item{2. }{the number of partitions do not exceed 100}
#' }
#' These two rules are not hard requirements. The goal is to reduce the
#' numbers of partitions as much as possible. 
#' 
#' @examples 
#' 
#' 
#' # Prepare 
#' library(filearray)
#' filebase <- tempfile()
#' if(file.exists(filebase)){ unlink(filebase, TRUE) }
#' 
#' # create array
#' x <- filearray_create(filebase, dimension = c(200, 30, 8))
#' print(x)
#' 
#' # Assign values
#' x[] <- rnorm(48000)
#' 
#' # Subset
#' x[1,2,]
#' 
#' # load existing array
#' filearray_load(filebase)
#' 
#' 
#' 
NULL

#' @rdname filearray
#' @export
filearray_create <- function(
    filebase, dimension, 
    type = c('double', 'float', 'integer', 'logical', 'raw', 'complex'), 
    partition_size = NA)
{
    type <- match.arg(type)
    
    if(length(dimension) < 2 || any(dimension == 0)){
        stop("Invalid dimension: FileArray dimension must not contain 0. Its length must be at least 2.")
    }
    
    size <- get_elem_size(type)
    if(is.na(partition_size)) {
        partition_size <- guess_partition(dimension, size)
    } else {
        partition_size <- round(partition_size)
        if (partition_size <= 0) {
            stop("Invalid `partition_size`: ", partition_size)
        }
    }
    
    arr <- new("FileArray")
    arr$create(
        filebase = filebase,
        dimension = dimension,
        type = type,
        partition_size = partition_size
    )
    arr
}

#' @rdname filearray
#' @export
filearray_load <- function(filebase, mode = c('readwrite', 'readonly')){
    mode <- match.arg(mode)
    arr <- new("FileArray")
    arr$load(
        filebase = filebase,
        mode = mode
    )
    arr
}

