#' @title Merge and bind homogeneous file arrays
#' @description The file arrays to be merged must be homogeneous:
#' same data type, partition size, and partition length
#' @param ...,.list file array instances
#' @param filebase where to create merged array
#' @param symlink whether to use \code{\link[base]{file.symlink}}; if true,
#' then partition files will be symbolic-linked to the original arrays,
#' otherwise the partition files will be copied over
#' @details The input arrays must share the same data type and partition size.
#' The dimension for each partition should also be the same. For example
#' an array \code{x1} has dimension \eqn{100x20x30} with partition size 
#' \code{1}, then each partition dimension is \eqn{100x20x1}, and there are 
#' \code{30} partitions. \code{x1} can bind with another array of the same 
#' partition size. This means if \code{x2} has dimension
#' \eqn{100x20x40} and each partition size is \code{1}, then \code{x1} and 
#' \code{x2} can be merged.
#' 
#' The \code{symlink} option should be used with caution. Creating symbolic
#' links is definitely faster than copying partition files. However, since 
#' the partition files are simply linked to the original partition files, 
#' changing to the input arrays will also affect the merged arrays, and 
#' vice versa; see 'Examples'. Also for arrays created from symbolic links, if 
#' the original 
#' arrays are deleted, while the merged arrays will not be invalidated, 
#' the corresponding partitions will no longer be accessible. Attempts to 
#' set deleted partitions will likely result in failure. Therefore
#' \code{symlink} should be set to true when creating merged arrays are
#' temporary for read-only purpose, and when speed and disk space is in
#' consideration. For extended reading, please check \code{\link[base]{files}} 
#' for details.
#' 
#' @examples 
#' partition_size <- 1
#' type <- "double"
#' x1 <- filearray_create(
#'     tempfile(), c(2,2), type = type,
#'     partition_size = partition_size)
#' x1[] <- 1:4
#' x2 <- filearray_create(
#'     tempfile(), c(2,1), type = type,
#'     partition_size = partition_size)
#' x2[] <- 5:6
#' 
#' y1 <- filearray_bind(x1, x2)
#' y2 <- filearray_bind(x1, x2, symlink = TRUE)
#' 
#' # y1 copies partition files, and y2 simply creates links 
#' y1[] - y2[]
#' 
#' # change x1
#' x1[1,1] <- NA
#' 
#' # y1 is not affected
#' y1[]
#' 
#' # y2 changes 
#' y2[]
#' 
#' 
#' @export
filearray_bind <- function(
    ..., .list = list(), filebase = tempfile(), symlink = FALSE
){
    arrays <- c(list(...), .list)
    if(!length(arrays)){
        stop("`filearray_bind`: No file arrays to bind")
    }
    if(!all(sapply(arrays, is_filearray))){
        stop("`filearray_bind`: All inputs must be file arrays")
    }
    type <- arrays[[1]]$type()
    part_size <- arrays[[1]]$partition_size()
    dim <- arrays[[1]]$dimension()
    dim[[length(dim)]] <- part_size
    
    last_margin <- sapply(arrays, function(x){
        if(!x$valid()){
            stop("`filearray_bind`: some input arrays are invalid")
        }
        if(x$type() != type){
            stop("`filearray_bind`: All arrays must be the same type")
        }
        if(x$partition_size() != part_size){
            stop("`filearray_bind`: All arrays must share the same `partition_size`")
        }
        xdim <- x$dimension()
        xdim[[length(xdim)]] <- part_size
        
        if(!identical(as.double(xdim), as.double(dim))){
            stop("`filearray_bind`: The partition size mismatch: they must be the same for all arrays")
        }
        x$initialize_partition()
        nrow(x$.partition_info)
    })
    
    dim[[length(dim)]] <- sum(last_margin)
    
    re <- filearray_create(filebase = filebase, dimension = dim, type = type, partition_size = part_size)
    
    start <- 1
    end <- 1
    
    for(ii in seq_along(last_margin)){
        arr <- arrays[[ii]]
        end <- start -1 + last_margin[[ii]]
        idx <- seq.int(start, end)
        if( symlink ){
            file.symlink(
                arr$partition_path(seq_len(last_margin[[ii]])),
                re$partition_path(idx)
            )
        } else {
            file.copy(
                arr$partition_path(seq_len(last_margin[[ii]])),
                re$partition_path(idx)
            )
        }
        
        start <- end + 1
    }
    
    if(symlink){
        re$.mode <- "readonly"
    }
    re
    
}


