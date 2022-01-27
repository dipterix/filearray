#' @title Merge and bind homogeneous file arrays
#' @description The file arrays to be merged must be homogeneous:
#' same data type, partition size, and partition length
#' @param ...,.list file array instances
#' @param filebase where to create merged array
#' @param symlink whether to use \code{\link[base]{file.symlink}}; if true,
#' then partition files will be symbolic-linked to the original arrays,
#' otherwise the partition files will be copied over. If you want your data
#' to be portable, do not use symbolic-links. The default value is \code{FALSE}
#' @param overwrite whether to overwrite when \code{filebase} already exists;
#' default is false, which raises errors
#' @param cache_ok see 'Details', only used if \code{overwrite} is true.
#' 
#' @return A bound array in \code{'FileArray'} class.
#' 
#' @details The input arrays must share the same data type and partition size.
#' The dimension for each partition should also be the same. For example
#' an array \code{x1} has dimension \eqn{100x20x30} with partition size 
#' \code{1}, then each partition dimension is \eqn{100x20x1}, and there are 
#' \code{30} partitions. \code{x1} can bind with another array of the same 
#' partition size. This means if \code{x2} has dimension
#' \eqn{100x20x40} and each partition size is \code{1}, then \code{x1} and 
#' \code{x2} can be merged.
#' 
#' If \code{filebase} exists and \code{overwrite} is \code{FALSE}, an error will 
#' always raise. If \code{overwrite=TRUE} and \code{cache_ok=FALSE}, then
#' the existing \code{filebase} will be erased and any data stored within will
#' be lost. 
#' If both \code{overwrite} and \code{cache_ok} are \code{TRUE}, then 
#' , before erasing \code{filebase}, the function validates the existing
#' array header and compare the header signatures. If the existing header
#' signature is the same as the array to be created, then the existing array 
#' will be returned. This \code{cache_ok} could be extremely useful when
#' binding large arrays with \code{symlink=FALSE} as the cache might avoid
#' moving files around. However, \code{cache_ok} should be enabled with caution.
#' This is because only the header information will be compared, but the 
#' partition data will not be compared. If the existing array was generated from
#' an old versions of the source arrays, but the data from the source arrays
#' has been altered, then the \code{cache_ok=TRUE} is rarely proper as the cache
#' is outdated.
#' 
#' The \code{symlink} option should be used with extra caution. Creating 
#' symbolic links is definitely faster than copying partition files. However, 
#' since the partition files are simply linked to the original partition files, 
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
#' y1 <- filearray_bind(x1, x2, symlink = FALSE)
#' y2 <- filearray_bind(x1, x2)
#' 
#' # y1 copies partition files, and y2 simply creates links 
#' # if symlink is supported
#' 
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
    ..., .list = list(), filebase = tempfile(), 
    symlink = FALSE, overwrite = FALSE, cache_ok = FALSE
){
    stopifnot(length(filebase) == 1)
    symlink <- as.logical(symlink)[[1]]
    # options("filearray.symlink_enabled" = TRUE)
    if(symlink && !getOption("filearray.symlink_enabled", symlink_enabled())){
        symlink <- FALSE
        quiet_warning("Symbolic link is disabled. Force `symlink` to be FALSE")
    }
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
    
    last_margins <- sapply(arrays, dim)
    last_margins <- last_margins[nrow(last_margins),]
    
    if(!all(last_margins %% part_size == 0)){
        quiet_warning("One or more arrays have last margin size that cannot be devided by `partition_size`. This will cause binding arrays to be mis-aligned. ")
    }
    
    dim[[length(dim)]] <- sum(last_margin) * part_size
    
    # Dry-run
    start <- 1
    end <- 1
    
    bind_info <- list(
        is_bound = TRUE,
        dimension = as.double(dim), 
        type = type, 
        partition_size = as.double(part_size),
        partition_header_signatures = sapply(arrays, function(y){ y$header_signature(include_path = TRUE) })
    )
    bind_signature <- digest::digest(bind_info, algo = "sha256")
    
    source_info <- list()
    for(ii in seq_along(last_margin)){
        arr <- arrays[[ii]]
        end <- start -1 + last_margin[[ii]]
        idx <- seq.int(start, end)
        source_info[[ii]] <- arr$partition_path(seq_len(last_margin[[ii]]))
        start <- end + 1
    }
    bind_info$source_info <- source_info
    bind_info$symlink  <- symlink
    
    if(file.exists(filebase)) {
        
        if(!overwrite){
            stop("Array has already existed at: ", filebase)
        } 
        
        if(cache_ok){
            check <- tryCatch({
                check <- FALSE
                if(file.exists(file.path(filebase, "meta"))){
                    header <- load_meta(filebase)
                    if(
                        identical(header$filearray_bind_signature, bind_signature) &&
                        identical(header$filearray_bind$symlink, symlink)
                    ){
                        check <- TRUE
                    }
                }
                check
            }, error = function(e){
                FALSE
            })
            if(check){
                re <- filearray_load(filebase = filebase, mode = "readonly")
                attr(re, "cached_bind") <- TRUE
                return(re)
            }
        }
        
        unlink(filebase, recursive = TRUE)
    }
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
    
    re$.header$filearray_bind <- bind_info
    re$.header$filearray_bind_signature <- bind_signature
    re$.save_header()
    
    if(symlink){
        re$.mode <- "readonly"
    } 
    re
    
}


