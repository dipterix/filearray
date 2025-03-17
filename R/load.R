
#' @title Create or load existing file arrays
#' @name filearray
#' @author Zhengjia Wang
#' @param filebase a directory path to store arrays in the local file 
#' system. When creating an array, the path must not exist.
#' @param x R object such as array, file array proxy, or character that can be 
#' transformed into file array
#' @param dimension dimension of the array, at least length of 2
#' @param type storage type of the array; default is \code{'double'}. Other
#' options include \code{'integer'}, \code{'logical'}, and \code{'raw'}.
#' @param mode whether allows writing to the file; choices are 
#' \code{'readwrite'} and \code{'readonly'}.
#' @param partition_size positive partition size for the last margin, or
#' \code{NA} to automatically guess; see 'Details'.
#' @param initialize whether to initialize partition files; default is false
#' for performance considerations. However, if the array is dense, it is 
#' recommended to set to true
#' @param ... additional headers to check used by \code{filearray_checkload}
#' (see 'Details'). This argument is ignored by \code{filearray_create}, 
#' reserved for future compatibility.
#' @param symlink_ok whether arrays with symbolic-link partitions can pass 
#' the test; this is usually used on bound arrays with symbolic-links; see 
#' \code{\link{filearray_bind}};
#' @param verbose whether to print out some debug messages
#' @param on_missing function to handle file array (such as initialization)
#' when a new array is created; must take only one argument, the array object
#' @param auto_set_headers whether to automatically set headers if array is 
#' missing or to be created; default is true
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
#' The arguments \code{...} in \code{filearray_checkload} should be named
#' arguments that provide additional checks for the header information. 
#' The check will fail if at least one header is not identical. For example,
#' if an array contains header key-signature pair, one can use 
#' \code{filearray_checkload(..., key = signature)} to validate the signature.
#' Note the comparison will be rigid, meaning the storage type of the headers 
#' will be considered as well. If the signature stored in the array is an 
#' integer while provided is a double, then the check will result in failure.
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
#' x$set_header("signature", "tom")
#' filearray_checkload(filebase, signature = "tom")
#' 
#' \dontrun{
#' # Trying to load with wrong signature
#' filearray_checkload(filebase, signature = "jerry")
#' }
#' 
#' 
#' # check-load, and create a new array if fail
#' x <- filearray_load_or_create(
#'     filebase = filebase, dimension = c(200, 30, 8),
#'     verbose = FALSE, signature = "henry"
#' )
#' x$get_header("signature")
#' 
#' # check-load with initialization
#' x <- filearray_load_or_create(
#'     filebase = filebase,
#'     dimension = c(3, 4, 5),
#'     verbose = FALSE, mode = "readonly",
#'     on_missing = function(array) {
#'         array[] <- seq_len(60)
#'     }
#' )
#' 
#' x[1:3,1,1]
#' 
#' # Clean up
#' unlink(filebase, recursive = TRUE)
#' 
NULL

#' @rdname filearray
#' @export
filearray_create <- function(
    filebase, dimension, 
    type = c('double', 'float', 'integer', 'logical', 'raw', 'complex'), 
    partition_size = NA,
    initialize = FALSE,
    ...)
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
    if(initialize){
        arr$initialize_partition()
    }
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

#' @rdname filearray
#' @export
filearray_checkload <- function(
    filebase, mode = c("readonly", "readwrite"), ..., symlink_ok = TRUE
) {
    mode <- match.arg(mode)
    if(!dir.exists(filebase)){
        stop("Filearray does not exists.")
    }
    arr <- filearray::filearray_load(filebase, mode = mode)
    
    if(!symlink_ok){
        bind_info <- arr$get_header("filearray_bind", list())
        if(isTRUE(bind_info$symlink)){
            stop("The array partition files contain symlinks")
        }
    }
    
    if(...length() == 0){ return(arr) }
    header_list <- list(...)
    header_names <- names(header_list)
    
    for(nm in header_names){
        expected <- arr$get_header(nm)
        given <- header_list[[nm]]
        if(nm != "" && !identical(given, expected)){
            stop("The header `", nm, "` (", deparse1(expected), ") is not identical with given values (", deparse1(given), ").")
        }
    }
    return(arr)
}


#' @rdname filearray
#' @export
filearray_load_or_create <- function(
    filebase, dimension, on_missing = NULL, type = NA, 
    ..., mode = c("readonly", "readwrite"), symlink_ok = TRUE,
    initialize = FALSE, partition_size = NA, verbose = FALSE, 
    auto_set_headers = TRUE
) {
    mode <- match.arg(mode)
    filebase <- normalizePath(filebase, mustWork = FALSE, winslash = "/")
    if(length(filebase) != 1 || grepl("(^|^[A-Za-z]:)/$", filebase)) {
        stop("Invalid filebase to store a file array.")
    }
    
    
    dimension <- as.integer(dimension)
    if(length(dimension) < 2 || any(is.na(dimension) | dimension < 0)) {
        stop("Incorrect dimension for a file array: `dimension` must a valid positive integer vector with length of two or above.")
    }
    
    if(!is.null(on_missing)) {
        if(!is.function(on_missing) || !length(formals(on_missing))) {
            stop("`filearray_load_or_create`: `on_missing` must be a function with one argument (i.e. the file array)")
        }
    }
    
    
    additional_headers <- list(...)
    add_header_names <- names(additional_headers)
    if(length(additional_headers)) {
        if(!length(add_header_names) || "" %in% trimws(additional_headers)) {
            stop("`filearray_load_or_create`: additional parameters must be named.")
        }
    }
    
    
    arr <- tryCatch(
        expr = {
            # try to load existing array
            arr <- filearray_checkload(
                filebase = filebase, mode = mode, 
                symlink_ok = symlink_ok, ...
            )
            
            # If no error raised, the array has been loaded
            
            if(!is.na(type)) {
                if(!identical(arr$type(), type)) {
                    stop("`filearray_load_or_create`: Requested array type does not match with existing array.")
                }
            } else {
                # in case the array needs to be reconstructed, assuming the type
                # remain the same
                type <- arr$type()
            }
            
            # Now check the dimension
            arr_dim <- as.integer(arr$dimension())
            if(!identical(arr_dim, dimension)) {
                stop("`filearray_load_or_create`: Requested dimension does not match with existing array.")
            }
            
            arr
        },
        error = function(e) {
            if(verbose) {
                message("`filearray_load_or_create`: cannot load the existing file array: ", e$message, "\nTrying creating a new one. If the array already exists, its file path will be removed.")
            }
            if(file.exists(filebase)) {
                unlink(filebase, recursive = TRUE, force = TRUE)
            }
            pdir <- dirname(filebase)
            if(!dir.exists(pdir)) {
                dir.create(pdir, showWarnings = FALSE, recursive = TRUE)
            }
            # create the array
            if(is.na(type)) { type <- 'double' }
            arr <- filearray_create(
                filebase = filebase,
                dimension = dimension,
                type = type,
                partition_size = partition_size,
                initialize =  initialize
            )
            # seal the header
            if( auto_set_headers ) {
                for(nm in add_header_names) {
                    arr$set_header(key = nm, value = additional_headers[[nm]], save = FALSE)
                }
            }
            # run on_missing if the function exists
            if(is.function(on_missing)) {
                arr$.mode <- "readwrite"
                on_missing(arr)
            }
            arr$.save_header()
            arr
        }
    )
    
    # set mode
    arr$.mode <- mode
    arr
}

