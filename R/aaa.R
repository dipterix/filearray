#' @importFrom methods new
#' @importFrom methods signature
#' @importFrom methods setGeneric
#' @importFrom methods setRefClass
#' @importFrom methods setMethod
NULL
HEADER_SIZE <- 1024
FILE_VER <- c( 1L, 1L, 0L )
HEADER_VER <- 1L
RESERVED_HEADERS <- c("endianness", "version", "sexp_type", 
                      "unit_bytes", "partition", "partition_size", 
                      "partition_dim", "header_bytes", "header_version",
                      "dimnames", "content_length")

# The saved files are always little endian
ENDIANNESS <- "little"

max_buffer_size <- local({
    # By default, maximum of 2MB buffer size
    size <- 2097152
    function(v){
        if(!missing(v)){
            if(v < 64){
                stop("Maximum buffer size is too small.")
            }
            v <- 2^ceiling(log2(v))
            if(v > 2^30){
                stop("Maximum buffer size is too large.")
            }
            size <<- v
        }
        return(size)
    }
})

quiet_warning <- function(..., call. = FALSE){
    if(!getOption("filearray.quiet", FALSE)){
        warning(..., '\n\n* To suppress this message, set `options("filearray.quiet" = TRUE)`', call. = call.)
    }
}

get_os <- function(){
    if("windows" %in% tolower(.Platform$OS.type)){
        return("windows")
    }
    os <- tolower(R.version$os)
    if(startsWith(os, "darwin")){
        return('darwin')
    }
    if(startsWith(os, "linux")){
        return('linux')
    }
    if(startsWith(os, "solaris")){
        return('solaris')
    }
    if(startsWith(os, "win")){
        return('windows')
    }
    return('unknown')
}


deparse1 <- function (expr, collapse = " ") {
    paste(deparse(expr), collapse = collapse)
}

temp_dir <- function(check = FALSE) {
    re <- file.path(getOption("filearray.temporary.path", tempdir()), "_filearray_tempdir")
    if(check && !dir.exists(re)) {
        dir.create(re, showWarnings = FALSE, recursive = TRUE)
    }
    re
}

temp_path <- function(pattern = "tmpfilearray", fileext = ".farr", check = FALSE) {
    tempfile(pattern = pattern, tmpdir = temp_dir(check = check), fileext = fileext)
}

clear_cache <- function() {
    tdir <- temp_dir()
    if(dir.exists(tdir)) {
        unlink(tdir, recursive = TRUE)
    }
}
