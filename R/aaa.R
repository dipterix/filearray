#' @importFrom methods new
#' @importFrom methods signature
#' @importFrom methods setGeneric
#' @importFrom methods setRefClass
NULL
HEADER_SIZE <- 1024
FILE_VER <- c( 1L, 0L, 0L )

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
