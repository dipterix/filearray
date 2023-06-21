
#' @title The type of a file array (extended)
#' @param x any file array
#' @return A character string. The possible values are \code{"double"},
#' \code{"integer"}, \code{"logical"}, and \code{"raw"}
#' @export
setGeneric("typeof")

#' @rdname typeof
setMethod('typeof', signature(x = "FileArray"), function(x){
    if(!x$valid()){
        stop("Invalid file array")
    }
    x$type()
})

#' @rdname typeof
setMethod('typeof', signature(x = "FileArrayProxy"), function(x){
    if(!x$valid()){
        stop("Invalid file array")
    }
    if( length(x$.ops) ) {
        final_op <- x$.ops[[length(x$.ops)]]
        return( final_op$output_type )
    } else {
        x$type()
    }
})
