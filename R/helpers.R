# ---- UUID generator -------------------------------------------------------
new_uuid <- function(prefix = 0L) {
    sprintf("%04d-%s", prefix, uuid::UUIDgenerate(output = "string"))
}

# ---- Index helpers --------------------------------------------------------

#' @noRd 
#' Check if two arrays share the same dimension
is_same_dim <- function(x, y) {
    dx <- dim(x)
    if(length(dx) >= 2L) {
        dy <- dim(y)
        if(length(dx) != length(dy)) { return(FALSE) }
        # valid # of margins and dimensions are consistent
        if(all(dx == dy)) { return(TRUE) }
    } else if( length(x) == length(y) ) {
        # dx dy might not exist, check length
        return(TRUE)
    }
    return(FALSE)
}


# ---- data type helpers ----------------------------------------------------

#' @noRd 
#' Guess output data types for meth operators such as \code{+-*/}
operation_output_type <- function(
        type1, type2, 
        logical = c("integer", "logical"), raw = c("error", "integer"),
        float = getOption("filearray.operator.precision", "double")
) {
    
    raw <- match.arg(raw)
    has_raw <- identical(type1, "raw") || identical(type2, "raw")
    
    if( has_raw && identical(raw, "error") ) {
        stop("non-numeric argument to binary operator")
    }
    
    if( identical(type1, "complex") ) { return("complex") }
    if( identical(type2, "complex") ) { return("complex") }
    
    if( identical(type1, "double") ) { return(float) }
    if( identical(type2, "double") ) { return(float) }
    
    if( identical(type1, "float") ) { return(float) }
    if( identical(type2, "float") ) { return(float) }
    
    if( identical(type1, "integer") ) { return("integer") }
    if( identical(type2, "integer") ) { return("integer") }
    
    if( identical(type1, "raw") ) { return("integer") }
    if( identical(type2, "raw") ) { return("integer") }
    
    logical <- match.arg(logical)
    if( identical(type1, "logical") ) { return(logical) }
    if( identical(type2, "logical") ) { return(logical) }
    
    stop("unrecognized combination of types: ", type1, ", ", type2)
}

