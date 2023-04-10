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

#' @noRd 
#' Guess partition size from given dimensions and element size.
#' This function is used to estimate a decent partition size when creating 
#' arrays
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


guess_fmap_buffer_size <- function(dim, element_size = 16L) {
    buffer_len <- get_buffer_size() / element_size
    buffer_large <- max(buffer_len, 1048576L) # 8.5MB, 1024 x 1024
    
    
    dimcprod <- cumprod(dim)
    len <- dimcprod[[length(dimcprod)]]
    min_partition_size <- dimcprod[[length(dimcprod) - 1]]
    
    # tiny array
    if( buffer_len >= len ) { return( as.integer(len) ) }
    
    # small array
    if( buffer_large >= min_partition_size ) { return( as.integer(min_partition_size) ) }
    
    # large array
    dimcprod <- dimcprod[dimcprod <= buffer_large]
    if(!length(dimcprod)) { return( dim[[1]] * dim[[2]] ) }
    
    # mid array
    unit_size <- dimcprod[[length(dimcprod)]]
    fct <- floor(buffer_large / unit_size)
    if(fct < 1) { fct <- 1 }
    return( as.integer(unit_size * fct) )
}

# Run this with caveat! must have GCD > 1... we don't check
common_fmap_buffer_size <- function(..., .list = NULL) {
    stopifnot(...length() + length(.list) > 0)
    sizes <- sapply(c(list(...), .list), function(x){
        if(length(x) == 1L) { return(as.integer(x)) }
        guess_fmap_buffer_size(x)
    }, simplify = TRUE)
    # sizes <- sizes[sizes < 1048576L]
    # if(!length(sizes)) { return(1048576L) }
    if(length(sizes) == 1L) { return( sizes ) }
    
    Reduce(function(a, b) {
        if( a <= 0 || b <= 0 ) {
            stop("Cannot find proper input buffer size when data has zero length")
        }
        rem <- a %% b
        if( rem == 0 ) { return( b ) }
        return( Recall(b, rem) )
    }, sizes)
}

validate_fmap_buffer_size <- function(size, input_lens) {
    size <- size[[1]]
    if( any(input_lens == 0) ) { return(FALSE) }
    if( is.na(size) || size != round(size) ) {
        return(FALSE)
    }
    
    # number of runs for each buffer is `min(input_lens) / size`
    # # elems in buffer is input_lens / count
    buffer_nelems <- input_lens / (min(input_lens) / size)
    
    if(any( buffer_nelems != round(buffer_nelems))) {
        return(FALSE)
    }
    if( size == 1 && any(buffer_nelems > 1) ) {
        return(FALSE)
    }
    return(structure(TRUE, buffer_nelems = buffer_nelems))
}

# ---- data type helpers ----------------------------------------------------

is_filearray <- function(object, proxy_ok = TRUE){
    if(!isS4(object)){ return(FALSE) }
    if( !proxy_ok && is_fileproxy(object) ) {
        return( FALSE )
    }
    return(inherits(object, c("FileArray", "FileArrayProxy")))
}

is_fileproxy <- function(object){
    if(!isS4(object)){ return(FALSE) }
    return(inherits(object, "FileArrayProxy"))
}


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

