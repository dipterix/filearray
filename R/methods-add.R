
fa_add_internal <- function(e1, e2, label = "add array") {
    
    stopifnot(is_filearray(e1) || is_filearray(e2))
    out_type <- operation_output_type(typeof(e1), typeof(e2))
    
    # check if e1 is scalar
    if(length(e1) == 1) {
        e2 <- as_filearrayproxy(e2)
        uuid2 <- e2$uuid()
        op_func <- function(v, ...) {
            # v is an env with names: uuids
            return(e1 + v[[ uuid2 ]])
        }
        e2$add_operator( op_func, out_type = out_type, context = "scalar", label = label )
        return(e2)
    }
    
    # check if e2 is a scalar
    if(length(e2) == 1) {
        e1 <- as_filearrayproxy(e1)
        uuid1 <- e1$uuid()
        op_func <- function(v, ...) {
            # v is an env with names: uuids
            return(v[[ uuid1 ]] + e2)
        }
        e1$add_operator( op_func, out_type = out_type, context = "scalar", label = label )
        return(e1)
    }
    
    # check if e1 or e2 is numerical
    if(!is_filearray(e1)) {
        if(!is_same_dim(e1, e2)) {
            stop("non-conformable arrays")
        }
        e1 <- as_filearray(e1, dimension = dim(e2))
    }
    
    if(!is_filearray(e2)) {
        if(!is_same_dim(e2, e1)) {
            stop("non-conformable arrays")
        }
        e2 <- as_filearray(e2, dimension = dim(e1))
    }
    
    # e1 and e2 must be filearray or filearray proxy
    if(!is_same_dim(e1, e2)) {
        stop("non-conformable arrays")
    }
    
    e1 <- as_filearrayproxy(e1)
    e2 <- as_filearrayproxy(e2)
    
    
    uuid1 <- e1$uuid()
    uuid2 <- e2$uuid()
    e1$link_proxy( e2 )
    
    # check input types 
    out_type <- operation_output_type(typeof(e1), typeof(e2))
    
    op_func <- function(v, ...) {
        return(v[[ uuid1 ]] + v[[ uuid2 ]])
    }
    
    e1$add_operator( op_func, out_type = out_type, context = "array", label = label )
    
    return( e1 )
}

fa_add <- function(e1, e2) {
    call <- match.call()
    call[[1]] <- quote(`+`)
    label <- sprintf("Calculating: %s (fa_add)", deparse1(call))
    
    fa_add_internal(e1, e2, label)
}


# setMethod('+', signature(e1 = "FileArrayProxy", e2 = "FileArrayProxy"), filearray_add)
# setMethod('+', signature(e1 = "FileArrayProxy", e2 = "FileArray"), filearray_add)
# setMethod('+', signature(e1 = "FileArray", e2 = "FileArrayProxy"), filearray_add)
#' @export
setMethod('+', signature(e1 = "FileArray", e2 = "FileArray"), fa_add)

# setMethod('+', signature(e1 = "FileArrayProxy", e2 = "numeric"), filearray_add)
# setMethod('+', signature(e1 = "numeric", e2 = "FileArrayProxy"), filearray_add2)
# setMethod('+', signature(e1 = "FileArrayProxy", e2 = "complex"), filearray_add)
# setMethod('+', signature(e1 = "complex", e2 = "FileArrayProxy"), filearray_add2)
# setMethod('+', signature(e1 = "FileArrayProxy", e2 = "logical"), filearray_add)
# setMethod('+', signature(e1 = "logical", e2 = "FileArrayProxy"), filearray_add2)

#' @export
setMethod('+', signature(e1 = "FileArray", e2 = "numeric"), fa_add)
#' @export
setMethod('+', signature(e1 = "numeric", e2 = "FileArray"), fa_add)

#' @export
setMethod('+', signature(e1 = "FileArray", e2 = "complex"), fa_add)
#' @export
setMethod('+', signature(e1 = "complex", e2 = "FileArray"), fa_add)

#' @export
setMethod('+', signature(e1 = "FileArray", e2 = "logical"), fa_add)
#' @export
setMethod('+', signature(e1 = "logical", e2 = "FileArray"), fa_add)

#' @export
setMethod('+', signature(e1 = "FileArray", e2 = "array"), fa_add)
#' @export
setMethod('+', signature(e1 = "array", e2 = "FileArray"), fa_add)
