
fa_add_scalar <- function(e1, e2, label = "adding scalar") {
    
    stopifnot(length(e2) == 1)
    
    e1 <- as_filearrayproxy(e1)
    input_uuid <- e1$uuid()
    
    # check input types 
    out_type <- operation_output_type(typeof(e1), typeof(e2))
    
    op_func <- function(v, ...) {
        # v is an env with names: uuids
        return(v[[ input_uuid ]] + e2)
    }
    
    e1$add_operator( op_func, out_type = out_type, context = "scalar", label = label )
    return( e1 )
}

fa_add_filearray <- function(e1, e2, label = "adding array") {
    # e1 and e2 must be filearray or filearray proxy
    stopifnot(is_filearray(e1) && is_filearray(e2))
    
    e1 <- as_filearrayproxy(e1)
    e2 <- as_filearrayproxy(e2)
    
    
    uuid1 <- e1$uuid()
    uuid2 <- e2$uuid()
    e1$link_proxy( e2 )
    
    # check input types 
    out_type <- operation_output_type(typeof(e1), typeof(e2))
    
    op_func <- function(value_list, ...) {
        return(value_list[[ uuid1 ]] + value_list[[ uuid2 ]])
    }
    
    e1$add_operator( op_func, out_type = out_type, context = "array", label = label )
    
    return( e1 )
}



filearray_add <- function(e1, e2) {
    call <- match.call()
    call[[1]] <- quote(`+`)
    label <- sprintf("Calculating: %s", deparse1(call))
    
    if(length(e2) > 1) {
        return(fa_add_filearray(e1, e2, label = label))
    } else {
        return(fa_add_scalar(e1, e2, label = label))
    }
}

filearray_add2 <- function(e1, e2) {
    call <- match.call()
    call[[1]] <- quote(`+`)
    label <- sprintf("Calculating: %s", deparse1(call))
    
    if(length(e1) > 1) {
        return(fa_add_filearray(e2, e1, label = label))
    } else {
        return(fa_add_scalar(e2, e1, label = label))
    }
}

# setMethod('+', signature(e1 = "FileArrayProxy", e2 = "FileArrayProxy"), filearray_add)
# setMethod('+', signature(e1 = "FileArrayProxy", e2 = "FileArray"), filearray_add)
# setMethod('+', signature(e1 = "FileArray", e2 = "FileArrayProxy"), filearray_add)
#' @export
setMethod('+', signature(e1 = "FileArray", e2 = "FileArray"), filearray_add)

# setMethod('+', signature(e1 = "FileArrayProxy", e2 = "numeric"), filearray_add)
# setMethod('+', signature(e1 = "numeric", e2 = "FileArrayProxy"), filearray_add2)
# setMethod('+', signature(e1 = "FileArrayProxy", e2 = "complex"), filearray_add)
# setMethod('+', signature(e1 = "complex", e2 = "FileArrayProxy"), filearray_add2)
# setMethod('+', signature(e1 = "FileArrayProxy", e2 = "logical"), filearray_add)
# setMethod('+', signature(e1 = "logical", e2 = "FileArrayProxy"), filearray_add2)

#' @export
setMethod('+', signature(e1 = "FileArray", e2 = "numeric"), filearray_add)
#' @export
setMethod('+', signature(e1 = "numeric", e2 = "FileArray"), filearray_add2)

#' @export
setMethod('+', signature(e1 = "FileArray", e2 = "complex"), filearray_add)
#' @export
setMethod('+', signature(e1 = "complex", e2 = "FileArray"), filearray_add2)

#' @export
setMethod('+', signature(e1 = "FileArray", e2 = "logical"), filearray_add)
#' @export
setMethod('+', signature(e1 = "logical", e2 = "FileArray"), filearray_add2)
