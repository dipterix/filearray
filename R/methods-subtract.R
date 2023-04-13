
fa_subtract <- function(e1, e2) {
    call <- match.call()
    call[[1]] <- quote(`-`)
    label <- sprintf("Calculating: %s (fa_subtract)", deparse1(call))
    
    fa_pairwise_operator(e1, e2, op = "-", label = label)
}


#' @export
setMethod('-', signature(e1 = "FileArray", e2 = "FileArray"), fa_subtract)

# setMethod('-', signature(e1 = "FileArrayProxy", e2 = "numeric"), filearray_subtract_scalar)
# setMethod('-', signature(e1 = "numeric", e2 = "FileArrayProxy"), scalar_subtract_filearray)
# setMethod('-', signature(e1 = "FileArrayProxy", e2 = "complex"), filearray_subtract_scalar)
# setMethod('-', signature(e1 = "complex", e2 = "FileArrayProxy"), scalar_subtract_filearray)
# setMethod('-', signature(e1 = "FileArrayProxy", e2 = "logical"), filearray_subtract_scalar)
# setMethod('-', signature(e1 = "logical", e2 = "FileArrayProxy"), scalar_subtract_filearray)



#' @export
setMethod('-', signature(e1 = "FileArray", e2 = "numeric"), fa_subtract)
#' @export
setMethod('-', signature(e1 = "numeric", e2 = "FileArray"), fa_subtract)

#' @export
setMethod('-', signature(e1 = "FileArray", e2 = "complex"), fa_subtract)
#' @export
setMethod('-', signature(e1 = "complex", e2 = "FileArray"), fa_subtract)

#' @export
setMethod('-', signature(e1 = "FileArray", e2 = "logical"), fa_subtract)
#' @export
setMethod('-', signature(e1 = "logical", e2 = "FileArray"), fa_subtract)

#' @export
setMethod('-', signature(e1 = "FileArray", e2 = "array"), fa_subtract)
#' @export
setMethod('-', signature(e1 = "array", e2 = "FileArray"), fa_subtract)
