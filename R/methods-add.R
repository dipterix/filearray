
fa_add <- function(e1, e2) {
    call <- match.call()
    call[[1]] <- quote(`+`)
    label <- sprintf("Calculating: %s (fa_add)", deparse1(call))
    
    fa_pairwise_operator(e1, e2, op = "+", label = label)
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
