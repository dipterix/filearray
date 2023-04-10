
#' @title Apply functions over file array margins (extended)
#' @param X a file array
#' @param MARGIN scalar giving the subscripts which the function will be applied over. Current implementation only allows margin size to be one
#' @param FUN the function to be applied
#' @param ... optional arguments to \code{FUN}
#' @param simplify a logical indicating whether results should be simplified if possible
#' @return See Section 'Value' in \code{\link[base]{apply}};
#' @export
setGeneric("apply")

apply_filearray <- function(X, MARGIN, FUN, ..., simplify = TRUE){
    if(!X$valid()){
        stop("Invalid file array")
    }
    dim <- X$dimension()
    
    FUN <- match.fun(FUN)
    simplify <- isTRUE(simplify)
    d <- dim(X)
    dl <- length(d)
    dn <- dimnames(X)
    ds <- seq_len(dl)
    if (is.character(MARGIN)) {
        dnn <- names(dn)
        if (is.null(dnn)) 
            stop("'X' must have named dimnames")
        MARGIN <- match(MARGIN, dnn)
        if (anyNA(MARGIN)) 
            stop("not all elements of 'MARGIN' are names of dimensions")
    }
    d.call <- d[-MARGIN]
    d.ans <- d[MARGIN]
    if (anyNA(d.call) || anyNA(d.ans)) {
        stop("'MARGIN' does not match dim(X)")
    }
    s.call <- ds[-MARGIN]
    s.ans <- ds[MARGIN]
    if(length(s.ans) != 1){
        stop("`apply` on FileArray margin size can only be 1.")
    }
    dn.call <- dn[-MARGIN]
    dn.ans <- dn[MARGIN]
    d2 <- prod(d.ans)
    if (d2 == 0L) {
        newX <- array(vector(typeof(X), 1L), 
                      dim = c(prod(d.call), 1L))
        if (length(d.call) < 2L) {
            tmp <- newX[, 1]
        } else {
            tmp <- array(newX[, 1L], d.call, dn.call)
        }
        ans <- forceAndCall(1, FUN, tmp, ...)
        if(is.null(ans)){
            return(ans)
        } else if (length(d.ans) < 2L) {
            return(ans[1L][-1L])
        } else {
            return(array(ans, d.ans, dn.ans))
        }
    }
    
    tmp <- rep("", dl)
    tmp[[s.ans]] <- ".__i__."
    f <- sprintf("function(.__i__., ...){ FUN(X[%s], ...) }", paste(tmp, collapse = ","))
    f <- eval(parse(text = f))
    
    sapply(seq_len(d[[s.ans]]), f, ..., simplify = simplify)
}

#' @rdname apply
#' @export
setMethod('apply', signature(X = "FileArray"), apply_filearray)

#' @rdname apply
#' @export
setMethod('apply', signature(X = "FileArrayProxy"), apply_filearray)

