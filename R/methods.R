
## S3

#' @title 'S3' methods for 'FileArray'
#' @name S3-filearray
#' @description These are 'S3' methods for 'FileArray'
#' @param x a file array
#' @param drop whether to drop dimensions; see topic \code{\link[base]{Extract}}
#' @param reshape a new dimension to set before returning subset results; default is \code{NULL} (use default dimensions)
#' @param strict whether to allow indices to exceed bound; currently only accept \code{TRUE}
#' @param dimnames whether to preserve \code{\link[base]{dimnames}}
#' @param value value to substitute or set
#' @param na.rm whether to remove \code{NA} values during the calculation
#' @param split_dim internally used; split dimension and calculate indices to
#' manually speed up the subset; value ranged from 0 to size of dimension minus
#' one.
#' @param i,... index set, or passed to other methods
NULL

#' @describeIn S3-filearray get element by position
#' @export
`[.FileArray` <- function(x, ..., drop = TRUE, reshape = NULL, strict = TRUE, dimnames = TRUE, split_dim = 0) {
    if(!x$valid()){
        stop("Invalid file array")
    }
    drop <- isTRUE(drop)
    # file <- tempfile(); x <- filearray_create(file, c(300, 400, 100, 1))
    arglen <- ...length()
    elem_size <- x$element_size()
    dim <- x$dimension()
    
    listOrEnv <- list()
    if(arglen == 1){
        tmp <- tryCatch({
            ...elt(1)
        }, error = function(e){
            NULL
        })
        if(length(tmp)){
            stop("Subset FileArray only allows x[] or x[i,j,...] (single index like x[i] is not allowed, use x[[i]] instead)")
        }
        
        
    } else if(arglen > 1){
        if(arglen != length(dim)){
            stop("Subset FileArray dimension mismatch.")
        }
        missing_args <- check_missing_dots(environment())
        
        for(ii in seq_len(arglen)){
            if( missing_args[[ii]] ){
                listOrEnv[[ii]] <- seq_len(dim[[ii]])
            } else {
                tmp <- ...elt(ii)
                if(!length(tmp)){
                    tmp <- integer(0L)
                } else if(is.logical(tmp)){
                    if(length(tmp) > dim[[ii]]){
                        stop("(subscript) logical subscript too long")
                    }
                    tmp <- rep(tmp, ceiling(dim[[ii]] / length(tmp)))
                    tmp <- tmp[seq_len(dim[[ii]])]
                    tmp <- seq_along(tmp)[tmp]
                }
                listOrEnv[[ii]] <- tmp
            }
        }
    }
    
    # guess split dim
    max_buffer <- get_buffer_size() / elem_size
    
    if(length(listOrEnv) == length(dim)){
        idxrange <- sapply(listOrEnv, function(x){
            if(!length(x) || all(is.na(x))){ return(1L) }
            rg <- range(x, na.rm = TRUE)
            return(rg[2] - rg[1] + 1)
        })
    } else {
        idxrange <- dim
    }
    split_dim <- as.integer(split_dim)
    if(is.na(split_dim) || split_dim <= 0 || split_dim >= length(dim)){
        # worst-case time-complexity
        time_complexity <-
            sapply(seq_len(length(dim) - 1), function(split_dim) {
                dim[[length(dim)]] <- 1
                idx1dim <- dim[seq_len(split_dim)]
                idx1dim[[split_dim]] <- idxrange[[split_dim]]
                idx1len <- prod(idx1dim)
                idx2len <- prod(dim[-seq_len(split_dim)])
                buffer_sz <-
                    ifelse(idx1len > max_buffer, max_buffer, idx1len)
                nloops <- ceiling(idx1len / buffer_sz)
                (idx1len * nloops + idx2len) * idx2len
            })
        split_dim <- which.min(time_complexity)
        split_dim <- split_dim[[length(split_dim)]]
    }
    

    FARR_subset2(
        filebase = x$.filebase,
        listOrEnv = listOrEnv,
        reshape = reshape,
        drop = drop,
        use_dimnames = isTRUE(dimnames),
        thread_buffer = get_buffer_size(), 
        split_dim = split_dim,
        strict = isTRUE(strict)
    )
    
}

#' @describeIn S3-filearray assign array values
#' @export
`[<-.FileArray` <- function(x, ..., value){
    if(!x$valid()){
        stop("Invalid file array")
    }
    if(isTRUE(x$.mode == 'readonly')){
        stop("File array is read-only")
    }
    
    buf_bytes <- get_buffer_size()
    on.exit({
        set_buffer_size(buf_bytes)
    })
    
    # parse ...
    dim <- x$dimension()
    arglen <- ...length()
    
    
    locs <- list()
    if(arglen <= 1){
        if( arglen == 1 ){
            missing_args <- check_missing_dots(environment())
            if(!missing_args){
                stop("SubsetAssign FileArray only allows x[] <- v or x[i,j,...] <- v (single index not allowed)")
            }
        }
        if(length(value) != prod(dim)){
            stop("SubsetAssign FileArray `value` length mismatch.")
        }
        target_dim <- dim
        x$initialize_partition(x$.partition_info[, 1])
    } else if(arglen > 1){
        if(arglen != length(dim)){
            stop("SubsetAssign FileArray dimension mismatch.")
        }
        
        missing_args <- check_missing_dots(environment())
        for(ii in seq_len(arglen)){
            if(missing_args[[ii]]){
                locs[[ii]] <- seq_len(dim[[ii]])
            } else {
                tmp <- ...elt(ii)
                if( !length(tmp) ){
                    return(x)
                }
                if(any(is.na(tmp))){
                    stop("SubsetAssign cannot contain duplicated or invalid indices.")
                }
                if(is.logical(tmp)){
                    if(length(tmp) > dim[[ii]]){
                        stop("(subscript) logical subscript too long")
                    }
                    tmp <- rep(tmp, ceiling(dim[[ii]] / length(tmp)))
                    tmp <- which(tmp[seq_len(dim[[ii]])])
                } else if(any(tmp > dim[[ii]]) ||
                   any(tmp <= 0) ||
                   any(duplicated(tmp))){
                    stop("SubsetAssign cannot contain duplicated or invalid indices.")
                }
                locs[[ii]] <- tmp
            }
        }
        
        target_dim <- sapply(locs, length)
        if(prod(target_dim) != length(value)){
            stop("SubsetAssign FileArray `value` length mismatch.")
        }
        
        # make sure partitions exist
        tmp <- locs[[length(locs)]]
        sapply(tmp, function(i){
            sel <- x$.partition_info[,3] <= i
            if(any(sel)){
                sel <- max(x$.partition_info[sel,1])
                if(x$.partition_info[sel, 3] < i){
                    sel <- sel + 1
                }
            } else {
                sel <- 1
            }
            x$initialize_partition(sel)
        })
    }
    
    if(prod(target_dim) == 0){
        return(invisible(x))
    }
    
    # decide split_dim
    buffer_sz <- buf_bytes / x$element_size()
    cprod <- cumprod(dim)
    if(length(locs) == length(dim)){
        tmp <- sapply(locs, function(x){
            if(!length(x) || all(is.na(x))){ return(1L) }
            rg <- range(x, na.rm = TRUE)
            return(rg[2] - rg[1] + 1L)
        })
        cprod <- cprod / dim * tmp
    }
    cprod <- cprod[-length(cprod)]
    if(all(cprod > buffer_sz)){
        split_dim <- 1
    } else {
        split_dim <- max(which(cprod <= buffer_sz))
    }
    
    # filebase <- paste0(x$.filebase, x$.sep)
    # FARR_subset_assign(
    #     filebase,
    #     listOrEnv = locs,
    #     dim = x$dimension(),
    #     cum_part_sizes = x$.partition_info[, 3],
    #     split_dim = split_dim,
    #     type = x$sexp_type(),
    #     value_ = value
    # )
    FARR_subset_assign2(
        filebase = x$.filebase, 
        value = value,
        listOrEnv = locs,
        split_dim = split_dim,
        thread_buffer = buf_bytes
    ) 
    invisible(x)
}

#' @describeIn S3-filearray get element by index
#' @export
`[[.FileArray` <- function(x, i){
    
    if(length(i) > 1){
        stop("attempt to select more than one element")
    } else if (length(i) < 1){
        stop("attempt to select less than one element")
    }
    
    dim <- x$dimension()
    
    locs <- arrayInd(i, dim)
    args <- c(
        list(
            x = quote(x),
            drop = TRUE
        ),
        lapply(seq_len(ncol(locs)), function(ii){
            locs[,ii,drop = TRUE]
        })
    )
    
    do.call('[', args)
}

#' @describeIn S3-filearray converts file array to native array in R
#' @export
as.array.FileArray <- function(x, reshape = NULL, drop = FALSE, ...){
    x[reshape = reshape, drop = drop]
}

#' @describeIn S3-filearray get dimensions
#' @export
dim.FileArray <- function(x){
    x$dimension()
}

#' @describeIn S3-filearray get dimension names
#' @export
dimnames.FileArray <- function(x){
    x$dimnames()
}

#' @describeIn S3-filearray set dimension names
#' @export
`dimnames<-.FileArray` <- function(x, value){
    x$dimnames(value)
    invisible(x)
}


#' @describeIn S3-filearray get array length
#' @export
length.FileArray <- function(x){
    prod(x$dimension())
}

#' @describeIn S3-filearray get max value
#' @export
max.FileArray <- function(x, na.rm = FALSE, ...){
    reduce <- function(x) {
        do.call('max', x)
    }
    if( na.rm ){
        mapreduce(
            x = x, map = .max_mapper1,
            reduce = reduce,
            ...
        )
    } else {
        x$initialize_partition()
        mapreduce(
            x = x, map = .max_mapper2,
            reduce = reduce,
            ...
        )
    }
    
}

#' @describeIn S3-filearray get min value
#' @export
min.FileArray <- function(x, na.rm = FALSE, ...){
    reduce <- function(x) {
        do.call('min', x)
    }
    
    if( na.rm ){
        mapreduce(
            x = x, map = .min_mapper1,
            reduce = reduce, ...
        )
    } else {
        x$initialize_partition()
        mapreduce(
            x = x, map = .min_mapper2,
            reduce = reduce, ...
        )
    }
    
}

#' @describeIn S3-filearray get value range
#' @export
range.FileArray <- function(x, na.rm = FALSE, ...){
    red_f <- function(x) {
        x <- do.call('cbind', x)
        c(min(x[1,]), max(x[2,]))
    }
    
    if( na.rm ){
        mapreduce( x = x, map = .range_mapper1,
                   reduce = red_f, ...)
    } else {
        x$initialize_partition()
        mapreduce( x = x, map = .range_mapper2,
                   reduce = red_f, ...)
    }
    
}

#' @describeIn S3-filearray get summation
#' @export
sum.FileArray <- function(x, na.rm = FALSE, ...){
    red_f <- function(x) {
        do.call('sum', x)
    }
    
    if( na.rm ){
        mapreduce( x = x, map = .sum_mapper1,
                   reduce = red_f, ...)
    } else {
        x$initialize_partition()
        mapreduce( x = x, map = .sum_mapper2,
                   reduce = red_f, ...)
    }
}

#' @describeIn S3-filearray get subset file array with formulae
#' @export
subset.FileArray <- function(x, ..., drop = FALSE){
    if(...length() == 0){
        stop("No filters to subset")
    }
    dnames <- dimnames(x)
    nms <- names(dnames)
    filters <- list(...)
    criteria <- list()
    for(ftr in filters){
        if(!is.call(ftr)){
            stop("All filters must be formula object, e.g `Var ~ Var < 10`")
        }
        name <- as.character(ftr[[2]])
        if(!name %in% nms){
            stop("Cannot find name `", name, "` in x")
        }
        expr <- ftr[[3]]
        if(length(criteria[[name]])){
            expr <- as.call(list(`&`, criteria[[name]], expr))
        }
        criteria[[name]] <- expr
    }
    dim <- dim(x)
    locs <- lapply(seq_along(dim), function(ii){
        if(ii > length(nms)){
            return(seq_len(dim[[ii]]))
        }
        name <- nms[[ii]]
        if(is.null(criteria[[name]])){
            return(seq_len(dim[[ii]]))
        }
        with(dnames, {
            eval(criteria[[name]])
        })
    })
    do.call(`[`, c(list(x = quote(x), drop = drop), locs))
}

#' @title A generic function of \code{which} that is \code{'FileArray'} compatible
#' @param x any R vector, matrix, array or file-array
#' @param val values to find
#' @param arr.ind logical; should array indices be 
#' returned when \code{x} is an array?
#' @param ... further passed to \code{\link{which}} or
#' \code{\link{arrayInd}}
#' @return The indices of \code{x} elements that are listed in \code{val}.
#' @examples 
#' 
#' 
#' # Default case
#' x <- array(1:27, rep(3,3))
#' fwhich(x, c(4,5))
#' 
#' # file-array case
#' arr <- filearray_create(tempfile(), dim(x))
#' arr[] <- x
#' fwhich(arr, c(4,5))
#' 
#' 
#' @export
fwhich <- function(x, val, arr.ind = FALSE, ...){
    UseMethod('fwhich')
}

#' @rdname fwhich
#' @export
fwhich.default <- function(x, val, arr.ind = FALSE, ...){
    which(x %in% val, arr.ind = arr.ind, ...)
}

#' @rdname fwhich
#' @export
fwhich.FileArray <- function(x, val, arr.ind = FALSE, ...){
    ret <- mapreduce(x, map = function(data, size, idx){
        if(size != length(data)){
            data <- data[seq_len(size)]
        }
        re <- which(data %in% val)
        if(length(re)){
            re <- re + (idx - 1)
        }
        re
    }, reduce = function(ret){
        do.call('c', ret)
    })
    if( arr.ind ){
        ret <- arrayInd(ind = ret, .dim = dim(x), 
                        .dimnames = dimnames(x), ...)
    }
    ret
}

## S4

#' @title The type of a file array (extended)
#' @param x any file array
#' @return A character string. The possible values are \code{"double"},
#' \code{"integer"}, \code{"logical"}, and \code{"raw"}
#' @export
setGeneric("typeof")

#' @rdname typeof
#' @export
setMethod('typeof', signature(x = "FileArray"), function(x){
    if(!x$valid()){
        stop("Invalid file array")
    }
    x$type()
})

#' @title Apply functions over file array margins (extended)
#' @param X a file array
#' @param MARGIN scalar giving the subscripts which the function will be applied over. Current implementation only allows margin size to be one
#' @param FUN the function to be applied
#' @param ... optional arguments to \code{FUN}
#' @param simplify a logical indicating whether results should be simplified if possible
#' @return See Section 'Value' in \code{\link[base]{apply}};
#' @export
setGeneric("apply")

#' @rdname apply
#' @export
setMethod(
    'apply', signature(X = "FileArray"), 
    function(X, MARGIN, FUN, ..., simplify = TRUE){
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
)

