
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
#' @param .env environment to evaluate formula when evaluating subset margin indices.
NULL


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
subset.FileArray <- function(x, ..., drop = FALSE, .env = parent.frame()){
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
        eval(criteria[[name]], envir = dnames, enclos = .env)
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
