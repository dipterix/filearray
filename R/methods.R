
## S3

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
