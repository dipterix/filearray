
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
#' @param val values to find, or a function taking one argument (a slice of 
#' data vector) and returns either logical vector with the same length as the 
#' slice or index of the slice; see 'Examples'
#' @param arr.ind logical; should array indices be 
#' returned when \code{x} is an array?
#' @param ret.values whether to return the values of corresponding indices as 
#' an attributes; default is false
#' @param ... passed to \code{val} if \code{val} is a function
#' @return The indices of \code{x} elements that are listed in \code{val}.
#' @examples 
#' 
#' 
#' # ---- Default case ------------------------------------
#' x <- array(1:27 + 2, rep(3,3))
#' 
#' # find index of `x` equal to either 4 or 5
#' fwhich(x, c(4,5))
#' res <- fwhich(x, c(4,5), ret.values = TRUE)
#' res
#' attr(res, "values")
#' 
#' # ---- file-array case --------------------------------
#' arr <- filearray_create(tempfile(), dim(x))
#' arr[] <- x
#' fwhich(arr, c(4,5))
#' fwhich(arr, c(4,5), arr.ind = TRUE, ret.values = TRUE)
#' 
#' arr[2:3, 1, 1]
#' 
#' # Clean up this example
#' arr$delete()
#' 
#' # ---- `val` is a function ----------------------------
#' x <- as_filearray(c(sample(15), 15), dimension = c(4,4))
#' 
#' ret <- fwhich(x, val = which.max, 
#'               ret.values = TRUE, arr.ind = FALSE)
#' 
#' # ret is the index
#' ret == which.max(x[])
#' 
#' # attr(ret, "values") is the max value
#' max(x[]) == attr(ret, "values")
#' 
#' # customize `val`
#' fwhich(x, ret.values = TRUE, arr.ind = FALSE,
#'        val = function( slice ) {
#'            slice > 10 # or which(slice > 10)
#'        })
#' 
#' 
#' @export
fwhich <- function(x, val, arr.ind = FALSE, ret.values = FALSE, ...){
    UseMethod('fwhich')
}

#' @rdname fwhich
#' @export
fwhich.default <- function(x, val, arr.ind = FALSE, ret.values = FALSE, ...){
    if(is.function(val)) {
        idx <- val(x, ...)
        if(is.logical(idx)) {
            idx <- which(idx, useNames = FALSE)
        }
    } else {
        idx <- which(x %in% val, arr.ind = FALSE, useNames = FALSE)
    }
    if( arr.ind && length(dim(x)) > 0 ) {
        re <- arrayInd(idx, .dim = dim(x), .dimnames = dimnames(x))
    } else {
        re <- idx
    }
    if( ret.values ) {
        attr(re, "values") <- x[idx]
    }
    return(re)
}

#' @rdname fwhich
#' @export
fwhich.FileArray <- function(x, val, arr.ind = FALSE, ret.values = FALSE, ...){
    if(is.function(val)) {
        args <- list(1, ...)
        impl <- function(y) {
            args[[1]] <- y
            do.call(val, args)
        }
    } else {
        impl <- function(y) {
            which(y %in% val, arr.ind = FALSE)
        }
    }
    mapreduce(
        x, 
        map = function(data, size, start_index){
            idx <- impl(data[seq_len(size)])
            if(is.logical(idx)) { idx <- which(idx, arr.ind = FALSE, useNames = FALSE) }
            if(!length(idx)) { return(NULL) }
            data.frame(
                index = start_index + idx - 1,
                value = data[idx]
            )
        },
        reduce = function(mapped_list) {
            mapped_data <- do.call("rbind", mapped_list)
            if(!length(mapped_data)) {
                idx <- numeric(0L)
                val <- raw(0L)
                storage.mode(val) <- storage.mode(x$.na)
            } else {
                mapped_data <- mapped_data[impl( mapped_data$val ), , drop = FALSE]
                idx <- mapped_data$index
                val <- mapped_data$value
            }
            
            if( arr.ind ) {
                idx <- arrayInd(idx, .dim = dim(x), .dimnames = dimnames(x))
            }
            if( ret.values ) {
                attr(idx, "values") <- val
            }
            idx
        }
    )
}

