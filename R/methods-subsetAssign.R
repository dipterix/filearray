fa_subsetAssign1 <- function(x, ..., value){
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

# setMethod("[<-",
#     signature(x = "Filearray"),
#     function (x, i, j, ..., value) 
#     {
#         stop("need a definition for the method here")
#     }
# )

# DIPSAUS DEBUG START
# verbose <- TRUE
# z = filearray_create(temp_path(), c(2,3,4))
# z[] <- 1:24
# x <- z+1
# i <- as_filearray(z[] < 5)

# `i` is a filearray
fa_subsetAssign2 <- function(x, i, value, label = "subset-assign (lazy)") {
    stopifnot(is_filearray(x) && is_filearray(i))
    
    if(typeof(i) != "logical") {
        stop("`fa_subsetAssign2`: subset index filearray must be logical")
    }
    
    e1 <- as_filearrayproxy(x)
    e2 <- as_filearrayproxy(i)
    uuid1 <- e1$uuid()
    uuid2 <- e2$uuid()
    e1$link_proxy( e2 )
    
    # check input types 
    out_type <- typeof(e1)
    
    globals <- fastmap::fastmap()
    globals$set("starting_idx", 0L)
    value_len <- length(value)
    
    op_func <- function(value_list, ...) {
        data <- value_list[[ uuid1 ]]
        
        idx <- value_list[[ uuid2 ]]
        n_assigned <- sum(idx)
        if(n_assigned == 0) { return(data) }
        
        if( value_len == 1L ) {
            data[ idx ] <- value
        } else {
            starting_idx <- globals$get("starting_idx")
            data[ idx ] <- value[starting_idx + seq_len(n_assigned)]
            globals$set("starting_idx", starting_idx + n_assigned)
        }
        
        return(data)
    }
    
    e1$add_operator(op_func,
                    out_type = out_type,
                    context = "array_subset_assign",
                    label = label)
    
    return( e1 )
}


#' @describeIn S3-filearray subset assign array
#' @export
`[<-.FileArray` <- function(x, i, ..., lazy = FALSE, value) {
    if(!x$valid()){
        stop("Invalid file array")
    }
    
    if(missing(i)) {
        if(is_fileproxy(x) && length(x$.ops) > 0) {
            x <- fa_eval_ops(x)
            x$.mode <- "readwrite"
        }
        return(fa_subsetAssign1(x, , ..., value = value))
    } else {
        if(is_filearray(i) || is.array(i) || 
           ( ...length() == 0 && !is.logical(i) )) {
            re <- fa_subsetAssign2(x, i, value = value)
            if(!lazy) {
                re <- fa_eval_ops(re)
                re$.mode <- "readwrite"
            }
            return(re)
        } else {
            if(is_fileproxy(x) && length(x$.ops) > 0) {
                x <- fa_eval_ops(x)
                x$.mode <- "readwrite"
            }
            return(fa_subsetAssign1(x, i, ..., value = value))
        }
    }
}