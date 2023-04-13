# DIPSAUS DEBUG START
# verbose <- TRUE
# z = filearray_create(temp_path(), c(2,3,4))
# z[] <- 1:24
# aaa <- z+1
# print(aaa[,,4])
# idx <- as_filearray(z[] > 2)
# 
# print(z[idx])
# print(aaa[idx])

# Normal subset via indices
fa_subset1 <- function(x, ..., drop = TRUE, reshape = NULL, strict = TRUE, dimnames = TRUE, split_dim = 0) {
    if(!x$valid()){
        stop("Invalid file array")
    }
    drop <- isTRUE(drop)
    # file <- temp_path(); x <- filearray_create(file, c(300, 400, 100, 1))
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
            stop("Subset FileArray only allows x[], x[i:j] or x[i,j,...] (single logical index like x[c(TRUE, ...)] is not allowed)")
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

# Special subset using another array
fa_subset2 <- function(x, i, ...) {
    
    single_index <- FALSE
    if(!is_filearray(i)) {
        if(is.array(i) && is.logical(i)) {
            if(!is_same_dim(x, i)) {
                stop("x[i]: `x` and `i` must share the same dimension when i is a logical array.")
            }
            i <- as_filearray(i)
        } else if (is.numeric(i) && is.vector(i)) {
            single_index <- TRUE
        } else {
            stop("x[i]: Cannot determine subset mode from `i`: must be a logical array or integer vector")
        }
    }
    
    if( single_index ) {
        stop("Single vector indexing (e.g. x[c(1,2,3,4,...)]) hasn't been implemented yet.")
        # re <- fastmap::fastqueue()
        # idx <- fastmap::fastmap()
        # idx$set("start_idx", 0L)
        # idx$set("idx_left", length(i))
        # 
        # 
        # fa_eval_ops(x, addon = function(env, data, uuid) {
        #     idx_left <- idx$get("idx_left")
        #     
        #     if( idx_left < 0 ) {
        #         return(NULL)
        #     }
        #     start_idx <- idx$get("start_idx")
        #     
        #     vec <- env[[ uuid ]]
        #     
        #     end_idx <- start_idx + length(vec)
        #     sel <- i[ i > start_idx & i <= end_idx] - start_idx
        #     if(length(sel)) {
        #         if(idx_left <= length(sel)) {
        #             sel <- sel[seq_len(idx_left)]
        #             re$add(vec[sel])
        #             idx_left <- 0L
        #         } else {
        #             re$add(vec[sel])
        #         }
        #         idx$set("idx_left", idx_left - length(sel))
        #     }
        #     idx$set("start_idx", start_idx)
        # })
        # re <- unlist(re$as_list())
        # 
    } else {
        re <- unlist(
            fmap2(
                x = list(x, i),
                fun = function(v) {
                    v[[1]][v[[2]]]
                }, 
                .simplify = FALSE
            )
        )
    }
    
    expected_mode <- operation_output_type(
        typeof(x), typeof(x), float = "double", logical = "logical")
    if(!identical(expected_mode, mode(re))) {
        mode(re) <- expected_mode
    }
    return(re)
    
}

#' @describeIn S3-filearray subset array
#' @export
`[.FileArray` <- function(x, i, ..., drop = TRUE, reshape = NULL, strict = TRUE, dimnames = TRUE, split_dim = 0) {
    x <- fa_eval_ops(x)
    if(missing(i)) {
        return(fa_subset1(x, , ..., drop = drop, reshape = reshape, strict = strict, dimnames = dimnames, split_dim = split_dim))
    } else {
        if(is_filearray(i) || is.array(i) || 
           ( ...length() == 0 && !is.logical(i) )) {
            return(fa_subset2(x, i))
        } else {
            return(fa_subset1(x, i, ..., drop = drop, reshape = reshape, strict = strict, dimnames = dimnames, split_dim = split_dim))
        }
    }
}



