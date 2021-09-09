# previous implementations in R

caster <-  function(type = c('double', 'integer', 'logical', 'raw')){
    type <- match.arg(type)
    switch(
        type,
        double = function(x){ 
            if(typeof(x) == "double"){
                return(as.vector(x))
            } else {
                return(as.double(x))
            }
        },
        integer = function(x){
            if(typeof(x) == "integer"){
                return(as.vector(x))
            } else {
                return(as.integer(x))
            }
        },
        logical = function(x){
            na <- as.raw(2)
            if(typeof(x) == "logical"){
                re <- as.vector(x)
            } else {
                re <- as.logical(x)
            }
            re[is.na(re)] <- 2L
            ret <- as.raw(re)
            ret
        },
        raw = function(x){
            if(typeof(x) == "raw"){
                return(as.vector(x))
            } else {
                return(as.raw(x))
            }
        },
        stop("Unknown data type: ", type))
}



write_partition <- function(
    file, partition, dimension, value,
    type = c("double","integer","logical","raw"), 
    size = NULL
){
    stopifnot(length(value) == prod(dimension))
    type <- match.arg(type)
    header <- ensure_partition(file, partition, dimension, type, size)
    
    fid <- file(description = file, open = "r+b")
    on.exit({
        try({ close(fid) }, silent = TRUE)
    }, after = FALSE, add = TRUE)
    
    write_seq(fid, 0L, value, length(value), header$unit_bytes, type)
    
    seek(con = fid, where = HEADER_SIZE - 8L, rw = "write")
    writeBin(con = fid, object = as.double(length(value)), size = 8L, 
             endian = header$endianness)
    
    close(fid)
    return(invisible())
}

write_seq = function(fid, start, value, total_len, size, type){
    stopifnot( start >= 0L )
    stopifnot( start+length(value) <= total_len )
    seek(con = fid, where = (start)*size + HEADER_SIZE, rw = "write")
    
    f_caster <- caster(type = type)
    
    # Writing data of non-naitive size is slow in R. (Why?)
    # This is solved by writing RAW data after using
    # writeBin to convert it into memory vector.
    if( ((size!=8) && (type=="double")) || 
        ((size!=4) && (type=="integer")) ){
        addwrite = function(value){
            tmp = writeBin(
                con = raw(),
                object = f_caster(value),
                size = size,
                endian = ENDIANNESS)
            writeBin(con = fid, object = tmp)
        }
    } else {
        addwrite = function(value){
            writeBin(
                con = fid,
                object = f_caster(value),
                size = size,
                endian = ENDIANNESS)
        }
    }
    
    # Writing long vectors is currently NOT supported 
    # (as of R 3.2.2, 3.3.0).
    # Thus write in pieces of 128 MB or less.
    if(length(value)*as.numeric(size) < 134217728){
        addwrite(value)
    } else {
        step1 = 134217728 %/% size
        mm = length(value)
        nsteps = ceiling(mm/step1)
        for( part in 1:nsteps ){ # part = 1
            # cat( part, "of", nsteps, "\n")
            fr = (part-1)*step1 + 1
            to = min(part*step1, mm)
            
            addwrite(value[fr:to])
        }
        rm(part, step1, mm, nsteps, fr, to)
    }
    # Instead of flush:
    seek(con = fid, where = 0, rw = "write")
    return(invisible())
}

#' @describeIn S3-filearray get element by position
#' @export
`[.FileArray` <- function(x, ..., drop = TRUE, reshape = NULL, strict = TRUE) {
    if(!x$valid()){
        stop("Invalid file array")
    }
    drop <- isTRUE(drop)
    # file <- tempfile(); x <- filearray_create(file, c(300, 400, 100, 1))
    filebase <- paste0(x$.filebase, x$.sep)
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
    max_buffer <- max_buffer_size() / elem_size
    
    if(length(listOrEnv) == length(dim)){
        idxrange <- sapply(listOrEnv, function(x){
            if(!length(x) || all(is.na(x))){ return(1L) }
            rg <- range(x, na.rm = TRUE)
            return(rg[2] - rg[1] + 1)
        })
    } else {
        idxrange <- dim
    }
    
    # sapply(seq_len(length(dim) - 1), function(split_dim){
    #     idx1dim <- dim[seq_len(split_dim)]
    #     idx1dim[[split_dim]] <- idxrange[[split_dim]]
    #     idx1len <- prod(idx1dim)
    #     idx2len <- prod(dim[-seq_len(split_dim)])
    #     nloops <- ceiling(idx1len / max_buffer)
    #     (idx1len * nloops) * idx2len
    # })
    
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
    
    # set buffer size
    idx1len <- prod(dim[seq_len(split_dim)])
    buffer_sz <- idx1len * elem_size
    buffer_sz <- ifelse(buffer_sz > max_buffer, max_buffer, buffer_sz)
    
    current_bsz <- get_buffer_size()
    on.exit({
        set_buffer_size(current_bsz)
    })
    set_buffer_size(buffer_sz)
    
    dnames <- NULL
    if(is.null(reshape)){
        dnames <- x$dimnames()
        if(length(dnames)){
            dnames <- structure(
                lapply(seq_along(dnames), function(ii){
                    if(length(dnames[[ii]])){
                        dnames[[ii]][listOrEnv[[ii]]]
                    } else {
                        NULL
                    }
                }),
                names = names(dnames)
            )
        }
    }
    
    re <- FARR_subset(
        filebase = filebase,
        type = x$sexp_type(),
        listOrEnv = listOrEnv,
        dim = dim,
        cum_part_sizes = x$.partition_info[, 3],
        reshape = reshape,
        drop = drop,
        strict = strict, 
        split_dim = split_dim,
        dimnames = dnames
    )
}