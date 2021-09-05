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

