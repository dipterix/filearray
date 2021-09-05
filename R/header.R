write_header <- function(fid, partition, dimension, type, size){
    seek(con = fid, where = 0, rw = "write")
    writeBin(con = fid, object = 1.0, size = 8L, endian = ENDIANNESS)
    writeBin(con = fid, object = FILE_VER, size = 4L, endian = ENDIANNESS)
    
    sexp_type <- type_to_sexp(type)
    writeBin(con = fid, object = sexp_type, size = 4L,
             endian = ENDIANNESS)
    writeBin(con = fid, object = as.integer(size), size = 4L,
             endian = ENDIANNESS)
    
    # partition number
    partition <- as.double(partition)
    writeBin(con = fid, object = partition, size = 8L, endian = ENDIANNESS)
    
    # write length of dimension
    dimension <- as.double(dimension)
    len <- prod(dimension)
    writeBin(con = fid, object = len, size = 8L, endian = ENDIANNESS)
    writeBin(con = fid, object = length(dimension), size = 4L, 
             endian = ENDIANNESS)
    writeBin(con = fid, object = dimension, size = 8L, 
             endian = ENDIANNESS)
    header_len <- 48L + 8L * length(dimension)
    writeBin(con = fid, object = rep(0L, (HEADER_SIZE - header_len) / 4 - 3),
             size = 4L, endian = ENDIANNESS)
    writeBin(con = fid, object = header_len, size = 4L, endian = ENDIANNESS)
    writeBin(con = fid, object = 0.0, size = 8L, endian = ENDIANNESS)
    seek(con = fid, where = 0, rw = "write")
}

read_header <- function(fid){
    seek(con = fid, where = 0, rw = "read")
    
    one <- readBin(fid, n = 1, size = 8L, what = 'double', endian = ENDIANNESS)
    native <- one == 1
    
    if( native ){
        endian <- ENDIANNESS
    } else {
        if( ENDIANNESS == "little" ){
            endian <- 'big'
            stop("The file endianess is not little?")
        } else {
            endian <- 'little'
        }
    }
    
    version <- readBin(fid, what = 'int', n = 3, size = 4L, endian = endian)
    
    # type, size
    sexp_type <- readBin(fid, what = 'int', size = 4L, endian = endian)
    vsize <- readBin(fid, what = 'int', size = 4L, endian = endian)
    
    # partition number
    partition <- readBin(con = fid, what = 'double', size = 8L, endian = endian)
    
    # dimension
    size <- readBin(con = fid, what = 'double', size = 8L, endian = endian)
    ndims <- readBin(con = fid, what = 'int', size = 4L, endian = endian)
    dim <- readBin(con = fid, what = 'double', size = 8L, n = ndims, 
                   endian = endian)
    
    seek(con = fid, where = HEADER_SIZE - 12L, rw = "read")
    header_bytes <- readBin(con = fid, what = 'int', size = 4L, 
                            endian = endian)
    
    content_length <- readBin(con = fid, what = 'double', size = 8L, 
                              endian = endian)
    list(
        endianness = endian,
        version = version,
        sexp_type = sexp_type,
        unit_bytes = vsize,
        partition = partition,
        partition_size = size,
        partition_dim = dim,
        header_bytes = header_bytes,
        content_length = content_length
    )
}

validate_header <- function(file, fid){
    fz <- -1
    if(!missing(file)){
        if(!file.exists(file)){
            stop("File is missing")
        }
        fz <- file.size(file)
        if(fz < HEADER_SIZE){
            stop("Invalid `filearray` partition. File size too small.")
        }
        fid <- file(description = file, open = "rb")
        on.exit({
            close(fid)
        })
    }
    
    header <- read_header(fid)
    if( header$header_bytes != 48 + 8 * length(header$partition_dim) ){
        stop("Filearray partition header is corrupted.")
    }
    if( fz > 0 && header$content_length + HEADER_SIZE > fz ){
        stop("Filearray data is corrupted")
    }
    return(header)
}
