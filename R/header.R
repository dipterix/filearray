#' Position (size): description - example value
#' 0 (8): determine endianness - 1.0 (double)
#' 8 (4 x 3): version number - [1, 0, 0] (int[3])
#' 20 (4): SEXP type - 14L (SEXPTYPE, data are double type)
#' 24 (4): element size - 8L (int, data element size is 8)
#' 28 (8): number of last dimensions per partition - 1 (double)
#' 36 (8): total number of elements - 24 (double)
#' 44 (4): number of dimensions (ndims) - 3 (int)
#' 48 (8 x ndims): dimensions - [2, 3, 4] (double)
#' 
#' ...
#' 
#' 1012 (4): size of the header - 72 (int)
#' 1016 (8): number of elements in the file - (double)
#' 
#' @noRd
NULL

write_header <- function(fid, partition, dimension, type, size){
    seek(con = fid, where = 0, rw = "write")
    
    # 0 (8): determine endianness - 1.0 (double)
    writeBin(con = fid, object = 1.0, size = 8L, endian = ENDIANNESS)
    
    # 8 (4 x 3): version number - [1, 0, 0] (int[3])
    writeBin(con = fid, object = FILE_VER, size = 4L, endian = ENDIANNESS)
    
    sexp_type <- type_to_sexp(type)
    
    # 20 (4): SEXP type - 14L (SEXPTYPE, data are double type)
    writeBin(con = fid, object = sexp_type, size = 4L,
             endian = ENDIANNESS)
    
    # 24 (4): element size - 8L (int, data element size is 8)
    writeBin(con = fid, object = as.integer(size), size = 4L,
             endian = ENDIANNESS)
    
    # partition number
    partition <- as.double(partition)
    
    # 28 (8): number of last dimensions per partition - 1 (double)
    writeBin(con = fid, object = partition, size = 8L, endian = ENDIANNESS)
    
    ## # write length of dimension
    dimension <- as.double(dimension)
    len <- prod(dimension)
    
    # 36 (8): total number of elements - 24 (double)
    writeBin(con = fid, object = len, size = 8L, endian = ENDIANNESS)
    
    # 44 (4): number of dimensions (ndims) - 3 (int)
    writeBin(con = fid, object = length(dimension), size = 4L, 
             endian = ENDIANNESS)
    
    # 48 (8 x ndims): dimensions - [2, 3, 4] (double)
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
            # TODO: support big-endian file? maybe not
            stop("The file endianess is not little?")
        } else {
            endian <- "little"
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
            # Might be on windows and partition files are symlinked
            if(get_os() != "windows" || fz != 0){
                stop("Invalid `filearray` partition. File size too small:\n  ", file)
            }
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

set_meta_content <- function(meta_file, data){
    stopifnot(file.exists(meta_file))
    data <- as.list(data)
    data$header_version <- HEADER_VER
    
    keys <- names(data)
    names(data) <- sprintf("__%s__", keys)
    
    conn <- rawConnection(raw(), "w+b")
    saveRDS(file = conn, data, ascii = FALSE)
    v <- rawConnectionValue(conn)
    close(conn)
    
    fid <- file(meta_file, "r+b")
    on.exit({ close(fid) })
    seek(con = fid, where = HEADER_SIZE, origin = "start", rw = "write")
    writeBin(object = v, con = fid, endian = ENDIANNESS)
    
    seek(con = fid, where = HEADER_SIZE - 8L, origin = "start", rw = "write")
    writeBin(con = fid, object = as.double(length(v)), size = 8L, endian = ENDIANNESS)
    seek(con = fid, where = 0, rw = "write")
}

