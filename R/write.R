
ensure_partition <- function(
    file, partition, dimension, 
    type = c("double","integer","logical","raw"), size = NULL){
    
    type <- match.arg(type)
    if(is.null(size)){
        size <- get_elem_size(type)
    } else {
        size <- as.integer(size)
    }
    
    if( !file.exists(file) ){
        fid <- file(description = file, open = "w+b")
        write_header(fid, partition, dimension, type, size)
        close(fid)
    }
    
    header <- validate_header(file)
    
    expected_type <- sexp_to_type(header$sexp_type)
    
    if( type != expected_type ){
        stop(sprintf("Partition data type mismatch: %s != %s", expected_type, type))
    }
    
    if( header$partition != partition ){
        quiet_warning(sprintf("Partition number mismatch: %s != %s", header$partition, partition))
    }
    if( prod(dimension) != header$partition_size ){
        quiet_warning(sprintf("Partition size mismatch: %s != %s", header$partition_size, prod(dimension)))
    }
    
    return(header)
    
}

sexp_to_type <- function(sexp){
    switch(
        as.character(sexp),
        '14' = 'double',
        '13' = 'integer',
        '10' = 'logical',
        '24' = 'raw',
        '15' = 'complex',
        '26' = 'float',
        stop("Unknown SEXP code: ", sexp)
    )
}

type_to_sexp <- function(type){
    switch(
        type,
        double = 14L,
        integer = 13L,
        logical = 10L,
        raw = 24L,
        complex = 15L,
        float = 26L,
        stop("Unknown data type: ", type)
    )
}

load_partition <- function(file, dim){
    stopifnot(file.exists(file))
    fid <- file(description = file, open = "rb")
    on.exit({
        close(fid)
    })
    header <- validate_header(fid = fid)
    type <- sexp_to_type(header$sexp_type)
    
    if( missing(dim) ){
        dim <- header$partition_dim
    } else {
        stopifnot(prod(header$partition_dim) == prod(dim))
    }
    
    structure(
        readBin(con = fid, what = type, size = header$unit_bytes, 
                n = header$content_length, endian = header$endianness),
        dim = dim
    )
}
