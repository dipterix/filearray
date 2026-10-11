
ensure_partition <- function(
    file, partition, dimension, 
    type = c("double", "integer", "logical", "raw"), size = NULL) {
    
    type <- match.arg(type)
    if (is.null(size)) {
        size <- get_elem_size(type)
    } else {
        size <- as.integer(size)
    }
    
    if ( !file.exists(file) ) {
        fid <- file(description = file, open = "w+b")
        write_header(fid, partition, dimension, type, size)
        close(fid)
    }
    
    header <- validate_header(file)
    
    expected_type <- sexp_to_type(header$sexp_type)
    
    if ( type != expected_type ) {
        stop(sprintf("Partition data type mismatch: %s != %s", expected_type, type))
    }
    
    if ( header$partition != partition ) {
        quiet_warning(sprintf("Partition number mismatch: %s != %s", header$partition, partition))
    }
    if ( prod(dimension) != header$partition_size ) {
        quiet_warning(sprintf("Partition size mismatch: %s != %s", header$partition_size, prod(dimension)))
    }
    
    return(header)
    
}

# Writes partition `part` of `x` filled with `value` through a temporary file.
# `replace = TRUE` replaces an existing partition; `replace = FALSE` only
# creates a missing one. `locked`: whether this session holds the partition
# lock, so that temporary files of killed writers can be removed
write_partition_file <- function(x, part, value, replace = TRUE, locked = FALSE) {
    value <- value[[1]]
    type <- x$type()
    switch(
        type,
        "complex" = {
            value <- cplxToReal2(as.complex(value))
        },
        "float" = {
            value <- realToFloat2(as.double(value))
        },
        {
            storage.mode(value) <- type
        }
    )
    size <- get_elem_size(type)
    file <- x$partition_path(part)
    if (locked) {
        remove_stale_partition_tmp(x, part)
    }

    # Write to a temporary file, then publish it as the partition: another
    # process that loads this array while the partition is being written
    # would reject a file shorter than the 1024-byte header. The name is new
    # for every call, so writers never share a temporary file; `load()` lists
    # only `<n>.farr`
    tmp <- file.path(x$.filebase, sprintf("%d.%s.tmp", as.integer(part) - 1L,
                                          uuid::UUIDgenerate()))
    fid <- file(tmp, "wb")
    fid_closed <- FALSE

    on.exit({
        if (!fid_closed) {
            close(fid)
        }
        # `tmp` is left if writing or publishing failed, or if the partition
        # already existed
        if (file.exists(tmp)) {
            unlink(tmp)
        }
    }, add = TRUE)

    if (part <= nrow(x$.partition_info)) {
        partition_size <- x$.partition_info[part, 2]
    } else {
        partition_size <- x$partition_size()
    }
    dimension <- x$dimension()
    dimension[[length(dimension)]] <- partition_size
    part_len <- prod(dimension)
    buffer_len <- get_buffer_size() / size
    if (buffer_len > part_len) {
        buffer_len <- part_len
    }

    write_header(
        fid = fid,
        partition = part,
        dimension = dimension,
        type = type,
        size = size
    )
    seek(con = fid, where = HEADER_SIZE, rw = "write")
    buf <- writeBin(
        con = raw(),
        object = rep(value, buffer_len),
        size = size,
        endian = ENDIANNESS
    )
    nloop <- floor(part_len / buffer_len)
    replicate(nloop, {
        writeBin(con = fid, object = buf)
        NULL
    })
    rest <- part_len - buffer_len * nloop
    if ( rest > 0 ) {
        writeBin(con = fid, object = buf[seq_len(rest * size)])
    }
    seek(con = fid, where = HEADER_SIZE - 8L, rw = "write")
    writeBin(con = fid, object = part_len, size = 8L, endian = ENDIANNESS)

    close(fid)
    fid_closed <- TRUE

    if (replace) {
        # rename() replaces `file` in one step, so other processes see either
        # the old partition or the new one, never a partial one
        if (!file.rename(tmp, file)) {
            stop("Cannot write partition file: ", file)
        }
        return(invisible(TRUE))
    }
    invisible(publish_partition_file(tmp, file))
}

# Publishes `tmp` as the partition file `file` unless that exists. A hard link
# is refused by the file system (on NFS, by the server) when `file` exists,
# so a partition another process has created and written is never replaced,
# even when this host still caches it as missing. Returns FALSE if `file`
# already existed
publish_partition_file <- function(tmp, file) {
    status <- FARR_link_noreplace(lock_os_path(tmp), lock_os_path(file))
    code <- as.integer(status)
    if (identical(code, LINK_CREATED)) {
        unlink(tmp)
        return(TRUE)
    }
    if (identical(code, LINK_EXISTS)) {
        return(FALSE)
    }
    if (!identical(code, LINK_UNSUPPORTED)) {
        stop("Cannot write partition file: ", file, " (", attr(status, "message"), ")")
    }
    # No hard links on this file system: rename, unless the partition
    # appeared meanwhile
    if (file.exists(file)) {
        return(FALSE)
    }
    if (!file.rename(tmp, file)) {
        stop("Cannot write partition file: ", file)
    }
    TRUE
}

# Removes temporary files that writers of partition `part` left behind when
# they were killed. Writers that do not lock are never this slow, so their
# temporary files are safe
remove_stale_partition_tmp <- function(x, part, age = STALE_TMP_AGE) {
    pattern <- sprintf(
        "^%d\\.[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}\\.tmp$",
        as.integer(part) - 1L
    )
    files <- list.files(x$.filebase, pattern = pattern, full.names = TRUE)
    if (!length(files)) {
        return(invisible())
    }
    mtime <- file.mtime(files)
    stale <- files[!is.na(mtime) &
                       difftime(Sys.time(), mtime, units = "secs") > age]
    if (length(stale)) {
        unlink(stale)
    }
    invisible()
}

sexp_to_type <- function(sexp) {
    switch(
        as.character(sexp),
        "14" = "double",
        "13" = "integer",
        "10" = "logical",
        "24" = "raw",
        "15" = "complex",
        "26" = "float",
        stop("Unknown SEXP code: ", sexp)
    )
}

type_to_sexp <- function(type) {
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

load_partition <- function(file, dim) {
    stopifnot(file.exists(file))
    fid <- file(description = file, open = "rb")
    on.exit({
        close(fid)
    })
    header <- validate_header(fid = fid)
    type <- sexp_to_type(header$sexp_type)
    
    if ( missing(dim) ) {
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
