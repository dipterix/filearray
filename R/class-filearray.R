#' @title Definition of file array
#' @name FileArray-class
#' @description \code{S4} class definition of \code{FileArray}. Please 
#' use \code{\link{filearray_create}} and \code{\link{filearray_load}}
#' to create instances.
#' @section Public Methods:
#' \describe{
#' \item{\code{get_header(key, default = NULL)}}{Get header information; returns \code{default} if \code{key} is missing}
#' \item{\code{set_header(key, value)}}{Set header information; the extra headers will be stored in meta file. Please do not store large headers as they will be loaded into memory frequently.}
#' \item{\code{can_write()}}{Whether the array data can be altered}
#' \item{\code{create(filebase, dimension, type = "double", partition_size = 1)}}{Create a file array instance}
#' \item{\code{delete(force = FALSE)}}{Remove array from local file system and reset}
#' \item{\code{dimension()}}{Get dimension vector}
#' \item{\code{dimnames(v)}}{Set/get dimension names}
#' \item{\code{element_size()}}{Internal storage: bytes per element}
#' \item{\code{fill_partition(part, value)}}{Fill a partition with given scalar}
#' \item{\code{get_partition(part, reshape = NULL)}}{Get partition data, and reshape (if not null) to desired dimension}
#' \item{\code{expand(n)}}{Expand array along the last margin; returns true if expanded; if the \code{dimnames} have been assigned prior to expansion, the last dimension names will be filled with \code{NA}}
#' \item{\code{initialize_partition()}}{Make sure a partition file exists; if not, create one and fill with \code{NA}s or 0 (\code{type='raw'})}
#' \item{\code{load(filebase, mode = c("readwrite", "readonly"))}}{Load file array from existing directory}
#' \item{\code{lock_partition(parts, timeout = NULL)}}{Lock partitions \code{parts} (default: all) for this \R session, so that other processes cannot write to them; waits and retries until all of them are locked or \code{timeout} seconds have passed (\code{NULL}: option \code{filearray.lock.timeout}, which defaults to waiting without limit; \code{0}: try once). Returns \code{TRUE} once locked, or \code{FALSE} after the time-out, with nothing locked; see section 'Partition locks'}
#' \item{\code{partition_path(part)}}{Get partition file path}
#' \item{\code{partition_size()}}{Get partition size; see \code{\link{filearray}}}
#' \item{\code{set_partition(part, value, ..., strict = TRUE)}}{Set partition value}
#' \item{\code{sexp_type()}}{Get data \code{SEXP} type; see R internal manuals}
#' \item{\code{show()}}{Print information}
#' \item{\code{type()}}{Get data type}
#' \item{\code{unlock_partition(parts)}}{Release one \code{lock_partition} lock on each of \code{parts}, or every \code{lock_partition} lock this \R session holds on the array if \code{parts} is missing; returns \code{TRUE} if a lock was released}
#' \item{\code{valid()}}{Check if the array is valid.}
#' }
#' @section Partition locks:
#' Several processes (for example \pkg{parallel}, \pkg{callr} or \pkg{mirai}
#' workers) can write to one array. Writes lock the partitions they change:
#' sub-assignment (\code{x[...] <- value}), \code{fill_partition},
#' \code{initialize_partition}, \code{set_partition}, \code{\link{fmap}},
#' \code{\link{fmap_element_wise}} and \code{\link{filearray_bind}} wait
#' while another process holds one of their partitions, so concurrent writers
#' never lose each other's data. Reading takes no locks, so a reader may see a
#' write that is still in progress. Functions that read but must first create
#' missing partitions (\code{max}, \code{min}, \code{range} and \code{sum}
#' with \code{na.rm = FALSE}, inputs of \code{fmap}, \code{filearray_bind},
#' evaluating array operations) lock the partitions they create, so they can
#' wait too.
#'
#' To keep partitions to yourself over several operations, for example to
#' check whether a partition has been computed and write it only if not, lock
#' them explicitly:
#' \preformatted{
#' if (!x$lock_partition(k, timeout = 60)) stop("partition ", k, " is busy")
#' on.exit(x$unlock_partition(k), add = TRUE)
#' if (is.na(x[1, 1, k])) x[, , k] <- compute(k)
#' }
#' Locks belong to the \R session, not to the object: every object of the
#' same array shares them, writes of this session to partitions it holds do
#' not wait, and each \code{lock_partition} call needs one
#' \code{unlock_partition} call. Lock everything you need in one call: two
#' sessions that each hold one partition and wait for the other's wait until
#' their time-outs. Likewise, do not hold partitions that forked workers
#' (\code{mclapply}) of the same session write. Non-interactive workers
#' should set a time-out.
#'
#' The locks are byte-range locks on an empty file \code{.filearray.lock} in
#' the array directory (byte \code{k} locks partition \code{k}). They are
#' advisory: older versions of this package and other programs ignore them.
#' The operating system releases them when a process exits, so a crashed or
#' killed process never leaves a stale lock. On file systems that cannot lock
#' (for example some network mounts) writes warn once and proceed unlocked;
#' cloud-synchronized folders lock on one computer only. Copying or archiving
#' the array directory in the session that holds its locks releases them on
#' \verb{POSIX} systems.
#'
#' Options (each falls back to the environment variable, which worker
#' processes inherit):
#' \describe{
#' \item{\code{filearray.lock} (\code{FILEARRAY_LOCK})}{set to \code{FALSE}
#' to stop writes from locking automatically; \code{lock_partition} still
#' locks}
#' \item{\code{filearray.lock.timeout} (\code{FILEARRAY_LOCK_TIMEOUT})}{seconds
#' a write waits for a locked partition before it fails with an error of class
#' \code{filearray_lock_timeout}; default \code{Inf}, wait until the
#' partition is free}
#' }
#' @seealso \code{\link{filearray}}
#' @exportClass FileArray
NULL

initialize_filearray <- function(path, dimension, partition_size, type) {
    stopifnot(!dir.exists(path))
    dir.create(path, showWarnings = FALSE)
    meta <- file.path(path, "meta")
    conn <- file(meta, "w+b")
    on.exit({
        try({close(conn)}, silent = TRUE)
    })
    size <- get_elem_size(type = type)
    write_header(conn, partition = partition_size, dimension = dimension, 
                 type = type, size = size)
    close(conn)
}

load_meta <- function(path) {
    meta <- file.path(path, "meta")
    stopifnot(file.exists(meta))
    header <- validate_header(meta)
    
    if (header$content_length > 0) {
        # load dimnames
        fid <- file(meta, "rb")
        on.exit({ close(fid) })
        seek(con = fid, where = HEADER_SIZE, origin = "start", rw = "read")
        v <- readBin(con = fid, what = "raw", size = 1L, 
                     n = header$content_length)
        conn <- rawConnection(v, open = "rb")
        
        # .self$.header[c("dimnames", "symlinks")]
        extra_header <- readRDS(conn)
        close(conn)
        if (is.list(extra_header)) {
            extra_header_names <- names(extra_header)
            if ("__header_version__" %in% extra_header_names) {
                extra_header_names <- sub("^__", "", extra_header_names)
                extra_header_names <- sub("__$", "", extra_header_names)
                extra_header_names <- extra_header_names[!extra_header_names %in% RESERVED_HEADERS]
                extra_header_names <- c(extra_header_names, "dimnames", "header_version")
                for (nm in extra_header_names) {
                    header[[nm]] <- extra_header[[sprintf("__%s__", nm)]]
                }
            } else {
                # old format
                header$dimnames <- extra_header
                header$header_version <- 0
            }
        } else {
            header$header_version <- 0
        }
    } else {
        header$header_version <- 0
    }
    header
}

get_elem_size <- function(type) {
    switch(
        type,
        double = 8L,
        float = 4L,
        integer = 4L,
        logical = 1L,
        raw = 1L,
        complex = 8L,
        stop("Unknown data type: ", type)
    )
}

FileArray <- setRefClass(
    "FileArray",
    fields = list(
        .mode = "character",
        .sep = "character",
        .filebase = "character",
        .header = "list",
        .partition_info = "ANY",
        .na = "ANY",
        .valid = "logical"
    ),
    methods = list(
        initialize = function() {
            .self$.mode <- "readonly"
            .self$.sep <- .Platform$file.sep
            .self$.filebase <- "."
            .self$.header <- list()
            .self$.partition_info <- NULL
            .self$.na <- NA_real_
            .self$.valid <- FALSE
        },
        create = function(filebase, dimension, type = "double",
                          partition_size = 1) {
            dir <- dirname(filebase)
            if (!dir.exists(dir)) {
                stop("Path not exists: ", dir)
            }
            partition_size <- as.integer(partition_size)
            if (is.na(partition_size) || partition_size < 1) {
                partition_size <- 1
            }
            
            if (dir.exists(filebase)) {
                stop("To create a file array, the path must be empty.")
            }
            stopifnot(length(dimension) >= 2)
            
            # create
            initialize_filearray(
                path = filebase,
                dimension = dimension,
                partition_size = partition_size,
                type = type
            )
            .self$load(filebase, "readwrite")
        },
        dimension = function() {
            .self$.header$partition_dim
        },
        dimnames = function(v) {
            if (!missing(v)) {
                dim <- .self$.header$partition_dim
                stopifnot(is.list(v) || length(v) <= length(dim))
                for (ii in seq_along(v)) {
                    if (length(v[[ii]]) && length(v[[ii]]) != dim[[ii]]) {
                        stop("Dimension ", ii, " length mismatch")
                    }
                }
                nms <- names(v)
                if (!is.null(nms) && length(nms) < length(dim)) {
                    nms <- c(nms, rep("", length(dim) - length(nms)))
                }
                v <- structure(lapply(seq_along(dim), function(ii) {
                    if (ii > length(v)) { return(NULL) }
                    v[[ii]]
                }), names = nms)
                # set dimnames
                meta <- file.path(.self$.filebase, "meta")
                .self$.header$dimnames <- v
                .self$.save_header()
                .self$.header$dimnames
            }
            .self$.header$dimnames
        },
        type = function() {
            sexp_to_type(.self$.header$sexp_type)
        },
        sexp_type = function() {
            .self$.header$sexp_type
        },
        element_size = function() {
            .self$.header$unit_bytes
        },
        partition_size = function() {
            .self$.header$partition
        },
        .save_header = function() {
            if (.self$.mode != "readwrite") {
                quiet_warning("Cannot save extra headers because the array has no write access.")
                return(invisible())
            }
            # get header version
            keys <- names(.self$.header)
            keys <- keys[!keys %in% RESERVED_HEADERS | keys %in% c("dimnames")]
            extra_header <- .self$.header[keys]
            meta <- file.path(.self$.filebase, "meta")
            set_meta_content(meta, extra_header)
            if (!identical(as.integer(.self$.header$header_version), HEADER_VER)) {
                # old header
                .self$.header <- load_meta(.self$.filebase)
            }
            return(invisible())
        },
        get_header = function(key, default = NULL) {
            if (key %in% names(.self$.header)) {
                return(.self$.header[[key]])
            }
            return(default)
        },
        set_header = function(key, value, save = TRUE) {
            force(value)
            if (key %in% RESERVED_HEADERS) {
                stop("Key `", key, "` is preserved and should be read-only or altered via other methods.")
            }
            .self$.header[[key]] <- value
            if ( save ) {
                .self$.save_header()
            }
            invisible(value)
        },
        header_signature = function(include_path = TRUE) {
            header_sig <- digest::digest(.self$.header, algo = "sha256")
            if ( include_path ) {
                path <- normalizePath(.self$.filebase)
                header_sig <- digest::digest(c(
                    header_sig, path
                ), algo = "sha256")
            }
            header_sig
        },
        load = function(filebase, mode = c("readwrite", "readonly")) {
            mode <- match.arg(mode)
            
            filebase <- normalizePath(filebase, mustWork = TRUE)
            if (endsWith(filebase, "/|\\\\")) {
                filebase <- gsub("[/\\\\]+$", "", x = filebase)
            }
            
            # set members
            .self$.mode <- mode
            .self$.sep <- .Platform$file.sep
            .self$.filebase <- filebase
            .self$.valid <- TRUE
            
            # meta
            header <- load_meta(filebase)
            .self$.header <- header
            
            
            .self$.na <- switch(
                .self$type(),
                double = NA_real_,
                integer = NA_integer_,
                logical = FALSE,
                raw = as.raw(0),
                complex = NA_complex_,
                float = NA_real_,
                stop("Unknown data type: ", .self$type())
            )
            
            # load partition information
            files <- list.files(filebase, pattern = "^[0-9]+\\.farr$",
                                recursive = FALSE, all.files = FALSE, 
                                full.names = FALSE, ignore.case = TRUE, 
                                include.dirs = FALSE)
            dimension <- .self$dimension()
            partition_size <- .self$partition_size()
            margin <- dimension[[length(dimension)]]
            nparts <- ceiling(margin / partition_size)
            if (length(files)) {
                partition_info <- t(sapply(files, function(f) {
                    header <- validate_header(file = file.path(filebase, f))
                    c(header$partition, header$sexp_type, header$partition_dim)
                }))
                sel <- partition_info[, 2] != .self$sexp_type()
                if (any(sel)) {
                    stop(
                        sprintf(
                            "Expected data type: [%s], but the following partition files disagree [%s]",
                            .self$type(),
                            paste(partition_info[sel, 1], collapse = ", ")
                        )
                    )
                }
                expected_part <- as.integer(gsub("[^0-9]+", "", files)) + 1
                if (!all(expected_part == partition_info[, 1])) {
                    # check if this array is a bound array
                    bind_info <- .self$.header$filearray_bind
                    if (is.list(bind_info) && isTRUE(bind_info$is_bound)) {
                        partition_info[, 1] <- seq_along(partition_info[, 1])
                        if (!all(expected_part == partition_info[, 1])) {
                            stop("Partition filenames mismatch with partition headers.")
                        }
                        if (bind_info$symlink &&
                            .self$.mode != "readonly") {
                            quiet_warning(
                                "Partition filenames mismatch with partition headers. This happens when the array partitions are symlinked from other arrays. For safety reasons, switched to read-only mode."
                            )
                            .self$.mode <- "readonly"
                        }
                    } else {
                        stop("Partition filenames mismatch with partition headers.")
                    }
                    
                }
                
                nparts <- max(c(nparts, partition_info[, 1]))
                pinfo <- cbind(seq_len(nparts), partition_size)
                pinfo[partition_info[, 1], 2] <- partition_info[, ncol(partition_info)]
                cs <- cumsum(pinfo[, 2])
                idx <- max(which(cs <= margin))
                if (cs[idx] < margin) { idx <- idx + 1 }
                pinfo <- pinfo[seq_len(idx), , drop = FALSE]
            } else {
                pinfo <- cbind(seq_len(nparts), partition_size)
            }
            # need to make sure cumpart-size is correct
            cpl <- cumsum(pinfo[, 2])
            nr <- sum(cpl <= margin)
            if (cpl[[nr]] < margin) {
                nr <- nr + 1
            }
            cpl <- cpl[seq_len(nr)]
            cpl[[nr]] <- margin
            pinfo <- cbind(pinfo[seq_len(nr), , drop = FALSE], cpl)
            .self$.partition_info <- pinfo
            return(.self)
        },
        show = function() {
            if (.self$valid()) {
                cat("Reference class object of class \"FileArray\"\n")
                cat("Mode:", .self$.mode, "\n")
                tryCatch({
                    cat("Dimension:", paste(.self$dimension(), collapse = "x"), "\n")
                    cat("Partition count:", nrow(.self$.partition_info), "\n")
                    cat("Partition size:", .self$partition_size(), "\n")
                    cat("Storage type: ", .self$type(), " (internal size: ", get_elem_size(.self$type()), ")\n", sep = "")
                    
                }, error = function(e) {
                    quiet_warning("Partition information is unavailable: might be broken or improperly set.", immediate. = FALSE)
                })
                cat("Location:", .self$.filebase, "\n")
            } else {
                if (.self$.valid) {
                    cat("The FileArray is invalid (it has not been initialized).")
                } else {
                    cat("The FileArray is invalid (unable to locate the array in the file system).")
                }
            }
            
        },
        partition_path = function(part) {
            part <- as.integer(part)
            file.path(.self$.filebase, sprintf("%d.farr", part - 1))
        },
        set_partition = function(part, value, ..., strict = TRUE) {
            if (isTRUE(.self$.mode == "readonly")) {
                stop("File array is read-only")
            }
            part <- as.integer(part)
            path <- .self$partition_path(part)
            dim <- .self$dimension()
            lastm <- dim[[length(dim)]]
            if ( is.na(part) || part < 1 || part > lastm ) {
                if ( strict ) {
                    stop("Invalid partition: ", part)
                } else {
                    return(FALSE)
                }
            }

            # Hold the partition from reading it to writing it back
            lock_scope <- lock_scope_begin()
            on.exit(lock_scope_end(lock_scope), add = TRUE)
            lock_scope_acquire(lock_scope, .self, part)

            arglen <- ...length()
            dim[[length(dim)]] <- .self$partition_size()
            if ( arglen > 1 ) {
                if (file.exists(path)) {
                    x <- load_partition(path, dim)
                } else {
                    x <- array(.self$.na, dim = dim)
                }
                x[...] <- value
                value <- x
            }
            
            # Edit start
            end <- .self$.partition_info[part, 3]
            if (part == 1) {
                start <- 1
            } else {
                start <- .self$.partition_info[part - 1, 3] + 1
            }
            
            tmp <- rep("", length(dim))
            tmp[[length(tmp)]] <- "seq.int(start, end)"
            expr <- sprintf(".self[%s] <- value", paste(tmp, collapse = ","))
            
            eval(parse(text = expr))
            # Edit end
            # Old implementation
            # write_partition(file = path, partition = part, dimension = dim, value = value, type = .self$type())
            
            return(TRUE)
        },
        get_partition = function(part, reshape = NULL) {
            if (is.null(reshape)) {
                reshape <- .self$dimension()
                reshape[[length(reshape)]] <- .self$partition_size()
            }
            return(load_partition(.self$partition_path(part), dim = reshape))
        },
        expand = function(n) {
            if (isTRUE(.self$.mode == "readonly")) {
                stop("File array is read-only")
            }
            
            # get the last margin size
            dim <- .self$dimension()
            ndims <- length(dim)
            last_dim <- dim[[ndims]]
            if (last_dim >= n) {
                return(FALSE)
            }
            dnames <- .self$.header$dimnames
            if (is.list(dnames) && length(dnames) >= ndims) {
                ldn <- tryCatch({
                    ldn <- dnames[[ndims]]
                    c(ldn, rep(NA, n - length(ldn)))
                }, error = function(e) {
                    NULL
                })
                dnames[[ndims]] <- ldn
            }
            meta <- file.path(.self$.filebase, "meta")
            fid <- file(meta, "r+b")
            on.exit({
                try({close(fid)}, silent = TRUE)
            }, add = TRUE)
            
            
            dim[[ndims]] <- n
            type <- sexp_to_type(.self$.header$sexp_type)
            write_header(fid = fid, partition = .self$.header$partition, 
                         dimension = dim, type = type, 
                         size = get_elem_size(type))
            close(fid)
            
            .self$load(.self$.filebase, mode = .self$.mode)
            .self$dimnames(dnames)
            return(TRUE)
            
        },
        fill_partition = function(part, value) {
            if (isTRUE(.self$.mode == "readonly")) {
                stop("File array is read-only")
            }
            if (!.self$valid()) {
                stop("Invalid file array")
            }
            part <- as.integer(part)
            if (length(part) != 1) {
                stop("`fill_partition` only allows one partition at a time")
            }
            if (is.na(part) || part <= 0) {
                stop("NA or non-positive partition are invalid")
            }
            if (length(value) > 1) {
                quiet_warning("`fill_partition` value length coerced to first value")
            }

            lock_scope <- lock_scope_begin()
            on.exit(lock_scope_end(lock_scope), add = TRUE)
            locked <- lock_scope_acquire(lock_scope, .self, part)
            write_partition_file(.self, part, value, replace = TRUE, locked = locked)
            invisible()
        },
        initialize_partition = function(parts) {
            if (!.self$valid()) {
                stop("Invalid file array")
            }
            if (missing(parts)) {
                parts <- .self$.partition_info[, 1]
            }
            parts <- parts[!is.na(parts)]
            # Read APIs (max, min, range, sum, filearray_bind, ...) call this:
            # when every partition exists, they return here without locking
            parts <- parts[!file.exists(.self$partition_path(parts))]
            if (!length(parts)) {
                return(invisible())
            }
            parts <- sort(unique(as.integer(parts)))
            if (any(parts <= 0)) {
                stop("NA or non-positive partition are invalid")
            }

            lock_scope <- lock_scope_begin()
            on.exit(lock_scope_end(lock_scope), add = TRUE)
            locked <- lock_scope_acquire(lock_scope, .self, parts)
            for (part in parts) {
                # Another process may have created the partition meanwhile;
                # write_partition_file() never replaces one that exists
                if (!file.exists(.self$partition_path(part))) {
                    write_partition_file(.self, part, NA, replace = FALSE, locked = locked)
                }
            }
            invisible()
        },
        can_write = function() {
            if (isTRUE(.self$.mode == "readonly")) {
                return(FALSE)
            }
            return(TRUE)
        },
        lock_partition = function(parts, timeout = NULL) {
            if (!.self$valid()) {
                stop("Invalid file array")
            }
            nparts <- nrow(.self$.partition_info)
            if (missing(parts)) {
                parts <- seq_len(nparts)
            }
            parts <- validate_lock_parts(parts, nparts)
            if (is.null(timeout)) {
                timeout <- lock_timeout_default()
            } else {
                timeout <- validate_lock_timeout(timeout)
            }
            locked <- lock_partition_explicit(.self$.filebase, parts, timeout)
            if (isTRUE(locked)) {
                return(invisible(locked))
            }
            locked
        },
        unlock_partition = function(parts) {
            if (missing(parts)) {
                released <- lock_release_explicit(.self$.filebase)
            } else {
                released <- lock_release_explicit(
                    .self$.filebase, validate_lock_parts(parts))
            }
            invisible(released > 0)
        },
        delete = function(force = FALSE) {
            if (!.self$valid()) {
                .self$.valid <- FALSE
                return(invisible())
            }
            if (isTRUE(.self$.mode == "readonly")) {
                if (force) {
                    quiet_warning("File array is read-only, but deleted with `force=TRUE`")
                } else {
                    quiet_warning("File array is read-only.")
                }
            }
            filebase <- .self$.filebase
            # Drop this session's partition locks and close the lock file:
            # on Windows an open file keeps the directory from being removed
            lock_forget(filebase)
            if (dir.exists(filebase)) {
                unlink(filebase, recursive = TRUE, force = force)
            }
            .self$.valid <- FALSE
            invisible()
        },
        valid = function() {
            return(.self$.valid && dir.exists(.self$.filebase))
        },
        collapse = function(
            keep, method = c("mean", "sum"),
            transform = c("asis", "10log10", "square", "sqrt", "normalize"), 
            na.rm = FALSE
        ) {
            method <- match.arg(method)
            transform <- match.arg(transform)
            keep <- as.integer(keep)
            dim <- .self$dimension()
            if (any(is.na(keep)) || !all(keep %in% seq_along(dim))) {
                stop("`keep` must be valid margin numbers")
            }
            filebase <- paste0(.self$.filebase, .self$.sep)
            
            dim1 <- dim
            transform1 <- which(c("asis", "10log10", "square", "sqrt", "normalize") == transform)
            if ( method == "sum" ) {
                scale <- 1
            } else {
                scale <- 1 / prod(dim[-keep])
            }
            # make sure last margin is the last one
            dim <- .self$dimension()
            ndims <- length(dim)
            perm <- FALSE
            keep1 <- keep
            nkeeps <- length(keep)
            if (nkeeps > 1 && 
               ndims %in% keep && 
               keep[[nkeeps]] != ndims)
            {
                keep_ldidx <- which(keep == ndims)
                
                keep_ord <- seq_along(keep)
                keep_ord <- c(
                    keep_ord[-seq.int(keep_ldidx, nkeeps)], 
                    nkeeps,
                    keep_ord[-seq.int(1, keep_ldidx)] - 1
                )
                
                keep1 <- c(keep[-keep_ldidx], keep[keep_ldidx])
                perm <- TRUE
            }
            
            
            expr <- quote(FARR_collapse(
                filebase = filebase,
                dim = dim1,
                keep = keep1,
                cum_part = .self$.partition_info[, 3],
                remove_na = na.rm,
                method = transform1,
                scale = scale
            ))
            if (.self$type() == "complex") {
                expr[[1]] <- quote(FARR_collapse_complex)
            } else {
                expr[["array_type"]] <- .self$sexp_type()
            }
                
            re <- eval(expr)
            
            dnames <- .self$dimnames()
            if (length(dnames) == length(dim)) {
                dnames <- dnames[keep]
                if (length(keep) == 1) {
                    re <- structure(re, names = dnames[[1]])
                } else {
                    re <- structure(re, dimnames = dnames)
                }
            }
            
            if (perm) {
                re <- aperm(re, keep_ord)
            }
            
            return(re)
            
        }
    )
)


#' @rdname filearray
#' @export
as_filearray <- function(x, ...) {
    UseMethod("as_filearray")
}

#' @export
as_filearray.default <- function(x, filebase = NULL, type = NULL, dimension = dim(x), ...) {
    if (!length(filebase)) {
        filebase <- temp_path(check = TRUE)
        while (file.exists(filebase)) {
            filebase <- temp_path(check = FALSE)
        }
    }
    x <- as.array(x)
    
    if (length(type) == 1) {
        expected_type <- type
    } else {
        expected_type <- typeof(x)
        if (expected_type %in% c("double", "float")) {
            expected_type <- getOption("filearray.operator.precision", "double")
        }
    }
    
    
    re <- filearray_create(filebase = filebase, dimension = dimension, type = expected_type, ...)
    re[] <- x
    
    dimnames(re) <- dimnames(x)
    return(re)
}

#' @export
as_filearray.character <- function(x, mode = c("readonly", "readwrite"), ...) {
    mode <- match.arg(mode)
    filearray_load(filebase = x, mode = mode)
}

#' @export
as_filearray.FileArray <- function(x, ...) {
    x
}

#' @export
as_filearray.FileArrayProxy <- function(x, ...) {
    x
}

#' @export
as_filearray.RAVEFileArray <- function(x, ...) {
    # x <- ravepipeline::RAVEFileArray$new(filearray::as_filearray(array(1:16, c(4,4))))
    if (inherits(x, c("FileArray", "FileArrayProxy"))) {
        return(x)
    }
    if (inherits(x$`@impl`, c("FileArray", "FileArrayProxy"))) {
        return(x$`@impl`)
    }
    stop("`x` is not a valid `RAVEFileArray` object.")
}
