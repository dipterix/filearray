# Partition locks
#
# Protocol v1. This is a contract between filearray versions; src/lock.h
# documents the same:
#
# * "<filebase>/.filearray.lock" is an empty file, created by the first
#   process that locks. Nothing writes to it; it is never renamed or removed
#   except together with the array, and no other code opens it.
# * Byte k >= 1 is the exclusive write lock of partition k (file
#   "<k-1>.farr"). Byte 0 is reserved for a future array-level lock.
# * POSIX: classic fcntl record lock F_WRLCK on [k, k + 1), taken with
#   non-blocking F_SETLK only. Windows: LockFileEx at offset k, length 1.
# * Only writers lock. The locks are advisory, and the operating system
#   releases them when a process exits.
#
# Locks belong to the R session: the C++ registry counts holds per byte, so
# nested writes and several objects for one array share a lock. A write holds
# its locks in a scope, which C++ records by id, so that `on.exit()` releases
# exactly what was taken even after an interrupt.

LOCK_FILE_NAME <- ".filearray.lock"
LOCK_PROTOCOL_VERSION <- 1L

# Status of FARR_lock_acquire(); keep in sync with src/lock.h
LOCK_STATUS_OK <- 0L
LOCK_STATUS_BUSY <- 1L
LOCK_STATUS_UNSUPPORTED <- 2L

# Status of FARR_link_noreplace(); keep in sync with src/lock.h
LINK_CREATED <- 0L
LINK_EXISTS <- 1L
LINK_UNSUPPORTED <- 2L

# Seconds before locking is tried again on a lock file that could not lock,
# so that one transient network error does not end locking for the session
LOCK_RETRY_AFTER <- 60

# Temporary files of killed writers older than this (seconds) are removed
STALE_TMP_AGE <- 3600

lock_state <- new.env(parent = emptyenv())
lock_state$scope_id <- 0
# lock path -> list(time, message) of the last "unsupported" failure
lock_state$unsupported <- new.env(parent = emptyenv())
# lock path -> TRUE once warned
lock_state$warned <- new.env(parent = emptyenv())

lock_file_path <- function(filebase) {
    file.path(filebase, LOCK_FILE_NAME)
}

# C++ takes UTF-8 on Windows (and converts it to UTF-16), native bytes elsewhere
lock_os_path <- function(path) {
    if (get_os() == "windows") {
        enc2utf8(path)
    } else {
        enc2native(path)
    }
}

# Whether writes lock automatically
lock_enabled <- function() {
    value <- getOption("filearray.lock", NULL)
    if (is.null(value)) {
        # callr workers inherit environment variables, not options
        value <- Sys.getenv("FILEARRAY_LOCK", unset = "TRUE")
    }
    value <- toupper(trimws(as.character(value)[1]))
    !isTRUE(value %in% c("FALSE", "F", "0", "NO", "OFF"))
}

validate_lock_timeout <- function(timeout, what = "timeout") {
    value <- suppressWarnings(as.numeric(timeout))
    if (length(value) != 1L || is.na(value) || value < 0) {
        stop(sprintf("`%s` must be a non-negative number of seconds or `Inf`", what),
             call. = FALSE)
    }
    value
}

# Seconds to wait for locks: `Inf` waits until they are free, `0` tries once
lock_timeout_default <- function() {
    value <- getOption("filearray.lock.timeout", NULL)
    what <- "filearray.lock.timeout"
    if (is.null(value)) {
        value <- Sys.getenv("FILEARRAY_LOCK_TIMEOUT", unset = "")
        if (!nzchar(value)) {
            return(Inf)
        }
        what <- "FILEARRAY_LOCK_TIMEOUT"
    }
    validate_lock_timeout(value, what)
}

# Sorted unique partition numbers, each from 1 to `nparts`
validate_lock_parts <- function(parts, nparts = NA) {
    if (!length(parts)) {
        return(numeric(0))
    }
    if (!is.numeric(parts) || anyNA(parts) || any(parts < 1) ||
        any(parts != round(parts)) || (!is.na(nparts) && any(parts > nparts))) {
        msg <- "`parts` must be partition numbers"
        if (!is.na(nparts)) {
            msg <- sprintf("%s from 1 to %d", msg, as.integer(nparts))
        }
        stop(msg, call. = FALSE)
    }
    sort(unique(as.numeric(parts)))
}

# Partitions holding the last-margin `slices`. C++ truncates fractional
# indices, so they are truncated here too
partitions_of_slices <- function(x, slices) {
    cum <- x$.partition_info[, 3]
    sort(unique(findInterval(trunc(slices), cum, left.open = TRUE) + 1))
}

lock_unsupported_since <- function(path) {
    failure <- lock_state$unsupported[[path]]
    if (is.null(failure) ||
        proc.time()[["elapsed"]] - failure$time >= LOCK_RETRY_AFTER) {
        return(NULL)
    }
    failure
}

lock_mark_unsupported <- function(path, filebase, message) {
    assign(path, list(time = proc.time()[["elapsed"]], message = message),
           envir = lock_state$unsupported)
    if (!isTRUE(lock_state$warned[[path]])) {
        assign(path, TRUE, envir = lock_state$warned)
        quiet_warning(sprintf(paste(
            "Cannot lock partitions of file array '%s' (%s).",
            "This session writes to it without partition locks: other",
            "processes writing to the same partitions are not excluded."
        ), filebase, message))
    }
    invisible()
}

# A scope groups the locks of one operation; C++ records them by its id
lock_scope_begin <- function() {
    lock_state$scope_id <- lock_state$scope_id + 1
    lock_state$scope_id
}

lock_scope_end <- function(scope) {
    suspendInterrupts(tryCatch(
        FARR_lock_release_scope(scope),
        error = function(e) { 0 }
    ))
    invisible()
}

# Locks `parts` (sorted) under `scope`, waiting while other processes hold
# them. Returns a list with `status` "acquired", "timeout" or "unsupported"
lock_scope_wait <- function(scope, filebase, parts, timeout) {
    path <- lock_file_path(filebase)
    failure <- lock_unsupported_since(path)
    if (!is.null(failure)) {
        return(list(status = "unsupported", pending = parts, message = failure$message))
    }
    os_path <- lock_os_path(path)
    deadline <- proc.time()[["elapsed"]] + timeout
    delay <- 0.001
    repeat {
        res <- FARR_lock_acquire(os_path, parts, scope)
        if (res$status == LOCK_STATUS_OK) {
            return(list(status = "acquired", pending = numeric(0)))
        }
        if (res$status == LOCK_STATUS_UNSUPPORTED) {
            lock_mark_unsupported(path, filebase, res$message)
            return(list(status = "unsupported", pending = parts, message = res$message))
        }
        if (res$status != LOCK_STATUS_BUSY) {
            stop(sprintf("Cannot lock partitions of file array '%s': %s",
                         filebase, res$message), call. = FALSE)
        }
        # the leading partitions are held while waiting for the next one
        if (res$acquired > 0) {
            parts <- parts[-seq_len(res$acquired)]
        }
        remaining <- deadline - proc.time()[["elapsed"]]
        if (remaining <= 0) {
            return(list(status = "timeout", pending = parts))
        }
        Sys.sleep(min(delay, remaining))
        delay <- min(delay * 2, 0.1)
    }
}

lock_timeout_error <- function(filebase, pending, timeout) {
    msg <- sprintf(paste(
        "Timed out after %s second(s) waiting to lock partition %s of file",
        "array '%s': another process is writing to it, or holds it with",
        "`lock_partition()`. Set `options(filearray.lock.timeout)` to wait longer."
    ), format(timeout), format(pending[[1]]), filebase)
    structure(
        class = c("filearray_lock_timeout", "error", "condition"),
        list(message = msg, call = NULL, filebase = filebase,
             partitions = pending, timeout = timeout)
    )
}

# Locks partitions for a write until `lock_scope_end(scope)`. Returns TRUE if
# they are locked; FALSE if automatic locking is off or the file system cannot
# lock. Signals a `filearray_lock_timeout` error on time-out
lock_scope_acquire <- function(scope, x, parts, timeout = NULL) {
    if (!length(parts) || !lock_enabled()) {
        return(invisible(FALSE))
    }
    if (is.null(timeout)) {
        timeout <- lock_timeout_default()
    }
    parts <- sort(unique(as.numeric(parts)))
    res <- lock_scope_wait(scope, x$.filebase, parts, timeout)
    if (identical(res$status, "timeout")) {
        stop(lock_timeout_error(x$.filebase, res$pending, timeout))
    }
    invisible(identical(res$status, "acquired"))
}

# Locks for `lock_partition()`: TRUE when locked; TRUE with attribute
# `locked = FALSE` (and a warning) if the file system cannot lock; FALSE on
# time-out, with nothing locked
lock_partition_explicit <- function(filebase, parts, timeout) {
    if (!length(parts)) {
        return(TRUE)
    }
    scope <- lock_scope_begin()
    on.exit(lock_scope_end(scope), add = TRUE)
    res <- lock_scope_wait(scope, filebase, parts, timeout)
    if (identical(res$status, "timeout")) {
        return(FALSE)
    }
    if (identical(res$status, "unsupported")) {
        warning(sprintf("Partitions of file array '%s' are not locked: %s",
                        filebase, res$message), call. = FALSE)
        return(structure(TRUE, locked = FALSE))
    }
    suspendInterrupts(FARR_lock_commit_scope(scope))
    TRUE
}

# Releases `lock_partition()` locks: one per partition, or all of them
lock_release_explicit <- function(filebase, parts = NULL) {
    os_path <- lock_os_path(lock_file_path(filebase))
    if (is.null(parts)) {
        FARR_lock_release_explicit(os_path, numeric(0), TRUE)
    } else {
        FARR_lock_release_explicit(os_path, parts, FALSE)
    }
}

# Drops every lock this session holds on an array and closes its lock file
lock_forget <- function(filebase) {
    tryCatch(
        FARR_lock_forget(lock_os_path(lock_file_path(filebase))),
        error = function(e) { 0 }
    )
}

# Locks this session holds on the array of `x`
lock_status <- function(x) {
    FARR_lock_status(lock_os_path(lock_file_path(x$.filebase)))
}
