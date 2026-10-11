# File locks: partition locks and with_filelock()
#
# Protocol v1. This is a contract between filearray versions; src/lock.h
# documents the same:
#
# * A lock file is an empty file, created by the first process that locks.
#   Nothing writes to it, it is never renamed or removed, and no other code
#   opens it. Each array has one: "<filebase>/.filearray.lock".
# * Byte 0 is the lock that with_filelock() takes. On an array's lock file it
#   is the array-level lock, which partition writers do not take.
# * Byte k >= 1 of an array's lock file is the exclusive write lock of
#   partition k (file "<k-1>.farr").
# * POSIX: classic fcntl record lock F_WRLCK on [k, k + 1), taken with
#   non-blocking F_SETLK only. Windows: LockFileEx at offset k, length 1.
# * Locks are exclusive; of an array, only writers lock. The locks are
#   advisory, and the operating system releases them when a process exits.
#
# Locks belong to the R session: the C++ registry counts holds per byte, so
# nested writes and several objects for one array share a lock. A write holds
# its locks in a scope, which C++ records by id, so that `on.exit()` releases
# exactly what was taken even after an interrupt.
#
# Locks guard; they never make the work they guard fail. When a lock cannot
# be taken for a reason other than another process holding it, writes warn
# once and proceed without it.

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

# Seconds to wait for locks: `Inf` waits until they are free, `0` tries once.
# A bad option or environment variable warns once and waits without limit:
# it must not stop every write
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
    timeout <- suppressWarnings(as.numeric(value))
    if (length(timeout) != 1L || is.na(timeout) || timeout < 0) {
        shown <- deparse1(value)
        lock_warn_once(sprintf("%s=%s", what, shown), quiet_warning(sprintf(paste(
            "`%s` must be a non-negative number of seconds or `Inf`;",
            "ignoring %s: locks are waited for without a time limit"
        ), what, shown)))
        return(Inf)
    }
    timeout
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

# Records that `path` cannot be locked; callers decide how to report it
lock_mark_unsupported <- function(path, message) {
    assign(path, list(time = proc.time()[["elapsed"]], message = message),
           envir = lock_state$unsupported)
    invisible()
}

# Evaluates `warning_expr` (a warning) only the first time per session and key
lock_warn_once <- function(key, warning_expr) {
    if (!isTRUE(lock_state$warned[[key]])) {
        assign(key, TRUE, envir = lock_state$warned)
        force(warning_expr)
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

# Locks `bytes` (sorted) of `lockfile` under `scope`, waiting while other
# processes hold them. Returns a list with `status`:
# * "acquired";
# * "timeout", with the bytes still `pending`;
# * "unsupported", with a `message`: the lock cannot be taken for a reason
#   other than another process holding it (unsupported file system, no
#   permission, stale network handle, ...)
lock_path_wait <- function(scope, lockfile, bytes, timeout) {
    failure <- lock_unsupported_since(lockfile)
    if (!is.null(failure)) {
        return(list(status = "unsupported", pending = bytes, message = failure$message))
    }
    os_path <- lock_os_path(lockfile)
    deadline <- proc.time()[["elapsed"]] + timeout
    delay <- 0.001
    repeat {
        res <- FARR_lock_acquire(os_path, bytes, scope)
        if (res$status == LOCK_STATUS_OK) {
            return(list(status = "acquired", pending = numeric(0)))
        }
        if (res$status != LOCK_STATUS_BUSY) {
            lock_mark_unsupported(lockfile, res$message)
            return(list(status = "unsupported", pending = bytes, message = res$message))
        }
        # the leading bytes are held while waiting for the next one
        if (res$acquired > 0) {
            bytes <- bytes[-seq_len(res$acquired)]
        }
        remaining <- deadline - proc.time()[["elapsed"]]
        if (remaining <= 0) {
            return(list(status = "timeout", pending = bytes))
        }
        Sys.sleep(min(delay, remaining))
        delay <- min(delay * 2, 0.1)
    }
}

# lock_path_wait() for partitions of the array at `filebase`
lock_scope_wait <- function(scope, filebase, parts, timeout) {
    lock_path_wait(scope, lock_file_path(filebase), parts, timeout)
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
    if (identical(res$status, "unsupported")) {
        lock_warn_once(lock_file_path(x$.filebase), quiet_warning(sprintf(paste(
            "Cannot lock partitions of file array '%s' (%s).",
            "This session writes to it without partition locks: other",
            "processes writing to the same partitions are not excluded."
        ), x$.filebase, res$message)))
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

validate_lockfile <- function(lockfile) {
    if (!is.character(lockfile) || length(lockfile) != 1L ||
        is.na(lockfile) || !nzchar(lockfile)) {
        stop("`lockfile` must be the path of a lock file: one non-empty string",
             call. = FALSE)
    }
    # absolute, so that the path names the same file if the working
    # directory changes while the lock is held
    lockfile <- path.expand(lockfile)
    file.path(normalizePath(dirname(lockfile), mustWork = FALSE), basename(lockfile))
}

filelock_timeout_error <- function(lockfile, timeout) {
    msg <- sprintf(paste(
        "Timed out after %s second(s) waiting to lock '%s': another R",
        "session holds it. Increase `timeout` to wait longer."
    ), format(timeout), lockfile)
    structure(
        class = c("filearray_lock_timeout", "error", "condition"),
        list(message = msg, call = NULL, lockfile = lockfile, timeout = timeout)
    )
}

filelock_unsupported <- function(lockfile, reason, type = c("warning", "error")) {
    type <- match.arg(type)
    msg <- sprintf("Cannot lock '%s' (%s)", lockfile, reason)
    if (type == "warning") {
        msg <- paste0(msg, ": running without the lock")
    }
    structure(
        class = c("filearray_lock_unsupported", type, "condition"),
        list(message = msg, call = NULL, lockfile = lockfile, reason = reason)
    )
}

#' @title Evaluate an expression while holding a file lock
#' @description Runs \code{expr} while holding an exclusive lock on
#' \code{lockfile}, so that only one \R session at a time runs code under
#' that lock; other sessions asking for it wait until it is released. The
#' lock has nothing to do with file arrays: any package or script can use it.
#' @param expr expression to evaluate while the lock is held; it is evaluated
#' in the calling environment
#' @param lockfile path of the lock file, a file used only for locking. It is
#' created if missing (its directory must exist), never written to, and left
#' in place afterwards
#' @param timeout seconds to wait for another session to release the lock:
#' \code{Inf} (default) waits until the lock is free, \code{0} tries once.
#' After the time-out an error of class \code{filearray_lock_timeout} is
#' raised and \code{expr} is not evaluated
#' @param on_unsupported what to do when the lock cannot be taken for a
#' reason other than another session holding it, for example a file system
#' without lock support or a lock file that cannot be created:
#' \code{"warn"} (default) warns once per lock file and session, then
#' evaluates \code{expr} without the lock; \code{"error"} raises an error and
#' does not evaluate \code{expr}; \code{"ignore"} evaluates \code{expr}
#' without the lock and without a warning. The warning and the error have
#' class \code{filearray_lock_unsupported}
#' @return The value of \code{expr}
#' @details The lock is an operating-system lock on the first byte of
#' \code{lockfile} (\code{fcntl} on \verb{POSIX} systems, \code{LockFileEx}
#' on 'Windows'), so it is released when a session ends, even when it
#' crashes. It is the lock that \code{lock()} of the \pkg{filelock} package
#' takes in exclusive mode, so the two exclude each other between sessions.
#'
#' Locks belong to the \R session: a nested \code{with_filelock()} call on the
#' same lock file in the same session runs at once instead of waiting for
#' itself. Code running in the same session is therefore never excluded, even
#' when it serves different users, as several 'Shiny' sessions in one \R
#' process do.
#'
#' Use a dedicated lock file, and do not open it in \code{expr}: on
#' \verb{POSIX} systems closing any connection to the lock file releases the
#' lock, and on 'Windows' the locked byte cannot be read or written.
#'
#' The lock is advisory: it only excludes code that locks the same file.
#' Forked workers (such as those of \code{mclapply} in \pkg{parallel}) do not
#' inherit it. Sessions that nest locks on two files in opposite orders wait for each
#' other until their time-outs. On network file systems the lock works across
#' computers only if the file system supports locks; folders synchronized by
#' cloud services are locked on one computer only.
#' @examples
#'
#' lockfile <- tempfile(fileext = ".lock")
#' log <- tempfile()
#'
#' # one R session at a time appends to the log
#' with_filelock({
#'     cat("one line\n", file = log, append = TRUE)
#' }, lockfile = lockfile)
#'
#' readLines(log)
#'
#' unlink(c(lockfile, log))
#'
#' @export
with_filelock <- function(expr, lockfile, timeout = Inf,
                          on_unsupported = c("warn", "error", "ignore")) {
    on_unsupported <- match.arg(on_unsupported)
    lockfile <- validate_lockfile(lockfile)
    timeout <- validate_lock_timeout(timeout)

    scope <- lock_scope_begin()
    on.exit(lock_scope_end(scope), add = TRUE)
    res <- lock_path_wait(scope, lockfile, 0, timeout)
    if (identical(res$status, "timeout")) {
        stop(filelock_timeout_error(lockfile, timeout))
    }
    if (identical(res$status, "unsupported")) {
        switch(
            on_unsupported,
            "error" = stop(filelock_unsupported(lockfile, res$message, "error")),
            "warn" = lock_warn_once(
                paste0("with_filelock:", lockfile),
                warning(filelock_unsupported(lockfile, res$message, "warning"))
            )
        )
    }

    res <- withVisible(expr)
    if (res$visible) {
        res$value
    } else {
        invisible(res$value)
    }
}
