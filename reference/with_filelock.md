# Evaluate an expression while holding a file lock

Runs `expr` while holding an exclusive lock on `lockfile`, so that only
one R session at a time runs code under that lock; other sessions asking
for it wait until it is released. The lock has nothing to do with file
arrays: any package or script can use it.

## Usage

``` r
with_filelock(
  expr,
  lockfile,
  timeout = Inf,
  on_unsupported = c("warn", "error", "ignore")
)
```

## Arguments

- expr:

  expression to evaluate while the lock is held; it is evaluated in the
  calling environment

- lockfile:

  path of the lock file, a file used only for locking. It is created if
  missing (its directory must exist), never written to, and left in
  place afterwards

- timeout:

  seconds to wait for another session to release the lock: `Inf`
  (default) waits until the lock is free, `0` tries once. After the
  time-out an error of class `filearray_lock_timeout` is raised and
  `expr` is not evaluated

- on_unsupported:

  what to do when the lock cannot be taken for a reason other than
  another session holding it, for example a file system without lock
  support or a lock file that cannot be created: `"warn"` (default)
  warns once per lock file and session, then evaluates `expr` without
  the lock; `"error"` raises an error and does not evaluate `expr`;
  `"ignore"` evaluates `expr` without the lock and without a warning.
  The warning and the error have class `filearray_lock_unsupported`

## Value

The value of `expr`

## Details

The lock is an operating-system lock on the first byte of `lockfile`
(`fcntl` on `POSIX` systems, `LockFileEx` on 'Windows'), so it is
released when a session ends, even when it crashes. It is the lock that
`lock()` of the filelock package takes in exclusive mode, so the two
exclude each other between sessions.

Locks belong to the R session: a nested `with_filelock()` call on the
same lock file in the same session runs at once instead of waiting for
itself. Code running in the same session is therefore never excluded,
even when it serves different users, as several 'Shiny' sessions in one
R process do.

Use a dedicated lock file, and do not open it in `expr`: on `POSIX`
systems closing any connection to the lock file releases the lock, and
on 'Windows' the locked byte cannot be read or written.

The lock is advisory: it only excludes code that locks the same file.
Forked workers (such as those of `mclapply` in parallel) do not inherit
it. Sessions that nest locks on two files in opposite orders wait for
each other until their time-outs. On network file systems the lock works
across computers only if the file system supports locks; folders
synchronized by cloud services are locked on one computer only.

## Examples

``` r

lockfile <- tempfile(fileext = ".lock")
log <- tempfile()

# one R session at a time appends to the log
with_filelock({
    cat("one line\n", file = log, append = TRUE)
}, lockfile = lockfile)

readLines(log)
#> [1] "one line"

unlink(c(lockfile, log))
```
