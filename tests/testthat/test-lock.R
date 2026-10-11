# Partition locks within one R session. Tests that need a second process
# are in test-lock-process.R

test_that("partitions_of_slices() maps last-margin slices to partitions", {
    x <- new_lock_array(c(2, 3, 9), partition_size = 2)
    on.exit(x$delete(force = TRUE), add = TRUE)

    # partitions hold slices 1-2, 3-4, 5-6, 7-8 and 9
    expect_equal(partitions_of_slices(x, c(9, 1, 2, 3)), c(1, 2, 5))
    # C++ truncates fractional indices, so 2.5 is slice 2 and 4.9 is slice 4
    expect_equal(partitions_of_slices(x, 2.5), 1)
    expect_equal(partitions_of_slices(x, c(4.9, 5)), c(2, 3))
})

test_that("lock_partition() holds are counted per session", {
    x <- new_lock_array()
    on.exit(x$delete(force = TRUE), add = TRUE)

    expect_true(x$lock_partition(c(3, 1)))
    st <- lock_status(x)
    expect_true(st$open)
    expect_equal(st$partition, c(1, 3))
    expect_equal(st$explicit, c(1L, 1L))
    expect_equal(st$automatic, c(0L, 0L))

    x$lock_partition(1)
    expect_equal(lock_status(x)$explicit, c(2L, 1L))

    # writing a partition this session holds re-enters its lock
    x[, , 1] <- 1:6
    expect_equal(x[, , 1], matrix(as.double(1:6), 2, 3))
    st <- lock_status(x)
    expect_equal(st$explicit, c(2L, 1L))
    expect_equal(st$automatic, c(0L, 0L))

    expect_true(x$unlock_partition(1))
    expect_equal(lock_status(x)$explicit, c(1L, 1L))

    expect_true(x$unlock_partition())
    expect_false(lock_status(x)$open)
    expect_false(x$unlock_partition(2))
})

test_that("unlock_partition() cannot release a lock a write is using", {
    x <- new_lock_array()
    on.exit(x$delete(force = TRUE), add = TRUE)

    scope <- lock_scope_begin()
    lock_scope_acquire(scope, x, 2)
    expect_equal(lock_status(x)$automatic, 1L)

    expect_false(x$unlock_partition(2))
    expect_equal(lock_status(x)$automatic, 1L)

    lock_scope_end(scope)
    expect_false(lock_status(x)$open)
})

test_that("writes leave no partition locks behind", {
    x <- new_lock_array(c(2, 3, 4), partition_size = 2)
    others <- list()
    on.exit({
        x$delete(force = TRUE)
        for (o in others) {
            o$delete(force = TRUE)
        }
    }, add = TRUE)

    x[] <- 1:24
    # slice 4 <- 101:106, slice 1 <- 107:112, slice 3 <- 113:118
    x[, , c(4, 1, 3)] <- 101:118
    # partition 2 holds slices 3 and 4
    x$fill_partition(2, 0)
    # element [1, 1, 1] of partition 1 is x[1, 1, 1]
    x$set_partition(1, 7, 1, 1, 1)
    expect_equal(x[1, 1, ], c(7, 7, 0, 0))
    expect_equal(x[2, 3, 1], 112)

    others$fmap <- fmap(x, function(input) {
        input[[1]] + 1
    }, .buffer_count = 2)
    others$element_wise <- fmap_element_wise(x, function(input) {
        input[[1]] * 2
    })
    others$as_filearray <- as_filearray(array(1:24, c(2, 3, 4)))
    others$bind <- filearray_bind(x, x)
    expect_equal(others$fmap[1, 1, ], c(8, 8, 1, 1))
    expect_equal(others$element_wise[2, 3, 1], 224)
    expect_equal(others$bind[1, 1, ], c(7, 7, 0, 0, 7, 7, 0, 0))

    registry <- FARR_lock_registry()
    expect_equal(registry$entries, 0)
    expect_equal(registry$scopes, 0)
    expect_true(file.exists(lock_file(x)))
    expect_equal(file.size(lock_file(x)), 0)
    expect_false(LOCK_FILE_NAME %in% list.files(x$.filebase))
})

test_that("fmap callbacks can write to the output array", {
    x <- new_lock_array(c(2, 3, 2))
    out <- new_lock_array(c(2, 3, 2))
    on.exit({
        x$delete(force = TRUE)
        out$delete(force = TRUE)
    }, add = TRUE)
    x[] <- 1:12
    # fmap holds every partition of `out`; a write that had to wait for
    # those locks would time out immediately
    old <- options(filearray.lock.timeout = 0)
    on.exit(options(old), add = TRUE)

    calls <- 0
    expect_warning(fmap(x, function(input) {
        calls <<- calls + 1
        if (calls == 2) {
            # partition 1 was written after the first call
            out[1, 1, 1] <- -99
        }
        input[[1]] * 2
    }, .y = out, .buffer_count = 2), NA)
    expect_equal(calls, 2)
    expect_equal(out[], array(c(-99, seq(4, 24, by = 2)), c(2, 3, 2)))
})

test_that("fmap() stops when the mapping function fails", {
    x <- new_lock_array(c(2, 3, 2))
    out <- new_lock_array(c(2, 3, 2))
    on.exit({
        x$delete(force = TRUE)
        out$delete(force = TRUE)
    }, add = TRUE)
    x[] <- 1:12

    expect_error(fmap(x, function(input) {
        stop("boom")
    }, .y = out), "boom")
    expect_equal(FARR_lock_registry()$scopes, 0)
})

test_that("a failed write releases its partition locks", {
    x <- new_lock_array()
    on.exit({
        untrace_header_writes()
        x$delete(force = TRUE)
    }, add = TRUE)

    trace_header_writes(quote(stop("disk full")))
    expect_error(x[, , 1] <- 1, "disk full")
    expect_error(x$fill_partition(2, 1), "disk full")
    untrace_header_writes()

    registry <- FARR_lock_registry()
    expect_equal(registry$entries, 0)
    expect_equal(registry$scopes, 0)
    expect_length(tmp_files(x), 0)
})

test_that("fill_partition() writes through a new temporary file each time", {
    x <- new_lock_array()
    on.exit({
        untrace_header_writes()
        x$delete(force = TRUE)
    }, add = TRUE)

    # the temporary file exists while its header is written
    seen <- new.env()
    seen$names <- character(0)
    fb <- x$.filebase
    trace_header_writes(bquote({
        assign("names", c(get("names", envir = .(seen)),
                          list.files(.(fb), pattern = "[.]tmp$")),
               envir = .(seen))
    }))
    x$fill_partition(1, 1)
    x$fill_partition(1, 2)
    untrace_header_writes()

    expect_length(seen$names, 2)
    expect_match(seen$names, "^0\\.[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\\.tmp$")
    expect_false(identical(seen$names[[1]], seen$names[[2]]))
    expect_length(tmp_files(x), 0)
    expect_equal(x[, , 1], matrix(2, 2, 3))
})

test_that("a locked fill removes stale temporary files of killed writers", {
    x <- new_lock_array()
    on.exit(x$delete(force = TRUE), add = TRUE)

    stale <- file.path(x$.filebase, "0.0f0e5f0c-6f4a-4f2e-9d55-2c7b6c1d2e3f.tmp")
    recent <- file.path(x$.filebase, "0.1a2b3c4d-5e6f-4a1b-8c9d-0e1f2a3b4c5d.tmp")
    stale_other <- file.path(x$.filebase, "1.0f0e5f0c-6f4a-4f2e-9d55-2c7b6c1d2e3f.tmp")
    file.create(c(stale, recent, stale_other))
    Sys.setFileTime(c(stale, stale_other), Sys.time() - 7200)

    x$fill_partition(1, 5)

    expect_false(file.exists(stale))
    expect_true(file.exists(recent))
    # partition 1's fill only cleans up partition 1
    expect_true(file.exists(stale_other))
})

test_that("initialize_partition() never replaces a partition that exists", {
    # Another process creates partition 1 while this one writes its NA-filled
    # copy: the partition the other process created must survive
    src <- new_lock_array()
    src[] <- 42
    x <- new_lock_array()
    on.exit({
        untrace_header_writes()
        src$delete(force = TRUE)
        x$delete(force = TRUE)
    }, add = TRUE)

    created <- new.env()
    created$done <- FALSE
    trace_header_writes(bquote({
        if (!get("done", envir = .(created))) {
            assign("done", TRUE, envir = .(created))
            file.copy(.(src$partition_path(1)), .(x$partition_path(1)))
        }
    }))
    x$initialize_partition(1)
    untrace_header_writes()

    expect_true(created$done)
    expect_equal(x[, , 1], matrix(42, 2, 3))
    expect_length(tmp_files(x), 0)
})

test_that("without hard links, missing partitions are still created safely", {
    skip_if_not_installed("testthat", "3.1.7")
    local_mocked_bindings(FARR_link_noreplace = function(from, to) {
        2L
    })
    src <- new_lock_array()
    src[] <- 42
    x <- new_lock_array()
    on.exit({
        untrace_header_writes()
        src$delete(force = TRUE)
        x$delete(force = TRUE)
    }, add = TRUE)

    x$initialize_partition(1)
    expect_true(all(is.na(x[, , 1])))

    # a partition that appears meanwhile is kept
    created <- new.env()
    created$done <- FALSE
    trace_header_writes(bquote({
        if (!get("done", envir = .(created))) {
            assign("done", TRUE, envir = .(created))
            file.copy(.(src$partition_path(2)), .(x$partition_path(2)))
        }
    }))
    x$initialize_partition(2)
    untrace_header_writes()

    expect_true(created$done)
    expect_equal(x[, , 2], matrix(42, 2, 3))
    expect_length(tmp_files(x), 0)
})

test_that("automatic locking can be turned off", {
    x <- new_lock_array()
    old <- options(filearray.lock = FALSE)
    on.exit({
        options(old)
        x$delete(force = TRUE)
    }, add = TRUE)

    x[] <- 1
    expect_false(file.exists(lock_file(x)))

    options(filearray.lock = NULL)
    restore_env <- set_env_var("FILEARRAY_LOCK", "false")
    on.exit(restore_env(), add = TRUE)
    x[] <- 2
    expect_false(file.exists(lock_file(x)))

    # explicit locks do not depend on the switch
    expect_true(x$lock_partition(1))
    expect_true(file.exists(lock_file(x)))
    expect_equal(lock_status(x)$explicit, 1L)
    x$unlock_partition()
})

test_that("lock arguments are validated", {
    x <- new_lock_array()
    on.exit(x$delete(force = TRUE), add = TRUE)

    expect_error(x$lock_partition(0), "partition")
    expect_error(x$lock_partition(NA), "partition")
    expect_error(x$lock_partition(5), "partition")
    expect_error(x$lock_partition("a"), "partition")
    expect_error(x$lock_partition(1, timeout = -1), "timeout")
    expect_error(x$lock_partition(1, timeout = NA), "timeout")
    expect_error(x$lock_partition(1, timeout = c(1, 2)), "timeout")
    expect_false(lock_status(x)$open)
})

test_that("a bad time-out option warns instead of stopping writes", {
    x <- new_lock_array()
    old <- options(filearray.lock.timeout = "soon", filearray.quiet = FALSE)
    on.exit({
        options(old)
        x$delete(force = TRUE)
    }, add = TRUE)

    expect_warning(x[] <- 1, "filearray.lock.timeout")
    expect_equal(x[1, 1, 1], 1)

    options(filearray.lock.timeout = NULL)
    restore_env <- set_env_var("FILEARRAY_LOCK_TIMEOUT", "-5")
    on.exit(restore_env(), add = TRUE)
    expect_warning(x[] <- 2, "FILEARRAY_LOCK_TIMEOUT")
    expect_equal(x[1, 1, 1], 2)
})

test_that("a lock file that cannot be opened does not stop writes", {
    x <- new_lock_array()
    old <- options(filearray.quiet = FALSE)
    on.exit({
        options(old)
        x$delete(force = TRUE)
    }, add = TRUE)

    # a directory where the lock file belongs cannot be opened as a file
    dir.create(lock_file(x))

    expect_warning(x[, , 1] <- 1:6, "Cannot lock partitions")
    expect_equal(x[, , 1], matrix(as.double(1:6), 2, 3))

    expect_warning(res <- x$lock_partition(2), "not locked")
    expect_true(res)
    expect_identical(attr(res, "locked"), FALSE)
})

test_that("lock_partition() works on read-only arrays", {
    x <- new_lock_array()
    on.exit(x$delete(force = TRUE), add = TRUE)

    y <- filearray_load(x$.filebase, mode = "readonly")
    expect_true(y$lock_partition(1))
    expect_equal(lock_status(x)$explicit, 1L)
    expect_true(y$unlock_partition(1))
    expect_false(lock_status(x)$open)
})

test_that("delete() releases the session's locks on the array", {
    x <- new_lock_array()
    fb <- x$.filebase
    on.exit(unlink(fb, recursive = TRUE), add = TRUE)

    x$lock_partition(1:2)
    x$delete()

    expect_false(dir.exists(fb))
    expect_equal(FARR_lock_registry()$entries, 0)
})

test_that("objects and path aliases of one array share its locks", {
    skip_on_os("windows")
    x <- new_lock_array()
    alias <- tempfile()
    on.exit({
        unlink(alias)
        x$delete(force = TRUE)
    }, add = TRUE)
    file.symlink(x$.filebase, alias)

    y <- filearray_load(x$.filebase)
    z <- filearray_load(alias)
    # load() resolves the link; keep the alias so the lock path differs
    z$.filebase <- alias

    expect_true(z$lock_partition(2))
    expect_equal(FARR_lock_registry()$entries, 1)
    expect_equal(lock_status(x)$explicit, 1L)
    expect_true(y$unlock_partition(2))
    expect_false(lock_status(x)$open)
})

test_that("an array directory without write access warns and still writes", {
    skip_on_os("windows")
    x <- new_lock_array()
    old <- options(filearray.lock = FALSE, filearray.quiet = FALSE)
    on.exit({
        options(old)
        Sys.chmod(x$.filebase, "755")
        x$delete(force = TRUE)
    }, add = TRUE)

    # create every partition, but not the lock file
    x[] <- 1
    options(filearray.lock = TRUE)
    Sys.chmod(x$.filebase, "555")
    skip_if(file.access(x$.filebase, 2) == 0, "the directory stays writable (root)")

    expect_warning(x[, , 1] <- 2, "Cannot lock partitions")
    expect_warning(x[, , 1] <- 3, NA)
    expect_equal(x[, , 1], matrix(3, 2, 3))

    # explicit locks report every time that nothing is locked
    expect_warning(res <- x$lock_partition(1), "not locked")
    expect_true(res)
    expect_identical(attr(res, "locked"), FALSE)
    expect_warning(x$lock_partition(1), "not locked")
    expect_false(file.exists(lock_file(x)))
})
