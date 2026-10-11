# with_filelock(): an exclusive lock shared by all R sessions

test_that("with_filelock() returns the value of expr", {
    lf <- tempfile(fileext = ".lock")
    on.exit(unlink(lf), add = TRUE)

    expect_equal(with_filelock(1 + 1, lf), 2)
    expect_invisible(with_filelock(invisible(3), lf))
    expect_visible(with_filelock(4, lf))
})

test_that("with_filelock() holds byte 0 of the lock file while expr runs", {
    lf <- tempfile(fileext = ".lock")
    on.exit(unlink(lf), add = TRUE)

    held <- with_filelock(FARR_lock_status(lock_os_path(lf)), lf)
    expect_true(held$open)
    expect_equal(held$partition, 0)
    expect_equal(held$automatic, 1L)
    expect_equal(held$explicit, 0L)

    registry <- FARR_lock_registry()
    expect_equal(registry$entries, 0)
    expect_equal(registry$scopes, 0)
    # the lock file stays, empty
    expect_true(file.exists(lf))
    expect_equal(file.size(lf), 0)
})

test_that("with_filelock() releases the lock when expr fails", {
    lf <- tempfile(fileext = ".lock")
    on.exit(unlink(lf), add = TRUE)

    expect_error(with_filelock(stop("boom"), lf), "boom")
    registry <- FARR_lock_registry()
    expect_equal(registry$entries, 0)
    expect_equal(registry$scopes, 0)
})

test_that("nested with_filelock() on one lock file runs at once", {
    lf <- tempfile(fileext = ".lock")
    on.exit(unlink(lf), add = TRUE)

    # an inner call that waited for the outer lock would time out
    expect_equal(with_filelock(with_filelock(1, lf, timeout = 0), lf), 1)
})

test_that("with_filelock() validates its arguments", {
    lf <- tempfile(fileext = ".lock")
    on.exit(unlink(lf), add = TRUE)

    expect_error(with_filelock(1, NA), "lockfile")
    expect_error(with_filelock(1, c("a.lock", "b.lock")), "lockfile")
    expect_error(with_filelock(1, 1), "lockfile")
    expect_error(with_filelock(1, ""), "lockfile")
    expect_error(with_filelock(1, lf, timeout = -1), "timeout")
    expect_error(with_filelock(1, lf, timeout = NA), "timeout")
    expect_error(with_filelock(1, lf, on_unsupported = "maybe"))
    # nothing is created before the arguments are checked
    expect_false(file.exists(lf))
})

test_that("on_unsupported decides what happens when the lock cannot be taken", {
    # a lock file cannot be created in a directory that does not exist
    lf <- file.path(tempfile(), "x.lock")
    ran <- 0

    expect_warning(
        expect_equal(with_filelock({
            ran <- ran + 1
            "done"
        }, lf), "done"),
        class = "filearray_lock_unsupported"
    )
    # once per lock file and session
    expect_warning(with_filelock({
        ran <- ran + 1
    }, lf), NA)
    expect_equal(ran, 2)

    expect_error(with_filelock({
        ran <- ran + 1
    }, lf, on_unsupported = "error"), class = "filearray_lock_unsupported")
    expect_equal(ran, 2)

    expect_warning(with_filelock({
        ran <- ran + 1
    }, lf, on_unsupported = "ignore"), NA)
    expect_equal(ran, 3)
})

test_that("another session waits for with_filelock()", {
    skip_on_cran()
    parallel <- asNamespace("parallel")
    cl <- lock_test_cluster(2, need = "with_filelock")
    lf <- tempfile(fileext = ".lock")
    marker <- tempfile()
    on.exit({
        try(parallel$stopCluster(cl), silent = TRUE)
        unlink(c(lf, marker))
    }, add = TRUE)

    task <- in_worker(function(role, lf, marker) {
        if (role == 1) {
            filearray::with_filelock({
                file.create(marker)
                Sys.sleep(1.5)
            }, lf)
            return(NA_real_)
        }
        deadline <- proc.time()[["elapsed"]] + 20
        while (!file.exists(marker) && proc.time()[["elapsed"]] < deadline) {
            Sys.sleep(0.01)
        }
        start <- proc.time()[["elapsed"]]
        filearray::with_filelock(TRUE, lf)
        proc.time()[["elapsed"]] - start
    })
    res <- parallel$clusterApply(cl, 1:2, task, lf = lf, marker = marker)

    expect_gt(res[[2]], 0.5)
})

test_that("with_filelock() gives up after its time-out without running expr", {
    skip_on_cran()
    parallel <- asNamespace("parallel")
    cl <- lock_test_cluster(2, need = "with_filelock")
    lf <- tempfile(fileext = ".lock")
    marker <- tempfile()
    on.exit({
        try(parallel$stopCluster(cl), silent = TRUE)
        unlink(c(lf, marker))
    }, add = TRUE)

    task <- in_worker(function(role, lf, marker) {
        if (role == 1) {
            filearray::with_filelock({
                file.create(marker)
                Sys.sleep(2)
            }, lf)
            return(NULL)
        }
        deadline <- proc.time()[["elapsed"]] + 20
        while (!file.exists(marker) && proc.time()[["elapsed"]] < deadline) {
            Sys.sleep(0.01)
        }
        ran <- FALSE
        cls <- tryCatch({
            filearray::with_filelock({
                ran <- TRUE
            }, lf, timeout = 0.3)
            "no error"
        }, filearray_lock_timeout = function(e) {
            "filearray_lock_timeout"
        })
        list(class = cls, ran = ran)
    })
    res <- parallel$clusterApply(cl, 1:2, task, lf = lf, marker = marker)

    expect_identical(res[[2]], list(class = "filearray_lock_timeout", ran = FALSE))
})

test_that("with_filelock() lets one session at a time run", {
    skip_on_cran()
    parallel <- asNamespace("parallel")
    cl <- lock_test_cluster(2, need = "with_filelock")
    lf <- tempfile(fileext = ".lock")
    log <- tempfile()
    on.exit({
        try(parallel$stopCluster(cl), silent = TRUE)
        unlink(c(lf, log))
    }, add = TRUE)

    task <- in_worker(function(k, lf, log) {
        for (i in 1:3) {
            filearray::with_filelock({
                cat(sprintf("start %d\n", k), file = log, append = TRUE)
                Sys.sleep(0.2)
                cat(sprintf("end %d\n", k), file = log, append = TRUE)
            }, lf)
        }
        TRUE
    })
    parallel$clusterApply(cl, 1:2, task, lf = lf, log = log)

    lines <- readLines(log)
    expect_length(lines, 12)
    # every start is directly followed by the end of the same session
    starts <- lines[c(TRUE, FALSE)]
    ends <- lines[c(FALSE, TRUE)]
    expect_equal(sub("start", "end", starts), ends)
})
