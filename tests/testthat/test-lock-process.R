# Partition locks between processes. PSOCK workers load the installed
# filearray: under devtools::test(), run R CMD INSTALL first or these skip

test_that("another process cannot lock a partition this session holds", {
    cl <- lock_test_cluster(1)
    x <- new_lock_array()
    on.exit({
        try(parallel::stopCluster(cl), silent = TRUE)
        x$unlock_partition()
        x$delete(force = TRUE)
    }, add = TRUE)

    probe <- in_worker(function(fb, part, timeout) {
        y <- filearray::filearray_load(fb)
        start <- proc.time()[["elapsed"]]
        locked <- isTRUE(y$lock_partition(part, timeout = timeout))
        waited <- proc.time()[["elapsed"]] - start
        if (locked) {
            y$unlock_partition(part)
        }
        list(locked = locked, waited = waited)
    })

    x$lock_partition(2)
    res <- parallel::clusterCall(cl, probe, x$.filebase, 2, 0.5)[[1]]
    expect_false(res$locked)
    expect_gte(res$waited, 0.4)
    res <- parallel::clusterCall(cl, probe, x$.filebase, 3, 0)[[1]]
    expect_true(res$locked)

    x$unlock_partition(2)
    res <- parallel::clusterCall(cl, probe, x$.filebase, 2, 0)[[1]]
    expect_true(res$locked)
})

test_that("a write waits while another process holds the partition", {
    cl <- lock_test_cluster(2)
    x <- new_lock_array()
    marker <- tempfile()
    on.exit({
        try(parallel::stopCluster(cl), silent = TRUE)
        unlink(marker)
        x$delete(force = TRUE)
    }, add = TRUE)

    task <- in_worker(function(role, fb, marker) {
        y <- filearray::filearray_load(fb)
        if (role == 1) {
            y$lock_partition(1)
            file.create(marker)
            Sys.sleep(1.5)
            y[, , 1] <- 1
            y$unlock_partition(1)
            return(NA_real_)
        }
        deadline <- proc.time()[["elapsed"]] + 20
        while (!file.exists(marker) && proc.time()[["elapsed"]] < deadline) {
            Sys.sleep(0.01)
        }
        start <- proc.time()[["elapsed"]]
        y[, , 1] <- 2
        proc.time()[["elapsed"]] - start
    })
    res <- parallel::clusterApply(cl, 1:2, task, fb = x$.filebase, marker = marker)

    expect_gt(res[[2]], 0.5)
    expect_equal(x[, , 1], matrix(2, 2, 3))
})

test_that("processes creating the same partition keep each other's data", {
    cl <- lock_test_cluster(2)
    # one partition holding both slices, created by whichever process is first
    x <- new_lock_array(c(2, 3, 2), partition_size = 2)
    on.exit({
        try(parallel::stopCluster(cl), silent = TRUE)
        x$delete(force = TRUE)
    }, add = TRUE)

    task <- in_worker(function(k, fb) {
        # a slow disk: each partition header takes a second to write
        suppressMessages(trace(
            "write_header", tracer = quote(Sys.sleep(1)),
            where = asNamespace("filearray"), print = FALSE
        ))
        if (k == 2) {
            Sys.sleep(0.3)
        }
        y <- filearray::filearray_load(fb)
        y[, , k] <- k
        TRUE
    })
    parallel::clusterApply(cl, 1:2, task, fb = x$.filebase)

    expect_equal(x[, , 1], matrix(1, 2, 3))
    expect_equal(x[, , 2], matrix(2, 2, 3))
})

test_that("a write that times out raises an error and holds nothing", {
    cl <- lock_test_cluster(1)
    x <- new_lock_array()
    old <- options(filearray.lock.timeout = 0.3)
    on.exit({
        options(old)
        try(parallel::stopCluster(cl), silent = TRUE)
        x$delete(force = TRUE)
    }, add = TRUE)

    hold <- in_worker(function(fb) {
        filearray::filearray_load(fb)$lock_partition(1, timeout = 0)
    })
    release <- in_worker(function(fb) {
        filearray::filearray_load(fb)$unlock_partition(1)
    })

    x[] <- 0
    expect_true(parallel::clusterCall(cl, hold, x$.filebase)[[1]])

    start <- proc.time()[["elapsed"]]
    expect_error(x[, , 1] <- 1, class = "filearray_lock_timeout")
    expect_gte(proc.time()[["elapsed"]] - start, 0.25)
    expect_equal(x[, , 1], matrix(0, 2, 3))
    expect_equal(FARR_lock_registry()$entries, 0)

    # other partitions are not affected
    x[, , 2] <- 2
    expect_equal(x[, , 2], matrix(2, 2, 3))

    # set_partition() locks before it reads the partition it rewrites
    suppressMessages(trace(
        "load_partition", tracer = quote(stop("read before locking")),
        where = asNamespace("filearray"), print = FALSE
    ))
    on.exit(suppressMessages(untrace(
        "load_partition", where = asNamespace("filearray")
    )), add = TRUE)
    expect_error(x$set_partition(1, 7, 1, 1, 1), class = "filearray_lock_timeout")

    # fmap() locks its output before it writes to it
    src <- new_lock_array()
    on.exit(src$delete(force = TRUE), add = TRUE)
    src[] <- 5
    expect_error(fmap(src, function(input) { input[[1]] }, .y = x),
                 class = "filearray_lock_timeout")
    expect_equal(x[, , 1], matrix(0, 2, 3))
    expect_equal(x[, , 2], matrix(2, 2, 3))

    expect_false(x$lock_partition(1, timeout = 0))
    parallel::clusterCall(cl, release, x$.filebase)
    expect_true(x$lock_partition(1, timeout = 0))
    x$unlock_partition()
})

test_that("the locks of a killed process are released", {
    cl <- lock_test_cluster(1)
    x <- new_lock_array()
    on.exit({
        try(parallel::stopCluster(cl), silent = TRUE)
        x$unlock_partition()
        x$delete(force = TRUE)
    }, add = TRUE)

    hold <- in_worker(function(fb) {
        filearray::filearray_load(fb)$lock_partition(1, timeout = 0)
    })
    pid <- parallel::clusterCall(cl, Sys.getpid)[[1]]
    # a killed R process cannot remove its own session temporary directory
    worker_tmp <- parallel::clusterCall(cl, tempdir)[[1]]
    on.exit(unlink(worker_tmp, recursive = TRUE), add = TRUE)

    expect_true(parallel::clusterCall(cl, hold, x$.filebase)[[1]])
    expect_false(x$lock_partition(1, timeout = 0))

    signal <- if (.Platform$OS.type == "windows") tools::SIGTERM else tools::SIGKILL
    tools::pskill(pid, signal)
    # Windows may release the locks of a terminated process with a delay
    expect_true(x$lock_partition(1, timeout = 10))
})

test_that("forked children do not inherit the parent's locks", {
    skip_on_cran()
    skip_on_os("windows")
    x <- new_lock_array()
    on.exit({
        x$unlock_partition()
        x$delete(force = TRUE)
    }, add = TRUE)

    x$lock_partition(1)
    job <- parallel::mcparallel(list(
        busy = x$lock_partition(1, timeout = 0),
        free = isTRUE(x$lock_partition(2, timeout = 0))
    ))
    expect_identical(parallel::mccollect(job)[[1]], list(busy = FALSE, free = TRUE))

    # the first child's exit did not release the parent's lock
    job <- parallel::mcparallel(x$lock_partition(1, timeout = 0))
    expect_false(parallel::mccollect(job)[[1]])
    expect_equal(lock_status(x)$explicit, 1L)
})
