# `write_header()` is the first write of `fill_partition()`: when it runs, the
# new partition is open but still empty. Tracing it lets a test load the array
# at that moment, in the same process (CRAN allows at most two)
trace_write_header <- function(tracer) {
    invisible(suppressMessages(trace(
        "write_header",
        tracer = tracer,
        where = asNamespace("filearray"),
        print = FALSE
    )))
}

untrace_write_header <- function() {
    invisible(suppressMessages(untrace(
        "write_header",
        where = asNamespace("filearray")
    )))
}

test_that("load() never lists a partition that is still being written", {
    fb <- tempfile()
    x <- filearray_create(fb, c(2, 3, 2), partition_size = 1)
    on.exit({
        untrace_write_header()
        unlink(fb, recursive = TRUE)
    }, add = TRUE)

    # Another process loading the array while partition 1 is being written
    loads <- list()
    record <- function(result) {
        loads[[length(loads) + 1L]] <<- result
    }
    trace_write_header(bquote(.(record)(tryCatch({
        filearray_load(.(fb), mode = "readonly")
        TRUE
    }, error = conditionMessage))))

    # creates partition 1
    x[, , 1] <- 1:6
    # refills partition 1
    x$fill_partition(1, 5)

    expect_equal(loads, list(TRUE, TRUE))
    expect_setequal(list.files(fb, all.files = TRUE, no.. = TRUE),
                    c("meta", "0.farr"))
})

test_that("fill_partition() replaces the values of an existing partition", {
    fb <- tempfile()
    on.exit({
        unlink(fb, recursive = TRUE)
    }, add = TRUE)
    x <- filearray_create(fb, c(2, 3, 2), partition_size = 1)

    x$fill_partition(1, 1)
    x$fill_partition(1, 5)

    expect_equal(x[, , 1], matrix(5, 2, 3))
})

test_that("fill_partition() stops when the partition file cannot be replaced", {
    fb <- tempfile()
    on.exit({
        unlink(fb, recursive = TRUE)
    }, add = TRUE)
    x <- filearray_create(fb, c(2, 3, 2), partition_size = 1)

    # No file system lets a file replace a directory
    dir.create(file.path(fb, "0.farr"))

    expect_error(suppressWarnings(x$fill_partition(1, 5)), "0.farr", fixed = TRUE)
    # and no temporary file is left behind
    expect_setequal(list.files(fb, all.files = TRUE, no.. = TRUE),
                    c("meta", "0.farr"))
})

test_that("load() only treats <n>.farr files as partitions", {
    fb <- tempfile()
    on.exit({
        unlink(fb, recursive = TRUE)
    }, add = TRUE)
    x <- filearray_create(fb, c(2, 3, 2), partition_size = 1)
    x[, , 1] <- 1:6

    # Empty files whose names contain a partition name
    file.create(file.path(fb, c("1.farr.tmp", "0.farr.bak")))

    expect_error(y <- filearray_load(fb, mode = "readonly"), NA)
    expect_equal(y[, , 1], matrix(as.double(1:6), 2, 3))
})
