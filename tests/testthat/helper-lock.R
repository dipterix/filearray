# Helpers for the partition-lock tests (test-lock.R, test-lock-process.R)

new_lock_array <- function(dim = c(2, 3, 4), partition_size = 1) {
    filearray_create(tempfile(), dim, partition_size = partition_size)
}

lock_file <- function(x) {
    file.path(x$.filebase, LOCK_FILE_NAME)
}

tmp_files <- function(x) {
    list.files(x$.filebase, pattern = "[.]tmp$", all.files = TRUE)
}

trace_header_writes <- function(tracer) {
    invisible(suppressMessages(trace(
        "write_header",
        tracer = tracer,
        where = asNamespace("filearray"),
        print = FALSE
    )))
}

# Untracing a function that is not traced fails in an installed package
untrace_header_writes <- function() {
    ns <- asNamespace("filearray")
    if (inherits(get("write_header", envir = ns), "functionWithTrace")) {
        suppressMessages(untrace("write_header", where = ns))
    }
    invisible()
}

# Sets an environment variable; returns a function that restores it
set_env_var <- function(name, value) {
    old <- Sys.getenv(name, unset = NA)
    do.call(Sys.setenv, structure(list(value), names = name))
    function() {
        if (is.na(old)) {
            Sys.unsetenv(name)
        } else {
            do.call(Sys.setenv, structure(list(old), names = name))
        }
    }
}

# Functions sent to workers must not carry the test environment along
in_worker <- function(f) {
    environment(f) <- globalenv()
    f
}

# Starts `n` PSOCK workers that load a filearray build with partition locks
# (and the function named by `need`, if any), or skips the test
lock_test_cluster <- function(n = 1, need = NULL) {
    testthat::skip_on_cran()
    # `R CMD check` sets R_TESTS to a relative path, which the base Rprofile
    # sources at start-up; workers started from this directory would fail
    restore <- set_env_var("R_TESTS", "")
    on.exit(restore(), add = TRUE)
    cl <- tryCatch(
        parallel::makePSOCKcluster(n, timeout = 120),
        error = function(e) { NULL }
    )
    if (is.null(cl)) {
        testthat::skip("Cannot start PSOCK workers")
    }
    setup <- in_worker(function(libs, version, need) {
        .libPaths(libs)
        ns <- tryCatch(asNamespace("filearray"), error = function(e) { NULL })
        # a regression should fail the test, not hang it
        options(filearray.lock.timeout = 30)
        !is.null(ns) &&
            identical(get0("LOCK_PROTOCOL_VERSION", envir = ns, inherits = FALSE), version) &&
            (is.null(need) || exists(need, envir = ns, inherits = FALSE))
    })
    ok <- tryCatch(
        all(unlist(parallel::clusterCall(cl, setup, .libPaths(), LOCK_PROTOCOL_VERSION, need))),
        error = function(e) { FALSE }
    )
    if (!ok) {
        try(parallel::stopCluster(cl), silent = TRUE)
        testthat::skip("Workers load a filearray build without partition locks; install this build first")
    }
    cl
}
