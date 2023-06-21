#' @title Set or get file array threads
#' @description Will enable/disable multi-threaded reading or writing
#' at \code{C++} level. 
#' @param n number of threads to set. If \code{n} is negative,
#' then default to the number of cores that computer has. 
#' @param ... internally used
#' @return An integer of current number of threads
#' @export
filearray_threads <- function(n, ...){
    
    if(!missing(n)){
        setThreads(n, ...)
    }
    
    return(getThreads(FALSE))
}

setThreads <- function (n = "auto", stack_size = "auto", ...) {
    if (identical(n, "auto")) {
        n <- -1L
    } else {
        n <- as.integer(n)
        if (length(n) != 1 || is.na(n) || !is.numeric(n)) {
            stop("n must be an integer")
        }
    }
    if (identical(stack_size, "auto")) {
        stack_size <- 0L
    } else if (!is.numeric(stack_size)) {
        stop("stack_size must be an integer")
    } else {
        stack_size <- as.integer(stack_size)
    }
    if (n == -1L) {
        Sys.unsetenv("FILEARRAY_NUM_THREADS")
    } else {
        Sys.setenv(FILEARRAY_NUM_THREADS = n)
    }
    if (stack_size == 0L) {
        Sys.unsetenv("FILEARRAY_STACK_SIZE")
    } else {
        Sys.setenv(FILEARRAY_STACK_SIZE = stack_size)
    }
    invisible()
}
