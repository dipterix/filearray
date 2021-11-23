#' @title Set or get file array threads
#' @description Will enable/disable multi-threaded reading or writing
#' at \code{C++} level using 'OpenMP'. 
#' @param n number of threads to set. If \code{n} is negative,
#' then default to the number of cores that computer has. 
#' @param reset_after_fork when forking a cluster (in \code{'osx'} or \code{'linux'}),
#' the number of threads will be set to one to avoid memory issues. 
#' Setting this to \code{1} will reset threads once forked 
#' clusters are shut down. Setting to \code{0} will disable reset, 
#' and \code{-1} to use last set values.
#' @return An integer of current number of threads
#' @export
filearray_threads <- function(n, reset_after_fork = -1L){
    
    if(!missing(n)){
        n <- as.integer(n)
        reset_after_fork <- sign(reset_after_fork)
        setThreads(n, reset_after_fork)
    }
    return(getThreads())
}
