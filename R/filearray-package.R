## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib filearray, .registration = TRUE
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname){
    if(hasOpenMP()){
        filearray_threads(-1)
    }
}

.onAttach <- function(libname, pkgname){
    if(hasOpenMP()){
        packageStartupMessage(
            "OpenMP detected: currently using ", filearray_threads(), " threads.")
    } else {
        packageStartupMessage("OpenMP not detected. Using single thread only.")
    }
}
