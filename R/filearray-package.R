## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib filearray, .registration = TRUE
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname){
    if(hasOpenMP()){
        n <- filearray_threads(-1)
        if(n > 8){
            filearray_threads(8)
        }
    }
    ns <- asNamespace(pkgname)
    ns$NA_float_ <- get_float_na()
}

.onAttach <- function(libname, pkgname){
    if(hasOpenMP()){
        packageStartupMessage(
            "OpenMP detected: currently using ", filearray_threads(), " threads.")
    } else {
        packageStartupMessage("OpenMP not detected. Using single thread only.")
    }
}
