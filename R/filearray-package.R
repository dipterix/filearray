## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib filearray, .registration = TRUE
## usethis namespace: end
NULL

in_rcmdcheck <- function (...) {
    evidences <- list()
    args <- commandArgs()
    is_vanilla <- is.element("--vanilla", args)
    if(!is_vanilla){
        return(FALSE)
    }
    pwd <- getwd()
    dirname <- basename(pwd)
    parent <- basename(dirname(pwd))
    pattern <- ".+[.]Rcheck$"
    in_test <- (grepl(pattern, parent) && grepl("^tests(|_.*)$", dirname))
    if(!(in_test || grepl(pattern, dirname))){
        return(FALSE)
    }
    in_examples <- is.element("CheckExEnv", search())
    in_win_builder <- (.Platform$OS.type == "windows" && grepl("Rterm[.]exe$", args[1]))
    if(in_win_builder){
        n <- length(args)
        if (!all(c("--no-save", "--no-restore", "--no-site-file", 
                  "--no-init-file") %in% args)) {
            return(FALSE)
        }
        if (!grepl(pattern, parent)) {
            return(FALSE)
        }
    }
    if(in_test){
        return(structure(TRUE, status = "tests"))
    }
    if(in_examples){
        return(structure(TRUE, status = "examples"))
    }
    return(FALSE)
}


.onLoad <- function(libname, pkgname){
    if(hasOpenMP()){
        
        # Check if in R CMD check mode
        if(Sys.getenv("_R_CHECK_LIMIT_CORES_") == "TRUE"){
            #  R CMD check with --as-cran
            n <- 2L
        } else if(in_rcmdcheck()){
            #  R CMD check (without CRAN)
            n <- 2L
        } else {
            n <- filearray_threads(-1)
            if(n > 8L){
                n <- 8L
            }
        }
        
        filearray_threads(n)
    }
    ns <- asNamespace(pkgname)
    ns$NA_float_ <- get_float_na()
}

.onAttach <- function(libname, pkgname){
    if(hasOpenMP()){
        if(Sys.getenv("_R_CHECK_LIMIT_CORES_") == "TRUE"){
            packageStartupMessage(
                "Found environment variable `_R_CHECK_LIMIT_CORES_`=TRUE. Using ",
                filearray_threads(), " threads.")
        } else if(in_rcmdcheck()){
            packageStartupMessage("R CMD check mode. Using ", 
                                  filearray_threads(), " threads.")
        } else {
            packageStartupMessage(
                "OpenMP detected: currently using ", filearray_threads(), " threads.")
        }
    } else {
        packageStartupMessage("OpenMP not detected. Using single thread only.")
    }
}
