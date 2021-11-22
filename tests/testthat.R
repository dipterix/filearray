library(testthat)

# Only use 2 cores on CRAN
if(isTRUE(testthat:::on_cran())){
    cat("--as-cran: Using ", filearray::filearray_threads(2L), " threads\n")
} else {
    cat("Not on CRAN, using ", filearray::filearray_threads(8L), " threads\n")
}


library(filearray)

test_check("filearray")
