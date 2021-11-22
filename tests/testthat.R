library(testthat)

# Only use 2 cores on CRAN
if(isTRUE(testthat:::on_cran())){
    message("--as-cran: Using ", filearray::filearray_threads(2L), " threads")
} else {
    message("Not on CRAN, using ", filearray::filearray_threads(8L), " threads")
}


library(filearray)

test_check("filearray")
