library(testthat)
library(filearray)


cat(utils::capture.output({
    print(Sys.getenv())
}), sep = "\n", file = stderr())

test_check("filearray")
