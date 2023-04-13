library(testthat)
library(filearray)

# Sys.setenv("FILEARRAY_SKIP_COLLAPSE" = "TRUE")

cat(utils::capture.output({
    print(Sys.getenv())
}), sep = "\n", file = stderr())

test_check("filearray")

filearray:::clear_cache()
