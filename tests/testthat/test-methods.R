test_that("R: FileArray-class", {
    set.seed(2)
    file <- tempfile()
    unlink(file, recursive = TRUE)
    dim <- 3:5
    x <- filearray_create(file, dim, partition_size = 3)
    
    expect_equal(x$dimension(), dim)
    expect_true(x$can_write())
    expect_equal(x$type(), 'double')
    expect_equal(x$element_size(), get_elem_size(x$type()))
    expect_error(x$fill_partition(-1, 1))
    expect_error(x$fill_partition(NA, 1))
    expect_error(x$fill_partition(0, 1))
    expect_warning(x$fill_partition(1, 1:10))
    x$fill_partition(2, 2)
    expect_equal(x[[60]], 2)
    
    unlink(file, recursive = TRUE)
    x <- filearray_create(file, dim, partition_size = 1)
    expect_equal(x$.mode, "readwrite")
    x$fill_partition(1, 2)
    
    x <- filearray_load(file, mode = 'readonly')
    expect_equal(x$.mode, "readonly")
    expect_error(x$fill_partition(1, 1))
    
    x$initialize_partition(1:2)
    expect_equal(x[1,1,1:2], c(2,NA))
    expect_equal(file.exists(x$partition_path(1:3)), c(TRUE, TRUE, FALSE))
    expect_equal(x$.mode, "readonly")
    
    expect_error(x[] <- 1:60)
    
    unlink(file, recursive = TRUE)
    # not removed by $delete
    expect_true(x$.valid)
    expect_false(x$valid())
    expect_error(x[])
    expect_error(as.array(x))
    expect_error(x[[1]])
    expect_error(mapreduce(x, I))
    expect_error(typeof(x))
    expect_error(max(x))
    
    x$delete()
    expect_false(x$.valid)
    
    unlink(file, recursive = TRUE)
})


test_that("R: S3 methods", {
    set.seed(3)
    file <- tempfile()
    unlink(file, recursive = TRUE)
    dim <- 3:5
    x <- filearray_create(file, dim, partition_size = 3)
    expect_equal(file.exists(x$partition_path(1:3)), c(FALSE, FALSE, FALSE))
    x[,,4] <- 1:12
    expect_equal(as.integer(x[,,4]), 1:12)
    expect_equal(file.exists(x$partition_path(1:3)), c(FALSE, TRUE, FALSE))
    
    expect_equal(typeof(x), 'double')
    expect_equal(range(x, na.rm = TRUE), as.double(c(1, 12)))
    expect_equal(file.exists(x$partition_path(1:3)), c(FALSE, TRUE, FALSE))
    expect_equal(range(x), as.double(c(NA, NA)))
    expect_equal(file.exists(x$partition_path(1:3)), c(TRUE, TRUE, FALSE))
    
    unlink(file, recursive = TRUE)
})
