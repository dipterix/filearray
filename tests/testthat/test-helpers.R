test_that("is_same_dim", {
    
    # sanity check
    testthat::expect_true(is_same_dim(array(1:12, c(3,4)), matrix(NA, nrow = 3, ncol = 4)))
    testthat::expect_false(is_same_dim(array(1:12, c(3,4)), matrix(NA, nrow = 3, ncol = 3)))
    testthat::expect_false(is_same_dim(array(1:12, c(3,4)), array(NA, c(3,4,1))))
    testthat::expect_false(is_same_dim(array(1:12, c(3,4)), array(NA, c(4, 3))))
    
    testthat::expect_false(is_same_dim(array(NA, c(3,4)), list()))
    testthat::expect_false(is_same_dim(array(NA, c(3,4)), data.frame()))
    testthat::expect_false(is_same_dim(array(NA, c(3,4)), NULL))
    testthat::expect_false(is_same_dim(array(NA, c(3,4)), 1))
    
    # number of margins <= 1
    testthat::expect_true(is_same_dim(1, 1))
    testthat::expect_false(is_same_dim(1, 1:3))
    testthat::expect_false(is_same_dim(1, 1:3))
    testthat::expect_true(is_same_dim(NULL, numeric()))
    
    # margin =1 but with dim
    x <- 1:5
    dim(x) <- 5
    testthat::expect_true(is_same_dim(x, 1:5))
    
})

test_that("operation_output_type", {
    double_type <- "float"
    options("filearray.operator.precision" = double_type)
    on.exit({
        options("filearray.operator.precision" = NULL)
    })
    
    # complex
    type1 <- "complex"
    for(type2 in c("complex", "double", "float", "integer", "logical")) {
        testthat::expect_equal(operation_output_type(type1, type2), type1)
    }
    testthat::expect_equal(operation_output_type(type1, 'raw', raw = "integer"), type1)
    testthat::expect_error(operation_output_type(type1, "raw"))
    
    # double/float
    type1 <- "double"
    for(type2 in c("double", "float", "integer", "logical")) {
        testthat::expect_equal(operation_output_type(type1, type2), double_type)
    }
    testthat::expect_equal(operation_output_type(type1, 'raw', raw = "integer"), double_type)
    testthat::expect_error(operation_output_type(type1, "raw"))
    
    type1 <- "float"
    for(type2 in c("double", "float", "integer", "logical")) {
        testthat::expect_equal(operation_output_type(type1, type2), double_type)
    }
    testthat::expect_equal(operation_output_type(type1, 'raw', raw = "integer"), double_type)
    testthat::expect_error(operation_output_type(type1, "raw"))
    
    # integer
    type1 <- "integer"
    for(type2 in c("integer", "logical")) {
        testthat::expect_equal(operation_output_type(type1, type2), type1)
    }
    testthat::expect_equal(operation_output_type(type1, 'raw', raw = "integer"), type1)
    testthat::expect_error(operation_output_type(type1, "raw"))
    
    # logical
    testthat::expect_equal(operation_output_type("logical", 'logical'), 'integer')
    testthat::expect_equal(operation_output_type("logical", 'logical', logical = "logical"), 'logical')
    testthat::expect_equal(operation_output_type('logical', 'raw', raw = "integer"), 'integer')
    testthat::expect_error(operation_output_type('logical', "raw"))
    
    # raw
    testthat::expect_equal(operation_output_type('raw', 'raw', raw = "integer"), 'integer')
    testthat::expect_error(operation_output_type('raw', "raw"))
    
})

