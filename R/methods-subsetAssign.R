# setMethod("[<-",
#     signature(x = "Filearray"),
#     function (x, i, j, ..., value) 
#     {
#         stop("need a definition for the method here")
#     }
# )

# DIPSAUS DEBUG START
# verbose <- TRUE
# z = filearray_create(temp_path(), c(2,3,4))
# z[] <- 1:24
# x <- z+1
# i <- as_filearray(z[] < 5)

# `i` is a filearray
fa_subsetAssign2 <- function(x, i, value, label = "subset-assign (lazy)") {
    stopifnot(is_filearray(x) && is_filearray(i))
    
    if(typeof(i) != "logical") {
        stop("`fa_subsetAssign2`: subset index filearray must be logical")
    }
    
    e1 <- as_filearrayproxy(x)
    e2 <- as_filearrayproxy(i)
    uuid1 <- e1$uuid()
    uuid2 <- e2$uuid()
    e1$link_proxy( e2 )
    
    # check input types 
    out_type <- typeof(e1)
    
    starting_idx <- 0
    value_len <- length(value)
    
    op_func <- function(value_list, ...) {
        data <- value_list[[ uuid1 ]]
        
        idx <- value_list[[ uuid2 ]]
        n_assigned <- sum(idx)
        if(n_assigned == 0) { return(data) }
        
        if( value_len == 1L ) {
            data[ idx ] <- value
        } else {
            data[ idx ] <- value[starting_idx + seq_len(n_assigned)]
            starting_idx <<- starting_idx + n_assigned
        }
        
        return(data)
    }
    
    e1$add_operator(op_func,
                    out_type = out_type,
                    context = "array_subset_assign",
                    label = label)
    
    return( e1 )
}
