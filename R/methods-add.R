
register_double_op <- function(op, counter_types = c("FileArray", "numeric", "complex", "logical", "array"), ...) {
    op_func <- function(e1, e2) {
        fa_pairwise_operator(e1, e2, op = op, label = parent_call(deparse = TRUE), ...)
    }
    
    for(ftype in counter_types) {
        if( ftype == "FileArray" ) {
            setMethod(op, signature(e1 = "FileArray", e2 = "FileArray"), op_func)
        } else {
            setMethod(op, signature(e1 = "FileArray", e2 = ftype), op_func)
            setMethod(op, signature(e1 = ftype, e2 = "FileArray"), op_func)
        }
    }
}

# Arith
register_double_op("+")
register_double_op("-")
register_double_op("*")
register_double_op("/")
register_double_op("^")
register_double_op("%%")
register_double_op("%/%")

# Compare
register_double_op("==")
register_double_op(">")
register_double_op("<")
register_double_op("!=")
register_double_op(">=")
register_double_op("<=")

# Logic
register_double_op("&")
register_double_op("|")

