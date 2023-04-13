

register_double_op <- function(op, counter_types = c("FileArray", "numeric", "complex", "logical", "array"), ...) {
    
    
    func_name <- sprintf("`op_func_%s`", op)
    s <- sprintf("%s <- function(e1, e2) {
    fa_pairwise_operator(e1, e2, op = '%s', label = parent_call(deparse = TRUE))
}\n", func_name, op)
    
    
    expr_str <- lapply(counter_types, function(ftype) {
        if( ftype == "FileArray" ) {
            re <- deparse1(bquote(setMethod(.(op), signature(e1 = "FileArray", e2 = "FileArray"), .(str2lang(func_name)))))
        } else {
            re <- c(
                deparse1(bquote(setMethod(.(op), signature(e1 = "FileArray", e2 = .(ftype)), .(str2lang(func_name))))),
                deparse1(bquote(setMethod(.(op), signature(e1 = .(ftype), e2 = "FileArray"), .(str2lang(func_name)))))
            )
        }
        paste0("#' @rdname S4-filearray\n", re, "\n")
    })
    expr_str <- unlist(c(s, expr_str))
    cat(expr_str, "\n", sep  = "\n")
    # op_func <- function(e1, e2) {
    #     fa_pairwise_operator(e1, e2, op = op, label = parent_call(deparse = TRUE), ...)
    # }
    # 
    # for(ftype in counter_types) {
    #     if( ftype == "FileArray" ) {
    #         setMethod(op, signature(e1 = "FileArray", e2 = "FileArray"), op_func)
    #     } else {
    #         setMethod(op, signature(e1 = "FileArray", e2 = ftype), op_func)
    #         setMethod(op, signature(e1 = ftype, e2 = "FileArray"), op_func)
    #     }
    # }
    
    rstudioapi <- asNamespace("rstudioapi")
    ctx <- rstudioapi$getActiveDocumentContext()
    line <- ctx$selection[[1]]$range$end[[1]]
    rstudioapi$insertText(c(line + 1, 1), paste(c(unlist(expr_str), ""), collapse = "\n"))
    invisible()
}


# Arith
# register_double_op("+")
`op_func_+` <- function(e1, e2) {
    fa_pairwise_operator(e1, e2, op = '+', label = parent_call(deparse = TRUE))
}

#' @rdname S4-filearray
setMethod("+", signature(e1 = "FileArray", e2 = "FileArray"),      `op_func_+`)

#' @rdname S4-filearray
setMethod("+", signature(e1 = "FileArray", e2 = "numeric"), `op_func_+`)

#' @rdname S4-filearray
setMethod("+", signature(e1 = "numeric", e2 = "FileArray"), `op_func_+`)

#' @rdname S4-filearray
setMethod("+", signature(e1 = "FileArray", e2 = "complex"), `op_func_+`)

#' @rdname S4-filearray
setMethod("+", signature(e1 = "complex", e2 = "FileArray"), `op_func_+`)

#' @rdname S4-filearray
setMethod("+", signature(e1 = "FileArray", e2 = "logical"), `op_func_+`)

#' @rdname S4-filearray
setMethod("+", signature(e1 = "logical", e2 = "FileArray"), `op_func_+`)

#' @rdname S4-filearray
setMethod("+", signature(e1 = "FileArray", e2 = "array"), `op_func_+`)

#' @rdname S4-filearray
setMethod("+", signature(e1 = "array", e2 = "FileArray"), `op_func_+`)

# register_double_op("-")
`op_func_-` <- function(e1, e2) {
    fa_pairwise_operator(e1, e2, op = '-', label = parent_call(deparse = TRUE))
}

#' @rdname S4-filearray
setMethod("-", signature(e1 = "FileArray", e2 = "FileArray"),      `op_func_-`)

#' @rdname S4-filearray
setMethod("-", signature(e1 = "FileArray", e2 = "numeric"), `op_func_-`)

#' @rdname S4-filearray
setMethod("-", signature(e1 = "numeric", e2 = "FileArray"), `op_func_-`)

#' @rdname S4-filearray
setMethod("-", signature(e1 = "FileArray", e2 = "complex"), `op_func_-`)

#' @rdname S4-filearray
setMethod("-", signature(e1 = "complex", e2 = "FileArray"), `op_func_-`)

#' @rdname S4-filearray
setMethod("-", signature(e1 = "FileArray", e2 = "logical"), `op_func_-`)

#' @rdname S4-filearray
setMethod("-", signature(e1 = "logical", e2 = "FileArray"), `op_func_-`)

#' @rdname S4-filearray
setMethod("-", signature(e1 = "FileArray", e2 = "array"), `op_func_-`)

#' @rdname S4-filearray
setMethod("-", signature(e1 = "array", e2 = "FileArray"), `op_func_-`)

# register_double_op("*")
`op_func_*` <- function(e1, e2) {
    fa_pairwise_operator(e1, e2, op = '*', label = parent_call(deparse = TRUE))
}

#' @rdname S4-filearray
setMethod("*", signature(e1 = "FileArray", e2 = "FileArray"),      `op_func_*`)

#' @rdname S4-filearray
setMethod("*", signature(e1 = "FileArray", e2 = "numeric"), `op_func_*`)

#' @rdname S4-filearray
setMethod("*", signature(e1 = "numeric", e2 = "FileArray"), `op_func_*`)

#' @rdname S4-filearray
setMethod("*", signature(e1 = "FileArray", e2 = "complex"), `op_func_*`)

#' @rdname S4-filearray
setMethod("*", signature(e1 = "complex", e2 = "FileArray"), `op_func_*`)

#' @rdname S4-filearray
setMethod("*", signature(e1 = "FileArray", e2 = "logical"), `op_func_*`)

#' @rdname S4-filearray
setMethod("*", signature(e1 = "logical", e2 = "FileArray"), `op_func_*`)

#' @rdname S4-filearray
setMethod("*", signature(e1 = "FileArray", e2 = "array"), `op_func_*`)

#' @rdname S4-filearray
setMethod("*", signature(e1 = "array", e2 = "FileArray"), `op_func_*`)

# register_double_op("/")
`op_func_/` <- function(e1, e2) {
    fa_pairwise_operator(e1, e2, op = '/', label = parent_call(deparse = TRUE))
}

#' @rdname S4-filearray
setMethod("/", signature(e1 = "FileArray", e2 = "FileArray"),      `op_func_/`)

#' @rdname S4-filearray
setMethod("/", signature(e1 = "FileArray", e2 = "numeric"), `op_func_/`)

#' @rdname S4-filearray
setMethod("/", signature(e1 = "numeric", e2 = "FileArray"), `op_func_/`)

#' @rdname S4-filearray
setMethod("/", signature(e1 = "FileArray", e2 = "complex"), `op_func_/`)

#' @rdname S4-filearray
setMethod("/", signature(e1 = "complex", e2 = "FileArray"), `op_func_/`)

#' @rdname S4-filearray
setMethod("/", signature(e1 = "FileArray", e2 = "logical"), `op_func_/`)

#' @rdname S4-filearray
setMethod("/", signature(e1 = "logical", e2 = "FileArray"), `op_func_/`)

#' @rdname S4-filearray
setMethod("/", signature(e1 = "FileArray", e2 = "array"), `op_func_/`)

#' @rdname S4-filearray
setMethod("/", signature(e1 = "array", e2 = "FileArray"), `op_func_/`)

# register_double_op("^")
`op_func_^` <- function(e1, e2) {
    fa_pairwise_operator(e1, e2, op = '^', label = parent_call(deparse = TRUE))
}

#' @rdname S4-filearray
setMethod("^", signature(e1 = "FileArray", e2 = "FileArray"),      `op_func_^`)

#' @rdname S4-filearray
setMethod("^", signature(e1 = "FileArray", e2 = "numeric"), `op_func_^`)

#' @rdname S4-filearray
setMethod("^", signature(e1 = "numeric", e2 = "FileArray"), `op_func_^`)

#' @rdname S4-filearray
setMethod("^", signature(e1 = "FileArray", e2 = "complex"), `op_func_^`)

#' @rdname S4-filearray
setMethod("^", signature(e1 = "complex", e2 = "FileArray"), `op_func_^`)

#' @rdname S4-filearray
setMethod("^", signature(e1 = "FileArray", e2 = "logical"), `op_func_^`)

#' @rdname S4-filearray
setMethod("^", signature(e1 = "logical", e2 = "FileArray"), `op_func_^`)

#' @rdname S4-filearray
setMethod("^", signature(e1 = "FileArray", e2 = "array"), `op_func_^`)

#' @rdname S4-filearray
setMethod("^", signature(e1 = "array", e2 = "FileArray"), `op_func_^`)

# register_double_op("%%")
`op_func_%%` <- function(e1, e2) {
    fa_pairwise_operator(e1, e2, op = '%%', label = parent_call(deparse = TRUE))
}

#' @rdname S4-filearray
setMethod("%%", signature(e1 = "FileArray", e2 = "FileArray"),      `op_func_%%`)

#' @rdname S4-filearray
setMethod("%%", signature(e1 = "FileArray", e2 = "numeric"),      `op_func_%%`)

#' @rdname S4-filearray
setMethod("%%", signature(e1 = "numeric", e2 = "FileArray"),      `op_func_%%`)

#' @rdname S4-filearray
setMethod("%%", signature(e1 = "FileArray", e2 = "complex"),      `op_func_%%`)

#' @rdname S4-filearray
setMethod("%%", signature(e1 = "complex", e2 = "FileArray"),      `op_func_%%`)

#' @rdname S4-filearray
setMethod("%%", signature(e1 = "FileArray", e2 = "logical"),      `op_func_%%`)

#' @rdname S4-filearray
setMethod("%%", signature(e1 = "logical", e2 = "FileArray"),      `op_func_%%`)

#' @rdname S4-filearray
setMethod("%%", signature(e1 = "FileArray", e2 = "array"), `op_func_%%`)

#' @rdname S4-filearray
setMethod("%%", signature(e1 = "array", e2 = "FileArray"), `op_func_%%`)

# register_double_op("%/%")
`op_func_%/%` <- function(e1, e2) {
    fa_pairwise_operator(e1, e2, op = '%/%', label = parent_call(deparse = TRUE), out_type = "integer")
}

#' @rdname S4-filearray
setMethod("%/%", signature(e1 = "FileArray", e2 = "FileArray"),      `op_func_%/%`)

#' @rdname S4-filearray
setMethod("%/%", signature(e1 = "FileArray", e2 = "numeric"),      `op_func_%/%`)

#' @rdname S4-filearray
setMethod("%/%", signature(e1 = "numeric", e2 = "FileArray"),      `op_func_%/%`)

#' @rdname S4-filearray
setMethod("%/%", signature(e1 = "FileArray", e2 = "complex"),      `op_func_%/%`)

#' @rdname S4-filearray
setMethod("%/%", signature(e1 = "complex", e2 = "FileArray"),      `op_func_%/%`)

#' @rdname S4-filearray
setMethod("%/%", signature(e1 = "FileArray", e2 = "logical"),      `op_func_%/%`)

#' @rdname S4-filearray
setMethod("%/%", signature(e1 = "logical", e2 = "FileArray"),      `op_func_%/%`)

#' @rdname S4-filearray
setMethod("%/%", signature(e1 = "FileArray", e2 = "array"), `op_func_%/%`)

#' @rdname S4-filearray
setMethod("%/%", signature(e1 = "array", e2 = "FileArray"), `op_func_%/%`)


# Compare
# register_double_op("==")
`op_func_==` <- function(e1, e2) {
    fa_pairwise_operator(e1, e2, op = '==', label = parent_call(deparse = TRUE), out_type = "logical")
}

#' @rdname S4-filearray
setMethod("==", signature(e1 = "FileArray", e2 = "FileArray"),      `op_func_==`)

#' @rdname S4-filearray
setMethod("==", signature(e1 = "FileArray", e2 = "numeric"),      `op_func_==`)

#' @rdname S4-filearray
setMethod("==", signature(e1 = "numeric", e2 = "FileArray"),      `op_func_==`)

#' @rdname S4-filearray
setMethod("==", signature(e1 = "FileArray", e2 = "complex"),      `op_func_==`)

#' @rdname S4-filearray
setMethod("==", signature(e1 = "complex", e2 = "FileArray"),      `op_func_==`)

#' @rdname S4-filearray
setMethod("==", signature(e1 = "FileArray", e2 = "logical"),      `op_func_==`)

#' @rdname S4-filearray
setMethod("==", signature(e1 = "logical", e2 = "FileArray"),      `op_func_==`)

#' @rdname S4-filearray
setMethod("==", signature(e1 = "FileArray", e2 = "array"), `op_func_==`)

#' @rdname S4-filearray
setMethod("==", signature(e1 = "array", e2 = "FileArray"), `op_func_==`)

# register_double_op(">")
`op_func_>` <- function(e1, e2) {
    fa_pairwise_operator(e1, e2, op = '>', label = parent_call(deparse = TRUE), out_type = "logical")
}

#' @rdname S4-filearray
setMethod(">", signature(e1 = "FileArray", e2 = "FileArray"),      `op_func_>`)

#' @rdname S4-filearray
setMethod(">", signature(e1 = "FileArray", e2 = "numeric"), `op_func_>`)

#' @rdname S4-filearray
setMethod(">", signature(e1 = "numeric", e2 = "FileArray"), `op_func_>`)

#' @rdname S4-filearray
setMethod(">", signature(e1 = "FileArray", e2 = "complex"), `op_func_>`)

#' @rdname S4-filearray
setMethod(">", signature(e1 = "complex", e2 = "FileArray"), `op_func_>`)

#' @rdname S4-filearray
setMethod(">", signature(e1 = "FileArray", e2 = "logical"), `op_func_>`)

#' @rdname S4-filearray
setMethod(">", signature(e1 = "logical", e2 = "FileArray"), `op_func_>`)

#' @rdname S4-filearray
setMethod(">", signature(e1 = "FileArray", e2 = "array"), `op_func_>`)

#' @rdname S4-filearray
setMethod(">", signature(e1 = "array", e2 = "FileArray"), `op_func_>`)

# register_double_op("<")
`op_func_<` <- function(e1, e2) {
    fa_pairwise_operator(e1, e2, op = '<', label = parent_call(deparse = TRUE), out_type = "logical")
}

#' @rdname S4-filearray
setMethod("<", signature(e1 = "FileArray", e2 = "FileArray"),      `op_func_<`)

#' @rdname S4-filearray
setMethod("<", signature(e1 = "FileArray", e2 = "numeric"), `op_func_<`)

#' @rdname S4-filearray
setMethod("<", signature(e1 = "numeric", e2 = "FileArray"), `op_func_<`)

#' @rdname S4-filearray
setMethod("<", signature(e1 = "FileArray", e2 = "complex"), `op_func_<`)

#' @rdname S4-filearray
setMethod("<", signature(e1 = "complex", e2 = "FileArray"), `op_func_<`)

#' @rdname S4-filearray
setMethod("<", signature(e1 = "FileArray", e2 = "logical"), `op_func_<`)

#' @rdname S4-filearray
setMethod("<", signature(e1 = "logical", e2 = "FileArray"), `op_func_<`)

#' @rdname S4-filearray
setMethod("<", signature(e1 = "FileArray", e2 = "array"), `op_func_<`)

#' @rdname S4-filearray
setMethod("<", signature(e1 = "array", e2 = "FileArray"), `op_func_<`)

# register_double_op("!=")
`op_func_!=` <- function(e1, e2) {
    fa_pairwise_operator(e1, e2, op = '!=', label = parent_call(deparse = TRUE), out_type = "logical")
}

#' @rdname S4-filearray
setMethod("!=", signature(e1 = "FileArray", e2 = "FileArray"),      `op_func_!=`)

#' @rdname S4-filearray
setMethod("!=", signature(e1 = "FileArray", e2 = "numeric"),      `op_func_!=`)

#' @rdname S4-filearray
setMethod("!=", signature(e1 = "numeric", e2 = "FileArray"),      `op_func_!=`)

#' @rdname S4-filearray
setMethod("!=", signature(e1 = "FileArray", e2 = "complex"),      `op_func_!=`)

#' @rdname S4-filearray
setMethod("!=", signature(e1 = "complex", e2 = "FileArray"),      `op_func_!=`)

#' @rdname S4-filearray
setMethod("!=", signature(e1 = "FileArray", e2 = "logical"),      `op_func_!=`)

#' @rdname S4-filearray
setMethod("!=", signature(e1 = "logical", e2 = "FileArray"),      `op_func_!=`)

#' @rdname S4-filearray
setMethod("!=", signature(e1 = "FileArray", e2 = "array"), `op_func_!=`)

#' @rdname S4-filearray
setMethod("!=", signature(e1 = "array", e2 = "FileArray"), `op_func_!=`)

# register_double_op(">=")
`op_func_>=` <- function(e1, e2) {
    fa_pairwise_operator(e1, e2, op = '>=', label = parent_call(deparse = TRUE), out_type = "logical")
}

#' @rdname S4-filearray
setMethod(">=", signature(e1 = "FileArray", e2 = "FileArray"),      `op_func_>=`)

#' @rdname S4-filearray
setMethod(">=", signature(e1 = "FileArray", e2 = "numeric"),      `op_func_>=`)

#' @rdname S4-filearray
setMethod(">=", signature(e1 = "numeric", e2 = "FileArray"),      `op_func_>=`)

#' @rdname S4-filearray
setMethod(">=", signature(e1 = "FileArray", e2 = "complex"),      `op_func_>=`)

#' @rdname S4-filearray
setMethod(">=", signature(e1 = "complex", e2 = "FileArray"),      `op_func_>=`)

#' @rdname S4-filearray
setMethod(">=", signature(e1 = "FileArray", e2 = "logical"),      `op_func_>=`)

#' @rdname S4-filearray
setMethod(">=", signature(e1 = "logical", e2 = "FileArray"),      `op_func_>=`)

#' @rdname S4-filearray
setMethod(">=", signature(e1 = "FileArray", e2 = "array"), `op_func_>=`)

#' @rdname S4-filearray
setMethod(">=", signature(e1 = "array", e2 = "FileArray"), `op_func_>=`)

# register_double_op("<=")
`op_func_<=` <- function(e1, e2) {
    fa_pairwise_operator(e1, e2, op = '<=', label = parent_call(deparse = TRUE), out_type = "logical")
}

#' @rdname S4-filearray
setMethod("<=", signature(e1 = "FileArray", e2 = "FileArray"),      `op_func_<=`)

#' @rdname S4-filearray
setMethod("<=", signature(e1 = "FileArray", e2 = "numeric"),      `op_func_<=`)

#' @rdname S4-filearray
setMethod("<=", signature(e1 = "numeric", e2 = "FileArray"),      `op_func_<=`)

#' @rdname S4-filearray
setMethod("<=", signature(e1 = "FileArray", e2 = "complex"),      `op_func_<=`)

#' @rdname S4-filearray
setMethod("<=", signature(e1 = "complex", e2 = "FileArray"),      `op_func_<=`)

#' @rdname S4-filearray
setMethod("<=", signature(e1 = "FileArray", e2 = "logical"),      `op_func_<=`)

#' @rdname S4-filearray
setMethod("<=", signature(e1 = "logical", e2 = "FileArray"),      `op_func_<=`)

#' @rdname S4-filearray
setMethod("<=", signature(e1 = "FileArray", e2 = "array"), `op_func_<=`)

#' @rdname S4-filearray
setMethod("<=", signature(e1 = "array", e2 = "FileArray"), `op_func_<=`)


# Logic
# register_double_op("&")
`op_func_&` <- function(e1, e2) {
    fa_pairwise_operator(e1, e2, op = '&', label = parent_call(deparse = TRUE), out_type = "logical")
}

#' @rdname S4-filearray
setMethod("&", signature(e1 = "FileArray", e2 = "FileArray"),      `op_func_&`)

#' @rdname S4-filearray
setMethod("&", signature(e1 = "FileArray", e2 = "numeric"), `op_func_&`)

#' @rdname S4-filearray
setMethod("&", signature(e1 = "numeric", e2 = "FileArray"), `op_func_&`)

#' @rdname S4-filearray
setMethod("&", signature(e1 = "FileArray", e2 = "complex"), `op_func_&`)

#' @rdname S4-filearray
setMethod("&", signature(e1 = "complex", e2 = "FileArray"), `op_func_&`)

#' @rdname S4-filearray
setMethod("&", signature(e1 = "FileArray", e2 = "logical"), `op_func_&`)

#' @rdname S4-filearray
setMethod("&", signature(e1 = "logical", e2 = "FileArray"), `op_func_&`)

#' @rdname S4-filearray
setMethod("&", signature(e1 = "FileArray", e2 = "array"), `op_func_&`)

#' @rdname S4-filearray
setMethod("&", signature(e1 = "array", e2 = "FileArray"), `op_func_&`)

# register_double_op("|")
`op_func_|` <- function(e1, e2) {
    fa_pairwise_operator(e1, e2, op = '|', label = parent_call(deparse = TRUE), out_type = "logical")
}

#' @rdname S4-filearray
setMethod("|", signature(e1 = "FileArray", e2 = "FileArray"),      `op_func_|`)

#' @rdname S4-filearray
setMethod("|", signature(e1 = "FileArray", e2 = "numeric"), `op_func_|`)

#' @rdname S4-filearray
setMethod("|", signature(e1 = "numeric", e2 = "FileArray"), `op_func_|`)

#' @rdname S4-filearray
setMethod("|", signature(e1 = "FileArray", e2 = "complex"), `op_func_|`)

#' @rdname S4-filearray
setMethod("|", signature(e1 = "complex", e2 = "FileArray"), `op_func_|`)

#' @rdname S4-filearray
setMethod("|", signature(e1 = "FileArray", e2 = "logical"), `op_func_|`)

#' @rdname S4-filearray
setMethod("|", signature(e1 = "logical", e2 = "FileArray"), `op_func_|`)

#' @rdname S4-filearray
setMethod("|", signature(e1 = "FileArray", e2 = "array"), `op_func_|`)

#' @rdname S4-filearray
setMethod("|", signature(e1 = "array", e2 = "FileArray"), `op_func_|`)


