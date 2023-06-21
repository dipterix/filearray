
arith_type_check <- function(x, complex = "complex", ...) {
    switch(
        typeof(x),
        "raw" = { stop("non-numeric argument to mathematical function") },
        "complex" = { complex },
        ...,
        { "double" }
    )
}

# Arith
# "+", "-", "*", "^", "%%", "%/%", "/"
# 
# Compare
# "==", ">", "<", "!=", "<=", ">="
# 
# Logic
# "&", "|".
# 
# Ops
# "Arith", "Compare", "Logic"
# 
# Math
# "abs", "sign", "sqrt", "ceiling", "floor", "trunc", "cummax", "cummin", "cumprod", "cumsum", "log", "log10", "log2", "log1p", "acos", "acosh", "asin", "asinh", "atan", "atanh", "exp", "expm1", "cos", "cosh", "cospi", "sin", "sinh", "sinpi", "tan", "tanh", "tanpi", "gamma", "lgamma", "digamma", "trigamma"
# 
# Math2
# "round", "signif"
# 
# Summary
# "max", "min", "range", "prod", "sum", "any", "all"
# 
# Complex
# "Arg", "Conj", "Im", "Mod", "Re"



# Methods covered in this script: 
# "acos", "acosh", "asin", "asinh", "atan", "atanh", "cos", "cosh", "cospi", "sin", "sinh", "sinpi", "tan", "tanh", "tanpi", 
# "gamma", "lgamma", "digamma", "trigamma"
# 
# Complex
# "Arg", "Conj", "Im", "Mod", "Re"

FILEARRAY_SIMPLE_OPS <- list(
    
    logic = c("!", "is.na"),
    math = c(
        "abs", "sign", "sqrt", "ceiling", "floor", "trunc", "log", "log10", "log2", 
        "log1p", "acos", "acosh", "asin", "asinh", "atan", "atanh", "exp", "expm1", 
        "cos", "cosh", "cospi", "sin", "sinh", "sinpi", "tan", "tanh", "tanpi", 
        "gamma", "lgamma", "digamma", "trigamma", "round", "signif"
    ),
    complex = c("Arg", "Conj", "Im", "Mod", "Re")
    
)


#' @rdname S4-filearray
setMethod("!", signature(x = "FileArray"), function(x) {
    fa_operator(x, op = "!", out_type = "logical", label = parent_call(deparse = TRUE))
})

# ---- exp / log ---------------------------------------------------------------

#' @rdname S4-filearray
setMethod("exp", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x)
    fa_operator(x, op = "exp", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("expm1", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x, complex = stop("unimplemented complex function"))
    fa_operator(x, op = "expm1", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("log", signature(x = "FileArray"), function(x, base = exp(1)) {
    out_type <- arith_type_check(x)
    fa_operator(x, op = "log", out_type = out_type, base = base)
})

#' @rdname S4-filearray
setMethod("log10", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x)
    fa_operator(x, op = "log10", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("log2", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x)
    fa_operator(x, op = "log2", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("log1p", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x, complex = stop("unimplemented complex function"))
    fa_operator(x, op = "log1p", out_type = out_type, label = parent_call(deparse = TRUE))
})

# ---- abs / sqrt --------------------------------------------------------------
#' @rdname S4-filearray
setMethod("abs", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x, complex = "double")
    fa_operator(x, op = "abs", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("sqrt", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x)
    fa_operator(x, op = "sqrt", out_type = out_type, label = parent_call(deparse = TRUE))
})

# ---- ceiling / round / floor / trunc / signif / sign -------------------------

#' @rdname S4-filearray
setMethod("sign", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x, complex = stop("unimplemented complex function"))
    fa_operator(x, op = "sign", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("signif", signature(x = "FileArray"), function(x, digits = 6) {
    out_type <- arith_type_check(x)
    fa_operator(x, op = "signif", out_type = out_type, label = parent_call(deparse = TRUE), digits = digits)
})

#' @rdname S4-filearray
setMethod("trunc", signature(x = "FileArray"), function(x, ...) {
    out_type <- arith_type_check(x, complex = stop("unimplemented complex function"))
    fa_operator(x, op = "trunc", out_type = out_type, label = parent_call(deparse = TRUE), ...)
})

#' @rdname S4-filearray
setMethod("floor", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x, complex = stop("unimplemented complex function"))
    fa_operator(x, op = "floor", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("ceiling", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x, complex = stop("unimplemented complex function"))
    fa_operator(x, op = "ceiling", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("round", signature(x = "FileArray"), function(x, digits = 0) {
    out_type <- arith_type_check(x)
    fa_operator(x, op = "round", out_type = out_type, label = parent_call(deparse = TRUE), digits = digits)
})

# ---- "acos", "acosh", "asin", "asinh", "atan", "atanh", "cos", "cosh", -------

#' @rdname S4-filearray
setMethod("acos", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x)
    fa_operator(x, op = "acos", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("acosh", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x)
    fa_operator(x, op = "acosh", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("asin", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x)
    fa_operator(x, op = "asin", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("asinh", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x)
    fa_operator(x, op = "asinh", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("atan", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x)
    fa_operator(x, op = "atan", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("atanh", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x)
    fa_operator(x, op = "atanh", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("cos", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x)
    fa_operator(x, op = "cos", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("cosh", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x)
    fa_operator(x, op = "cosh", out_type = out_type, label = parent_call(deparse = TRUE))
})

# ---- "cospi", "sin", "sinh", "sinpi", "tan", "tanh", "tanpi", 
#' @rdname S4-filearray
setMethod("cospi", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x, complex = stop("unimplemented complex function"))
    fa_operator(x, op = "cospi", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("sin", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x)
    fa_operator(x, op = "sin", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("sinh", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x)
    fa_operator(x, op = "sinh", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("sinpi", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x, complex = stop("unimplemented complex function"))
    fa_operator(x, op = "sinpi", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("tan", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x)
    fa_operator(x, op = "tan", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("tanh", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x)
    fa_operator(x, op = "tanh", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("tanpi", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x, complex = stop("unimplemented complex function"))
    fa_operator(x, op = "tanpi", out_type = out_type, label = parent_call(deparse = TRUE))
})

# ---- "gamma", "lgamma", "digamma", "trigamma"

#' @rdname S4-filearray
setMethod("gamma", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x, complex = stop("unimplemented complex function"))
    fa_operator(x, op = "gamma", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("lgamma", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x, complex = stop("unimplemented complex function"))
    fa_operator(x, op = "lgamma", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("digamma", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x, complex = stop("unimplemented complex function"))
    fa_operator(x, op = "digamma", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("trigamma", signature(x = "FileArray"), function(x) {
    out_type <- arith_type_check(x, complex = stop("unimplemented complex function"))
    fa_operator(x, op = "trigamma", out_type = out_type, label = parent_call(deparse = TRUE))
})



# ---- "Arg", "Conj", "Im", "Mod", "Re" ----------------------------------------
#' @rdname S4-filearray
setMethod("Arg", signature(z = "FileArray"), function(z) {
    out_type <- arith_type_check(z, complex = "double")
    fa_operator(z, op = "Arg", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("Conj", signature(z = "FileArray"), function(z) {
    out_type <- arith_type_check(z)
    fa_operator(z, op = "Conj", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("Im", signature(z = "FileArray"), function(z) {
    out_type <- arith_type_check(z, complex = "double")
    fa_operator(z, op = "Im", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("Mod", signature(z = "FileArray"), function(z) {
    out_type <- arith_type_check(z, complex = "double")
    fa_operator(z, op = "Mod", out_type = out_type, label = parent_call(deparse = TRUE))
})

#' @rdname S4-filearray
setMethod("Re", signature(z = "FileArray"), function(z) {
    out_type <- arith_type_check(z, complex = "double")
    fa_operator(z, op = "Re", out_type = out_type, label = parent_call(deparse = TRUE))
})

# ---- is.na -------------------------------------------------------------------

#' @rdname S4-filearray
setMethod("is.na", signature(x = "FileArray"), function(x) {
    fa_operator(x, op = "is.na", out_type = "logical", label = parent_call(deparse = TRUE))
})
