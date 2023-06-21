# 
# fa_subtract <- function(e1, e2) {
#     call <- match.call()
#     call[[1]] <- quote(`-`)
#     label <- sprintf("Calculating: %s (fa_subtract)", deparse1(call))
#     
#     fa_pairwise_operator(e1, e2, op = "-", label = label)
# }
# 
# 
# 
# setMethod('-', signature(e1 = "FileArray", e2 = "FileArray"), fa_subtract)
# 
# 
# setMethod('-', signature(e1 = "FileArray", e2 = "numeric"), fa_subtract)
# 
# setMethod('-', signature(e1 = "numeric", e2 = "FileArray"), fa_subtract)
# 
# 
# setMethod('-', signature(e1 = "FileArray", e2 = "complex"), fa_subtract)
# 
# setMethod('-', signature(e1 = "complex", e2 = "FileArray"), fa_subtract)
# 
# 
# setMethod('-', signature(e1 = "FileArray", e2 = "logical"), fa_subtract)
# 
# setMethod('-', signature(e1 = "logical", e2 = "FileArray"), fa_subtract)
# 
# 
# setMethod('-', signature(e1 = "FileArray", e2 = "array"), fa_subtract)
# 
# setMethod('-', signature(e1 = "array", e2 = "FileArray"), fa_subtract)
