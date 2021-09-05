devtools::load_all()

# 800 MB data
dim3 <- 100

set.seed(1)
file <- tempfile()
unlink(file, recursive = TRUE)

dim <- c(400, 500, dim3, 5)

arr <- filearray_create(file, dim, type = 'integer', partition_size = 2)
x <- array(as.integer(seq_len(prod(dim))), dim)
for(ii in 1:5){
    x[1,1,1,ii] <- NA
}

system.time({
    arr$set_partition(1, x[,,,1:2], lock = FALSE)
    arr$set_partition(2, x[,,,3:4], lock = FALSE)
    arr$set_partition(3, x[,,,5],,,,1,lock = FALSE)
})

basefile <- paste0(normalizePath(arr$.filebase), arr$.sep)

sample1 = function(x, ...){x}
sample1 <- base::sample
sample2 <- base::sample
replace <- TRUE
set.seed(1)
locs <- list(
    quote(x),
    drop=F,
    sample1(c(1:dim[[1]]), replace = replace),
    sample1(c(1:dim[[2]]), replace = replace),
    sample2(c(1:dim3), replace = replace),
    sample2(c(1:5), replace = replace)
)
# get_buffer_size()
# set_buffer_size(1048576)->bs; bs; log10(bs)
# set_buffer_size(2^16)
getThreads()
setThreads(1)

system.time({
    b <- do.call('[', locs)
})

# system.time({
#     a <- FARR_subset(filebase = basefile, 
#                     listOrEnv = locs[-(1:2)], 
#                     dim = dim, 
#                     cum_part_sizes = 1:5, 
#                     type = type_to_sexp('integer'), 
#                     split_dim = 3)})

system.time({
    a <- local({
        x <- arr
        do.call('[', locs)
    })
})


identical(a, b)
