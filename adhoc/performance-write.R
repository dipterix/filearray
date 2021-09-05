devtools::load_all()
set.seed(1)
dim <- c(500, 400, 100, 5)
file <- tempfile(); unlink(file, recursive = TRUE);
x <- filearray_create(file, dim, type = 'integer', partition_size = 1)
# na <- NA; storage.mode(na) <- 'integer'
na <- as.integer(NA)
v <- array(na, dim)

filebase <- paste0(x$.filebase, x$.sep)
pna <- local({
    dim[[length(dim)]] <- x$partition_size()
    rep(na, prod(dim))
})
reset <- function(){
    system.time({
        lapply(seq_len(ceiling(dim[[length(dim)]] / x$partition_size())), function(i){
            dim[[length(dim)]] <- x$partition_size()
            write_partition(
                file = x$partition_path(i),
                partition = i,
                dimension = dim,
                type = 'integer',
                value = pna
            )
        })
    })
}

sample <- function(x, ...){x}
sample <- base::sample

listOrEnv <- function(seed){
    set.seed(seed)
    lapply(dim, function(d){
        sample(seq_len(d), size = sample(seq_len(d), size = 1), replace = FALSE)
    })
}


seed = 1
locs <- listOrEnv(seed)
vals <- 1:prod(sapply(locs, length))
system.time({
    b <- local({
        locs <- listOrEnv(seed)
        v[locs[[1]], locs[[2]], locs[[3]], locs[[4]]] <- as.integer(vals)
        v
    })
})

locs <- listOrEnv(seed)
reset(); gc()
system.time({
    setThreads(5)
    locs <- listOrEnv(seed)
    x[locs[[1]], locs[[2]], locs[[3]], locs[[4]]] <- as.double(vals)
}, gcFirst = TRUE)
system.time({
    x[,,,]
})


identical(x[], b)
