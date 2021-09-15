require(filearray)
dim <- c(100,100,10,40)
set.seed(1); 
tmp <- seq_len(1e5)
xs <- lapply(1:2, function(i){
    file <- tempfile(); unlink(file, recursive = TRUE)
    x <- filearray_create(file, dim, type = 'double')
    for(i in 1:40){
        x[,,,i] <- tmp
    }
    x
})
set.seed(2); file <- tempfile(); unlink(file, recursive = TRUE)
y <- filearray_create(file, c(100,10,40), type = 'double')
y$initialize_partition()


system.time({
    z <- xs[[1]]$collapse(keep = c(2,3,4), method = "sum")
})
system.time({
    fmap(xs, function(x, a){
        # a <<- c(a, Sys.time() - now)
        z <- x[[1]] + x[[2]]
        dim(z) <- c(100,100,10)
        z <- dipsaus::collapse(z, keep = c(2,3))
        as.list(z + a)
    }, .y = y, .input_size = 100000, .output_size = 1000,
    a = 2
    )
})
range(y[] - z*2)

# aa <- c()
system.time({
    now <- Sys.time()
        # aa <<- c(aa, Sys.time() - now)
        y[] <- xs[[1]][] + xs[[2]][]
})

# filearray:::setThreads(1)
filearray:::set_buffer_size(2097152)
filearray:::set_buffer_size(8000000)
filearray:::get_buffer_size()
filearray_threads(8)
env <- new.env(parent = emptyenv())
env$a <- matrix(NA_real_, nrow = 4, ncol = ceiling(length(y) / filearray:::get_buffer_size() * 8))
env$count = 1;
system.time({
    now <- Sys.time()
    fmap_element_wise(xs, function(input) {
        input[[1]] + input[[2]]
    }, y
    , profile = function(){
        env$a[[env$count]] <- Sys.time() - now
        now <<- Sys.time()
        env$count <- env$count + 1
    }
    )
})
b <- t(env$a)
colSums(b, na.rm = TRUE)
summary(b)

range(y[] - xs[[1]][] - xs[[2]][])
