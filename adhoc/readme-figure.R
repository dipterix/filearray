dim <- c(100,200,200,100)
lazyx <- lazyarray::create_lazyarray(
    tempfile(), storage_format = 'double', dim = dim)
lazyx[] <- NA
filex <- filearray::filearray_create(
    tempfile(), dim, 'double')
filex$initialize_partition()

set.seed(1)
tmp <- rnorm(4e7)
res1 <- microbenchmark::microbenchmark(
    # lazyarray = {
    #     for(i in 1:10){
    #         lazyx[,,,(i-1)*10 +1:10] <- tmp
    #     }
    # },
    filearray = {
        for(i in 1:10){
            filex[,,,(i-1)*10 +1:10] <- tmp
        }
    }, times = 1
); res1



speed1 <- sapply(split(res1, res1$expr), function(res){
    speed <- matrix(800*4 / res$time, nrow = 10) * 1e9
    c(mean(speed), sd(speed) / 2)
}); speed1


res2 <- microbenchmark::microbenchmark(
    lazyarray = {
        for(i in 1:10){
            lazyx[,,,1:10 + (i-1) * 10]
        }
    },
    filex = {
        for(i in 1:10){
            filex[,,,1:10 + (i-1) * 10]
        }
    }, times = 10, setup = quote(gc())
)
speed2 <- sapply(split(res2, res2$expr), function(res){
    speed <- matrix(3200 / res$time, nrow = 10) * 1e9
    c(mean(speed), sd(speed) / 3)
})



set.seed(1)
locs <- lapply(dim, function(d){
    sample(1:d, replace = FALSE, size = sample(50:d, 1))
})


microbenchmark::microbenchmark(
    lazyarray = {
        lazyx[locs[[1]],locs[[2]],locs[[3]],locs[[4]]]
    }, times = 10, setup = quote(gc()))
# Unit: milliseconds
# expr      min       lq     mean   median       uq
# lazyarray 971.9185 1008.228 1083.982 1094.343 1139.947
# max neval
# 1212.9    10
microbenchmark::microbenchmark(
    filearray = {
        filex[locs[[1]],locs[[2]],locs[[3]],locs[[4]]]
    }, times = 10, setup = quote(gc()))
# Unit: milliseconds
# expr      min       lq     mean   median       uq
# filearray 68.79333 71.39941 282.4135 215.8452 429.8881
# max neval
# 666.0666    10

z <- filex[]
microbenchmark::microbenchmark(
    nativeR = {
        z[locs[[1]],locs[[2]],locs[[3]],locs[[4]]]
    }, times = 10, setup = quote(gc()))
# Unit: milliseconds
# expr     min     lq     mean   median      uq      max
# nativeR 169.609 173.31 222.3218 203.4826 246.041 392.5921
# neval
# 10
speed3 <- matrix(nrow = 1, prod(sapply(locs, length)) / 1e8 * 800 / c(
    1094.343, 215.8452, 203.4826
))

tmp <- rnorm(prod(sapply(locs, length)))
rm(z); gc()
res4 <- microbenchmark::microbenchmark(
    # lazyarray = {
    #     lazyx[locs[[1]],locs[[2]],locs[[3]],locs[[4]]] <- tmp
    # },
    filearray = {
        filex[locs[[1]],locs[[2]],locs[[3]],locs[[4]]] <- tmp
    }, times = 10, setup = quote(gc())
)
sapply(split(res4, res4$expr), function(res){
    speed <- matrix(prod(sapply(locs, length)) / 1e8 * 800 / res$time, nrow = 10) * 1e9
    c(mean(speed), sd(speed) / 3)
})
# lazyarray filearray
# [1,] 38.8646985 394.38949
# [2,]  0.7159783  32.14047

z <- filex[]
res5 <- microbenchmark::microbenchmark(
    nativeR = {
        z[locs[[1]],locs[[2]],locs[[3]],locs[[4]]] <- tmp
    }, times = 10, setup = quote(gc())
)
rm(z); gc()
mean(prod(sapply(locs, length)) / 1e8 * 800 / res5$time) * 1e9
# 931.8366
    
speed4 <- matrix(nrow = 1, c(38.8646985, 394.38949, 931.8366)) / 1024

f <- function(){
    
    par(mfrow = c(1,4))
    
    cols <- ravebuiltins:::group_colors[1:2]
    cols2 <- ravebuiltins:::group_colors[1:3]
    
    speed <- cbind(
        speed1[1,],
        speed2[1,]
    ) / 1024
    rownames(speed) <- c("lazyarray", "filearray")
    colnames(speed) <- c("Write", "Read")
    
    txt_cex = 1.2
    txt_cex2 = 1.2
    
    plt <- barplot.default(
        speed[,1,drop=FALSE], beside = TRUE,
        ylab = "Speed (GB/s)", 
        col = dipsaus::col2hexStr(cols, alpha = 0.5),
        ylim = c(0, 3), las = 1, yaxt = "n", 
        border = NA,
        main = "Write 3GB Data", 
        cex.names = 1.4, cex.lab = 1.4, cex.main = 1.4
    )
    axis(2, 0:3, las = 1)
    text.default(x = plt[,], y = speed[,1], 
                 labels = sprintf(c("lazyarray\n%.0f MB/s","filearray\n%.0f MB/s"), speed[,1] * 1024), cex = txt_cex,
                 col = 'white')
    text.default(x = plt[,], y = speed[,1], 
                 labels = c("lazyarray\n","filearray\n"),
                 cex = txt_cex2,
                 col = cols)
    
    
    plt <- barplot.default(
        speed[,2,drop=FALSE], beside = TRUE,
        ylab = "Speed (GB/s)", 
        col = dipsaus::col2hexStr(cols, alpha = 0.5),
        ylim = c(0, 3), las = 1, yaxt = "n", 
        border = NA,
        main = "Read 3GB Data"
    )
    axis(2, c(0, 1, 2, 3), las = 1)
    text.default(x = plt[,], y = speed[,2], 
                 labels = sprintf(c("lazyarray\n%.0f MB/s","filearray\n%.0f MB/s"), speed[,2] * 1024), cex = txt_cex,
                 col = 'white')
    text.default(x = plt[,], y = speed[,2], 
                 labels = c("lazyarray\n","filearray\n"),
                 cex = txt_cex2,
                 col = cols)
    
    speed <- cbind(
        speed3[1,],
        speed4[1,]
    )
    rownames(speed) <- c("lazyarray", "filearray", "in-memory")
    colnames(speed) <- c("Subset", "SubsetAssign")
    plt <- barplot.default(
        speed[,1,drop=FALSE], beside = TRUE,
        ylab = "Speed (GB/s)", 
        col = dipsaus::col2hexStr(cols2, alpha = 0.5),
        ylim = c(0, 3), las = 1, yaxt = "n", 
        border = NA,
        main = "Randomly Subset\n800MB Data"
    )
    axis(2, c(0, 1, 2, 3), las = 1)
    text.default(x = plt, y = speed[,1], 
                 labels = sprintf(
                     c("lazyarray\n%.0f MB/s",
                       "filearray\n%.0f MB/s",
                       "in-memory\n%.0f MB/s"), 
                     speed[,1]*1024), cex = txt_cex,
                 col = 'white')
    text.default(x = plt[,], y = speed[,1], 
                 labels = c("lazyarray\n", 
                            "filearray\n",
                            "in-memory\n"),
                 cex = txt_cex2,
                 col = cols2)
    
    
    plt <- barplot.default(
        speed[,2,drop=FALSE], beside = TRUE,
        ylab = "Speed (GB/s)", 
        col = dipsaus::col2hexStr(cols2, alpha = 0.5),
        ylim = c(0, 3), las = 1, yaxt = "n", 
        border = NA,
        main = "Randomly Replace\n800MB Data"
    )
    axis(2, c(0, 1, 2, 3), las = 1)
    text.default(x = plt, y = speed[,2], 
                 labels = sprintf(
                     c("lazyarray\n%.0f MB/s",
                       "filearray\n%.0f MB/s",
                       "in-memory\n%.0f MB/s"), 
                     speed[,2]*1024), cex = txt_cex,
                 col = 'white')
    text.default(x = plt[,], y = speed[,2], 
                 labels = c("lazyarray\n", 
                            "filearray\n",
                            "in-memory\n"),
                 cex = txt_cex2,
                 col = cols2)
    
}

png("./adhoc/readme-speed.png", width = 4267, height = 1600, res = 300)
f()
dev.off()
