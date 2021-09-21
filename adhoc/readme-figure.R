set.seed(1)
dim <- c(100,100,125,1000)
# lazyx <- lazyarray::create_lazyarray(
#     tempfile(), storage_format = 'double', dim = dim)
# lazyx[] <- NA
filex <- filearray::filearray_create(
    tempfile(), dim, 'double')
filex$initialize_partition()

set.seed(1)
tmp <- rnorm(1.25e8)
res1 <- microbenchmark::microbenchmark(
    # lazyarray = {
    #     for(i in 1:10){
    #         lazyx[,,,(i-1)*10 +1:10] <- tmp
    #     }
    # },
    filearray = {
        for(i in 1:10){
            filex[,,,i*100-0:99] <- tmp
        }
    }, times = 1, setup = quote(gc())
); res1



speed1 <- sapply(split(res1, res1$expr), function(res){
    speed <- length(filex) *8000 / res$time
    c(mean(speed), sd(speed) / 2)
}); speed1


res2 <- microbenchmark::microbenchmark(
    # lazyarray = {
    #     for(i in 1:10){
    #         lazyx[,,,1:10 + (i-1) * 10]
    #     }
    # },
    filex = {
        for(i in 1:1000){
            filex[,,,i]
        }
    }, times = 1, setup = quote(gc())
)
speed2 <- sapply(split(res2, res2$expr), function(res){
    speed <- length(filex) *8000 / res$time
    c(mean(speed), sd(speed) / 3)
}); speed2



set.seed(1)
locs <- lapply(dim, function(d){
    sample(1:d, replace = FALSE, size = sample(50:d, 1))
})

res3 <- microbenchmark::microbenchmark(
    # lazyarray = {
    #     lazyx[locs[[1]],locs[[2]],locs[[3]],locs[[4]]]
    # }, 
    filearray = {
        filex[locs[[1]],locs[[2]],locs[[3]],locs[[4]]]
    }, 
    times = 1, setup = quote(gc()))

speed3 <- sapply(split(res3, res3$expr), function(res){
    speed <- prod(sapply(locs, length)) * 8000 / res$time
    c(mean(speed), sd(speed) / 3)
}); speed3


z <- filex[]
res5 <- microbenchmark::microbenchmark(
    nativeR = {
        z[locs[[1]],locs[[2]],locs[[3]],locs[[4]]]
    }, 
    times = 10, setup = quote(gc()))
speed3 <- cbind(speed3, sapply(split(res5, res5$expr), function(res){
    speed <- matrix(prod(sapply(locs, length)) / 1e8 * 800 / res$time, nrow = 10) * 1e9
    c(mean(speed), sd(speed) / 3)
}))


rm(z); gc()
tmp <- rnorm(prod(sapply(locs, length)))
res4 <- microbenchmark::microbenchmark(
    lazyarray = {
        lazyx[locs[[1]],locs[[2]],locs[[3]],locs[[4]]] <- tmp
    },
    filearray = {
        filex[locs[[1]],locs[[2]],locs[[3]],locs[[4]]] <- tmp
    }, times = 10, setup = quote(gc())
)
speed4 <- sapply(split(res4, res4$expr), function(res){
    speed <- matrix(prod(sapply(locs, length)) / 1e8 * 800 / res$time, nrow = 10) * 1e9
    c(mean(speed), sd(speed) / 3)
})

z <- filex[]
res5 <- microbenchmark::microbenchmark(
    nativeR = {
        z[locs[[1]],locs[[2]],locs[[3]],locs[[4]]] <- tmp
    }, times = 10, setup = quote(gc())
)
rm(z); gc()
speed4 <- cbind(speed4, sapply(split(res5, res5$expr), function(res){
    speed <- matrix(prod(sapply(locs, length)) / 1e8 * 800 / res$time, nrow = 10) * 1e9
    c(mean(speed), sd(speed) / 3)
}))
mean(prod(sapply(locs, length)) / 1e8 * 800 / res5$time) * 1e9

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
    ) / 1024
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
