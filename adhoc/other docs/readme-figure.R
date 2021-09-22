set.seed(1)
dim <- c(100,200,200,100)
lazyx <- lazyarray::create_lazyarray(
    tempfile(), storage_format = 'double', dim = dim)
lazyx[] <- NA
filex <- filearray::filearray_create(
    tempfile(), dim, 'double')
filex$initialize_partition()
xs <- list(lazyx, filex)

lazyarray:::set_lazy_threads(8)
filearray::filearray_threads(8)

set.seed(1)
tmp <- rnorm(4e7)

speed1 <- rowMeans(replicate(1, {
    res <- sapply(xs, function(x){
        gc()
        system.time({
            for(i in 1:10){
                x[,,,(i-1)*10 +1:10] <- tmp
            }
        }, gcFirst = TRUE)
    })
    speed <- prod(dim) *8e-6 / res[3,]; speed
})); speed1

speed2 <- rowMeans(replicate(1, {
    res <- sapply(xs, function(x){
        gc()
        system.time({
            for(i in 1:10){
                x[,,,(i-1)*10 +1:10]
            }
        }, gcFirst = TRUE)
    })
    speed <- prod(dim) *8e-6 / res[3,]; speed
})); speed2

set.seed(1)
locs <- lapply(dim, function(d){
    sample(1:d, replace = FALSE, size = sample(50:d, 1))
})

speed3 <- rowMeans(replicate(10, {
    res <- sapply(xs, function(x){
        gc()
        system.time({
            x[locs[[1]],locs[[2]],locs[[3]],locs[[4]]]
        }, gcFirst = TRUE)
    })
    speed <- prod(sapply(locs, length)) *8e-6 / res[3,]; speed
})); speed3

tmp <- rnorm(prod(sapply(locs, length)))
speed4 <- rowMeans(replicate(10, {
    res <- sapply(xs, function(x){
        gc()
        system.time({
            x[locs[[1]],locs[[2]],locs[[3]],locs[[4]]] <- tmp
        }, gcFirst = TRUE)
    })
    speed <- prod(sapply(locs, length)) *8e-6 / res[3,]; speed
})); speed4


rm(tmp, sudo_pwd); gc()

f <- function(txt_cex = 1.2){
    
    cols <- c("orange", "dodgerblue3")
    speed <- cbind(
        speed1,
        speed4,
        speed2,
        speed3
    )
    rownames(speed) <-c("lazyarray", "filearray")
    colnames(speed) <- NULL
    
    txt_cex2 = txt_cex
    
    par(mfrow = c(1,2))
    
    plt <- barplot.default(
        speed[,1:2], beside = TRUE,
        ylab = "Speed (MB/s)", 
        col = dipsaus::col2hexStr(cols, alpha = 0.5),
        ylim = c(0, 1000), las = 1, yaxt = "n", 
        border = NA,
        main = "Single threaded\nmemory purged", 
        cex.names = 1.4, cex.lab = 1.4, cex.main = 1.4
    )
    axis(2, c(0, 350, 700), las = 1)
    axis(1, colMeans(plt), las = 1, label = c(
        "Write 10GB Data\n(Sequential)",
        "Substitute 800MB Data\n(Random)"
        # "Read 10GB Data\n(Sequential)",
        # "Subset 800MB Data\n(Random)"
    ), tick = FALSE)
    text.default(
        x = plt,
        y = speed[,1:2],
        labels = sprintf(c(
            "\n%.0f MB/s", 
            "\n%.0f MB/s"
        ), speed[,1:2]),
        cex = txt_cex,
        col = 'white'
    )
    text.default(
        x = plt,
        y = speed[,1:2],
        labels = sprintf("%s\n", rownames(speed)),
        cex = txt_cex2,
        col = cols
    )
    
    plt <- barplot.default(
        speed[,3:4], beside = TRUE,
        ylab = "Speed (GB/s)", 
        col = dipsaus::col2hexStr(cols, alpha = 0.5),
        ylim = c(0, 2500), las = 1, yaxt = "n", 
        border = NA,
        main = "Single threaded\nmemory purged", 
        cex.names = 1.4, cex.lab = 1.4, cex.main = 1.4
    )
    axis(2, c(0, 1000, 2000), las = 1, labels = c(0,1,2))
    axis(1, colMeans(plt), las = 1, label = c(
        # "Write 10GB Data\n(Sequential)",
        # "Substitute 800MB Data\n(Random)"
        "Read 10GB Data\n(Sequential)",
        "Subset 800MB Data\n(Random)"
    ), tick = FALSE)
    text.default(
        x = plt,
        y = speed[,3:4],
        labels = sprintf(c(
            "\n%.0f MB/s", 
            "\n%.0f MB/s"
        ), speed[,3:4]),
        cex = txt_cex,
        col = 'white'
    )
    text.default(
        x = plt,
        y = speed[,3:4],
        labels = sprintf("%s\n", rownames(speed)),
        cex = txt_cex2,
        col = cols
    )
    
    
}

# png("./adhoc/other docs/comparison-simple.png", width = 4267, height = 1600, res = 300)
f(0.7)
dev.off()
