sudo_pwd <- rstudioapi::askForPassword("sudo password")
cold_start <- TRUE

set.seed(1)
dim <- c(100,200,200,100)
filex <- filearray::filearray_create(
    tempfile(), dim, 'double')
filex$initialize_partition()

set.seed(1)
tmp <- rnorm(1e8)

threads <- c(1,2,4,8)

# run in order to make sure we have cold start
speed1 <- rowMeans(replicate(5, {
    res <- sapply(threads, function(thread){
        filearray::filearray_threads(thread)
        gc()
        if(cold_start){system("sudo -kS purge", input = sudo_pwd)}
        Sys.sleep(1)
        system.time({
            filex[,,,1:25 + (log2(thread) * 25)] <- tmp
        })
    })
    speed <- length(filex) *8e-6 / 4 / res[3,]; speed
})); speed1

rm(tmp); gc()


speed2 <- rowMeans(replicate(5, {
    res <- sapply(threads, function(thread){
        filearray::filearray_threads(thread)
        gc()
        if(cold_start){system("sudo -kS purge", input = sudo_pwd)}
        Sys.sleep(1)
        system.time({
            filex[,,,1:25 + (log2(thread) * 25)]
        })
    })
    speed <- length(filex) *8e-6 / 4 / res[3,]; speed
})); speed2


# 800 MB data indices
set.seed(1)
locs <- lapply(dim, function(d){
    sample(1:d, replace = FALSE, size = 100)
})

# CMD: sudo purge
speed3 <- rowMeans(replicate(5, {
    res <- sapply(threads, function(thread){
        filearray::filearray_threads(thread)
        gc()
        if(cold_start){system("sudo -kS purge", input = sudo_pwd)}
        Sys.sleep(1)
        system.time({
            filex[locs[[1]],locs[[2]],locs[[3]],locs[[4]]]
        }, gcFirst = TRUE)
    })
    speed <- 800 / res[3,]; speed
})); speed3


tmp <- rnorm(prod(sapply(locs, length)))
speed4 <- rowMeans(replicate(5, {
    res <- sapply(threads, function(thread){
        filearray::filearray_threads(thread)
        stopifnot(filearray::filearray_threads() == thread)
        gc()
        if(cold_start){system("sudo -kS purge", input = sudo_pwd)}
        Sys.sleep(1)
        system.time({
            filex[locs[[1]],locs[[2]],locs[[3]],locs[[4]]] <- tmp
        }, gcFirst = TRUE)
    })
    speed <- 800 / res[3,]; speed
})); speed4

rm(tmp, sudo_pwd); gc()
# save.image("./adhoc/other docs/comparison-threads.RData")

f <- function(txt_cex = 0.7){
    
    par(mfrow = c(1,2))
    
    cols <- c("orangered", "orange", "darkgreen", "dodgerblue3")
    speed <- cbind(
        speed1,
        speed4,
        speed2,
        speed3
    ) / 1024
    rownames(speed) <-c("1-thread", paste0(c(2,4,8), "-threads"))
    colnames(speed) <- NULL
    
    
    txt_cex2 = txt_cex
    
    idx <- 1:2
    plt <- barplot.default(
        speed[,idx], beside = TRUE,
        ylab = "Speed (GB/s)", 
        col = dipsaus::col2hexStr(cols, alpha = 0.5),
        ylim = c(0, 1), las = 1, yaxt = "n", 
        border = NA,
        main = "Speed Comparisons (Different Threads, memory purged)", 
        cex.names = 1.4, cex.lab = 1.4, cex.main = 1.4
    )
    axis(2, c(0,0.5,1), las = 1)
    axis(1, colMeans(plt), las = 1, label = c(
        "Write 800MB Data\n(Sequential)",
        "Substitute 800MB Data\n(Random)"
        # "Read 800MB Data\n(Sequential)",
        # "Subset 800MB Data\n(Random)"
    ), tick = FALSE)
    text.default(
        x = plt,
        y = speed[,idx],
        labels = sprintf(c(
            "\n%.0f MB/s", 
            "\n%.0f MB/s"
        ), speed[,idx] * 1024),
        cex = txt_cex,
        col = 'white'
    )
    text.default(
        x = plt,
        y = speed[,idx],
        labels = sprintf("%s\n", rownames(speed)),
        cex = txt_cex2,
        col = cols
    )
    
    
    idx <- 3:4
    plt <- barplot.default(
        speed[,idx], beside = TRUE,
        ylab = "Speed (GB/s)", 
        col = dipsaus::col2hexStr(cols, alpha = 0.5),
        ylim = c(0, 3.5), las = 1, yaxt = "n", 
        border = NA,
        main = "Speed Comparisons (Different Threads, memory purged)", 
        cex.names = 1.4, cex.lab = 1.4, cex.main = 1.4
    )
    axis(2, c(0:3), las = 1)
    axis(1, colMeans(plt), las = 1, label = c(
        # "Write 800MB Data\n(Sequential)",
        # "Substitute 800MB Data\n(Random)"
        "Read 800MB Data\n(Sequential)",
        "Subset 800MB Data\n(Random)"
    ), tick = FALSE)
    text.default(
        x = plt,
        y = speed[,idx],
        labels = sprintf(c(
            "\n%.0f MB/s", 
            "\n%.0f MB/s"
        ), speed[,idx] * 1024),
        cex = txt_cex,
        col = 'white'
    )
    text.default(
        x = plt,
        y = speed[,idx],
        labels = sprintf("%s\n", rownames(speed)),
        cex = txt_cex2,
        col = cols
    )
    
    
}

png("./adhoc/other docs/comparison-threads.png", width = 4267, height = 1600, res = 300)
f()
dev.off()
