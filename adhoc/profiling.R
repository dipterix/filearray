# performance test

filearray::filearray_threads()
# 2.56GB file

set.seed(1)
file <- tempfile()
unlink(file, recursive = TRUE)

x <- filearray::filearray_create(file, c(128, 128, 128, 128))

# One-time initialize partition files with NA
system.time({
    x$initialize_partition()
})
#   user  system elapsed 
#  0.236   2.476   2.864

# -------------------- WRITE - single threaded ------------------------
# write to fast margin - single threaded
filearray::filearray_threads(1)
tmp <- rnorm(128^3)
system.time({
    for(i in 1:128){
        x[,,,i] <- tmp
    }
})
#   user  system elapsed 
#  0.220   1.097   2.089

# 1.2 GB/s


system.time({
    for(i in 1:128){
        x[,,i,] <- tmp
    }
})
#   user  system elapsed 
#  1.044   1.400   4.795

# 500 MB/s

# write to slow margin - Never recommended to write on single 
# (see next section - block write)
system.time({
    for(i in 1:128){
        x[i,,,] <- tmp
    }
})
#   user  system elapsed 
#  0.057   1.048   2.097

# Using memory map boosts the speed
# 1.2 GB/s

# -------------------- WRITE (block) - 4-threaded ------------------------
# write to fast margin - 4 threaded
filearray::filearray_threads(4)

# 1GB data
tmp <- rnorm(128^3 * 50)
idx <- sample(1:128, 50)
system.time({
    x[,,,idx] <- tmp
})
#   user  system elapsed 
#  0.135   0.642   0.730

# 1.4 GB/s


system.time({
    x[,,idx,] <- tmp
})
#   user  system elapsed 
#  0.288   0.707   1.165

# 1 GB/s

system.time({
    x[,idx,,] <- tmp
})
#   user  system elapsed 
#  0.166   1.379   2.082

# 500 MB/s

system.time({
    x[idx,,,] <- tmp
})
#   user  system elapsed 
#  0.152   1.555   1.995

# 500 MB/s

rm(tmp); gc()
# -------------------- READ - single-threaded ------------------------
### single-threaded
filearray::filearray_threads(1)

# read 1GB subset

#### ordered indices ####
idx <- sort(sample(1:128, 50))

# partition margin 
system.time({
    x[,,,idx]
})
#   user  system elapsed 
#  0.135   0.267   0.620 

# 1.6 GB/s

# fast margin
system.time({
    x[,,idx,]
})
#   user  system elapsed 
#  0.135   0.414   0.955

# 1 GB/s

# slow margin
system.time({
    x[idx,,,]
})
#   user  system elapsed 
#  0.141   0.376   0.950 

# 1 GB/s


#### random indices ####
idx <- sample(1:128, 50)

# partition margin 
system.time({
    x[,,,idx]
})
#   user  system elapsed 
#  0.127   0.219   0.507 

# 2 GB/s

# fast margin
system.time({
    x[,,idx,]
})
#   user  system elapsed 
#  0.133   0.355   0.796

# 1.4 GB/s

# slow margin
system.time({
    x[idx,,,]
})
#   user  system elapsed 
#  0.141   0.347   0.953

# 1 GB/s



# -------------------- READ - 4-threaded ------------------------
filearray::filearray_threads(8)
idx <- sample(1:128, 50)

# partition margin 
system.time({
    x[,,,idx]
})
#   user  system elapsed 
#  0.175   0.264   0.289

# 3.4 GB/s

# fast margin
system.time({
    x[,,idx,]
})
#   user  system elapsed 
#  0.378   0.324   0.248 

# 4 GB/s

# slow margin
system.time({
    x[idx,,,]
})
#   user  system elapsed 
#  0.174   0.448   0.681

# 1.5 GB/s


system.time({
    x[1:65,,,]
})
system.time({
    x[c(1, 67:128),,,]
})
