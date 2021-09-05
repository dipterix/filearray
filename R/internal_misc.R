# misc internal functions that are byte-compiled

# move here to byte-compile
.max_mapper1 <- function(data, size, idx){
    if(!length(size)){
        return(numeric(0))
    }
    max(data, na.rm = TRUE)
}
.max_mapper2 <- function(data, size, idx){
    if(!length(size)){
        return(numeric(0))
    }
    if(length(data) != size){
        data <- data[seq_len(size)]
    }
    max(data, na.rm = FALSE)
}
.min_mapper1 <- function(data, size, idx){
    if(!length(size)){
        return(numeric(0))
    }
    min(data, na.rm = TRUE)
}
.min_mapper2 <- function(data, size, idx){
    if(!length(size)){
        return(numeric(0))
    }
    if(length(data) != size){
        data <- data[seq_len(size)]
    }
    min(data, na.rm = FALSE)
}
.range_mapper1 <- function(data, size, idx){
    if(!length(size)){
        return(numeric(0))
    }
    range(data, na.rm = TRUE)
}
.range_mapper2 <- function(data, size, idx){
    if(!length(size)){
        return(numeric(0))
    }
    if(length(data) != size){
        data <- data[seq_len(size)]
    }
    range(data, na.rm = FALSE)
}

.sum_mapper1 <- function(data, size, idx){
    if(!length(size)){
        return(0)
    }
    sum(data, na.rm = TRUE)
}
.sum_mapper2 <- function(data, size, idx){
    if(!length(size)){
        return(0)
    }
    if(length(data) != size){
        data <- data[seq_len(size)]
    }
    sum(data, na.rm = FALSE)
}
