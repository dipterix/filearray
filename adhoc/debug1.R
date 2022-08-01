devtools::load_all()
loadNamespace("bit64")
unlink('~/Desktop/junk/arr', recursive = TRUE)
arr <- filearray::filearray_create(
    filebase = '~/Desktop/junk/arr',
    dimension = c(861584, 233),
    type = "integer", partition_size = 1
)

parition_size <- 9001
niters <- 96
begin <- 216025
end <- 225025
by <- 9001
# arr[begin-1 + seq_len(9000),] <- rep(1:233, 9000)
arr[begin-1 + seq_len(9001),] <- rep(1:233, 9001)

0x102204000
0x1021fb360

filearray::filearray_threads(1)

arr[1,] <- 1:233

arr[seq_len(by),] <- rep(1:233,by)
arr[end,] <- rep(1:233,1)
i <- begin
arr[i-1 + 1:100,] <- rep(1:233,100)


# 0, skip=0
# idx start-end: 0 - 0
# [1] "/Users/dipterix/Desktop/junk/arr/0.farr"
# block_size: 861584, idx1len: 9001, idx1_start: 216024, idx2_start: 0, idx2_len: 1
# ### idx2ii:0, start_loc: 0, buf pos: 0, idx1_start: 216024

# *idx1ptr: 216024, idx1_start: 216024
# *idx1ptr: 225024, idx1_start: 216024
arr[begin-1 + seq_len(by),] <- rep(1:233,by)


for(i in seq(begin, 861584-by+1, by = by)) {
    print(i)
    try({ arr[i-1 + seq_len(by),] <- rep(1:233,by) })
}
225024 - 216024

file.size(arr$partition_path(1))
(3447360 - 1024) / 4
