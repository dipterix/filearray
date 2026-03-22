# Map multiple file arrays and save results

Advanced mapping function for multiple file arrays. `fmap` runs the
mapping functions and stores the results in file arrays. `fmap2` stores
results in memory. This feature is experimental. There are several
constraints to the input. Failure to meet these constraints may result
in undefined results, or even crashes. Please read Section 'Details'
carefully before using this function.

## Usage

``` r
fmap(
  x,
  fun,
  .y = NULL,
  .buffer_count = NA_integer_,
  .output_size = NA_integer_,
  ...
)

fmap2(x, fun, .buffer_count = NA, .simplify = TRUE, ...)

fmap_element_wise(x, fun, .y, ..., .input_size = NA)
```

## Arguments

- x:

  a list of file arrays to map; each element of `x` must share the same
  dimensions.

- fun:

  function that takes one list

- .y:

  a file array object, used to save results

- .buffer_count:

  number of total buffers (chunks) to run

- .output_size:

  `fun` output vector length

- ...:

  other arguments passing to `fun`

- .simplify:

  whether to apply
  [`simplify2array`](https://rdrr.io/r/base/lapply.html) to the result

- .input_size:

  number of elements to read from each array of `x`

## Value

File array instance `.y`

## Details

Denote the first argument of `fun` as `input`, The length of `input`
equals the length of `x`. The size of each element of `input` is defined
by `.input_size`, except for the last loop. For example, given dimension
of each input array as \\10x10x10x10\\, if `.input_size=100`, then
`length(input[[1]])=100`. The total number of runs equals to
`length(x[[1]])/100`. If `.input_size=300`, then `length(input[[1]])`
will be `300` except for the last run. This is because \\10000\\ cannot
be divided by `300`. The element length of the last run will be `100`.

The returned variable length of `fun` will be checked by `.output_size`.
If the output length exceed `.output_size`, an error will be raised.

Please make sure that `length(.y)/length(x[[1]])` equals to
`.output_size/.input_size`.

For `fmap_element_wise`, the `input[[1]]` and output length must be the
consistent.

## Examples

``` r

set.seed(1)
x1 <- filearray_create(tempfile(), dimension = c(100,20,3))
x1[] <- rnorm(6000)
x2 <- filearray_create(tempfile(), dimension = c(100,20,3))
x2[] <- rnorm(6000)

# Add two arrays
output <- filearray_create(tempfile(), dimension = c(100,20,3))
fmap(list(x1, x2), function(input){
    input[[1]] + input[[2]]
}, output)
#> Reference class object of class "FileArray"
#> Mode: readwrite 
#> Dimension: 100x20x3 
#> Partition count: 3 
#> Partition size: 1 
#> Storage type: double (internal size: 8)
#> Location: /tmp/Rtmp2k7wVo/file1cc989f59c0 

# check
range(output[] - (x1[] + x2[]))
#> [1] 0 0

output$delete()

# Calculate the maximum of x1/x2 for every 100 elements
# total 60 batches/loops (`.buffer_count`)
output <- filearray_create(tempfile(), dimension = c(20,3))
fmap(list(x1, x2), function(input){
    max(input[[1]] / input[[2]])
}, .y = output, .buffer_count = 60)
#> Reference class object of class "FileArray"
#> Mode: readwrite 
#> Dimension: 20x3 
#> Partition count: 3 
#> Partition size: 1 
#> Storage type: double (internal size: 8)
#> Location: /tmp/Rtmp2k7wVo/file1cc97fed0f9e 

# check
range(output[] - apply(x1[] / x2[], c(2,3), max))
#> [1] 0 0

output$delete()

# A large array example
if(interactive()){
    x <- filearray_create(tempfile(), dimension = c(287, 100, 301, 4))
    dimnames(x) <- list(
        Trial = 1:287,
        Marker = 1:100,
        Time = 1:301,
        Location = 1:4
    )

    for(i in 1:4){
        x[,,,i] <- runif(8638700)
    }
    # Step 1:
    # for each location, trial, and marker, calibrate (baseline)
    # according to first 50 time-points

    output <- filearray_create(tempfile(), dimension = dim(x))

    # baseline-percentage change
    fmap(
        list(x),
        function(input){
            # get locational data
            location_data <- input[[1]]
            dim(location_data) <- c(287, 100, 301)

            # collapse over first 50 time points for
            # each trial, and marker
            baseline <- apply(location_data[,,1:50], c(1,2), mean)

            # calibrate
            calibrated <- sweep(location_data, c(1,2), baseline,
                                FUN = function(data, bl){
                                    (data / bl - 1) * 100
                                })
            return(calibrated)
        },

        .y = output,

        # input dimension is 287 x 100 x 301 for each location
        # hence 4 loops in total
        .buffer_count = 4
    )

    # cleanup
    x$delete()

}

# cleanup
x1$delete()
x2$delete()
output$delete()
```
