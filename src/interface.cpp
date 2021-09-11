#include "common.h"
#include "core.h"
#include "load.h"
#include "save.h"
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

// // [[Rcpp::export]]


// // [[Rcpp::export]]
// SEXP FARR_subset_assign2(
//         const std::string& filebase,
//         SEXP value_,
//         const SEXP listOrEnv,
//         const size_t thread_buffer = 2097152,
//         int split_dim = 0
// ) {
//     
//     const std::string fbase = correct_filebase(filebase);
//     List meta = FARR_meta(fbase);
//     const int elem_size = meta["elem_size"];
//     const SEXPTYPE sexp_type = meta["sexp_type"];
//     SEXP dim = meta["dimension"]; // double
//     SEXP cum_part_size = meta["cumsum_part_sizes"];
//     
//     // calculate split_dim
//     R_len_t ndims = Rf_length(dim);
//     if( split_dim == NA_INTEGER || split_dim == 0 ){
//         split_dim = guess_splitdim(dim, elem_size, thread_buffer);
//     } else if (split_dim < 1 || split_dim > ndims-1 ){
//         stop("Incorrect `split_dim`: must be an integer from 1 to ndims-1 ");
//     }
//     set_buffer(dim, elem_size, thread_buffer, split_dim);
//     
//     FARR_subset_assign(fbase, listOrEnv, dim, cum_part_size,
//                        split_dim, sexp_type, value_);
//     
//     return(R_NilValue);
//     
// }

/*** R
set.seed(1)
require(filearray)
file <- tempfile(); unlink(file, recursive = TRUE)
x <- filearray_create(file, 3:5, type = "float")
dimnames(x) = list(1:3, 1:4, 1:5)

# FARR_subset2(
#     x$.filebase,
#     list(1:2, 3, 2), use_dimnames = T, drop = FALSE
# )

FARR_meta(x$.filebase)
*/
