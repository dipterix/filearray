#include "core.h"
#include "save.h"
#include "load.h"
using namespace Rcpp;

// [[Rcpp:interfaces(cpp)]]

// [[Rcpp::export]]
SEXP filearray_meta(
        const std::string& filebase
){
    List re = FARR_meta(filebase);
    return( re );
}

// [[Rcpp::export]]
SEXP filearray_assign(
        const std::string& filebase, SEXP value,
        const SEXP position_indices
) {
    size_t thread_buffer = get_buffer_size();
    FARR_subset_assign2(
        filebase, value, position_indices, 
        thread_buffer, 0);
    return(R_NilValue);
}

// [[Rcpp::export]]
SEXP filearray_subset(
        const std::string& filebase,
        const SEXP position_indices,
        const bool drop = true,
        const bool use_dimnames = true,
        const SEXP reshape = R_NilValue
) {
    size_t thread_buffer = get_buffer_size();
    SEXP ret = PROTECT(FARR_subset2(filebase, position_indices, reshape, drop,
                                    use_dimnames, thread_buffer, 0, 1));
    UNPROTECT(1);
    return( ret );
}