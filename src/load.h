#ifndef FARR_LOAD_H
#define FARR_LOAD_H

#include <Rcpp.h>

SEXP FARR_subset2(
        const std::string& filebase,
        const SEXP listOrEnv,
        const SEXP reshape = R_NilValue,
        const bool drop = false,
        const bool use_dimnames = true,
        const size_t thread_buffer = 2097152,
        int split_dim = 0,
        const int strict = 1
);
    
#endif // FARR_LOAD_H