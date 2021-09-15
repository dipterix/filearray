#ifndef FARR_LOAD_H
#define FARR_LOAD_H

#include <Rcpp.h>

SEXP FARR_subset2(
        const std::string& filebase,
        const SEXP listOrEnv,
        const SEXP reshape,
        const bool drop,
        const bool use_dimnames,
        const size_t thread_buffer,
        int split_dim,
        const int strict
);

SEXP FARR_subset_sequential(
        const std::string& filebase, 
        const int64_t& unit_partlen, 
        SEXP cum_partsizes, 
        SEXPTYPE array_type,
        SEXP file_buffer, 
        SEXP ret, 
        const int64_t from, 
        const int64_t len
);
    
#endif // FARR_LOAD_H