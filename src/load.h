#ifndef FARR_LOAD_H
#define FARR_LOAD_H

#include <Rcpp.h>

SEXP FARR_subset(const std::string& filebase, 
                 const SEXPTYPE type,
                 const SEXP listOrEnv, 
                 const Rcpp::NumericVector& dim, 
                 const Rcpp::NumericVector& cum_part_sizes,
                 const int split_dim, 
                 const SEXP reshape = R_NilValue, 
                 const bool drop = false,
                 const int strict = 1,
                 const SEXP dimnames = R_NilValue);

#endif // FARR_LOAD_H