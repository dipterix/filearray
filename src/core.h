#ifndef FARR_CORE_H
#define FARR_CORE_H

#include <Rcpp.h>
#include "defs.h"

/**********************************************************
 * Buffer
 ***********************************************************/

int system_buffer_size();

int set_buffer_size(int size);

int get_buffer_size();

/**********************************************************
 * Load meta
 ***********************************************************/

std::string correct_filebase(const std::string& filebase);

Rcpp::List FARR_meta(const std::string& filebase);

/**********************************************************
 * Schedule reading
 ***********************************************************/
SEXP locationList(const SEXP listOrEnv, const Rcpp::NumericVector& dim, const int strict);

SEXP addCycle(SEXP x, SEXP ret, const R_xlen_t step = 1, const R_xlen_t mag = 1);

SEXP loc2idx(const Rcpp::List sliceIdx, const Rcpp::NumericVector& dim);

Rcpp::List schedule(const SEXP listOrEnv, 
                    const Rcpp::NumericVector& dim,
                    const Rcpp::NumericVector& cum_part_sizes,
                    const int split_dim, const int strict = 1);

/**********************************************************
 * Utils
 ***********************************************************/

SEXP seq_len_int64(const R_xlen_t len);
double prod_double(const Rcpp::NumericVector& x);

#endif // FARR_CORE_H