#ifndef FARR_UTILS_H
#define FARR_UTILS_H

#include <Rcpp.h>
#include "defs.h"
#include "core.h"

int guess_splitdim(SEXP dim, int elem_size, size_t buffer_bytes);

void set_buffer(SEXP dim, int elem_size, size_t buffer_bytes, int split_dim);

SEXPTYPE file_buffer_sxptype(SEXPTYPE array_type);

int kinda_sorted(SEXP idx, int64_t min_, int64_t buffer_count);

SEXP check_missing_dots(const SEXP env);

SEXP sub_vec(SEXP x, SEXP idx_int64);
SEXP sub_vec_range(SEXP x, const R_xlen_t& min_, const R_xlen_t& len_);

SEXP reshape_or_drop(SEXP x, SEXP reshape, bool drop);

SEXP subset_dimnames(SEXP dimnames, SEXP sliceIdx);


#endif  // FARR_UTILS_H