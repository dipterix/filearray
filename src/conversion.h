#ifndef FARR_CONVERSION_H
#define FARR_CONVERSION_H

#include <Rcpp.h>
#include "defs.h"

double na_cplx_dbl();
SEXP realToInt64(Rcpp::NumericVector x, const double min_ = NA_REAL, const double max_ = NA_REAL, const int strict = 1);
SEXP realToInt64_inplace(SEXP x, const double min_ = NA_REAL, const double max_ = NA_REAL, const int strict = 1);
SEXP convert_as(SEXP x, SEXPTYPE type);

// void realToCplx(double* x, Rcomplex* y, size_t nelem);
void realToCplx(const double* x, Rcomplex* y, const size_t& nelem);

void cplxToReal(Rcomplex* x, double* y, size_t nelem);

void realToFloat(double* x, float* y, size_t nelem);

void floatToReal(float* x, double* y, size_t nelem);

    
#endif  // FARR_CONVERSION_H