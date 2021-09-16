#ifndef FARR_CONVERSION_H
#define FARR_CONVERSION_H

#include "common.h"
#include "defs.h"

double na_cplx_dbl();
SEXP realToInt64(Rcpp::NumericVector x, const double min_ = NA_REAL, const double max_ = NA_REAL, const int strict = 1);
SEXP realToInt64_inplace(SEXP x, const double min_ = NA_REAL, const double max_ = NA_REAL, const int strict = 1);
SEXP convert_as(SEXP x, SEXPTYPE type);
SEXP convert_as2(SEXP x, SEXP y, SEXPTYPE type);

// void realToCplx(double* x, Rcomplex* y, size_t nelem);
void realToCplx(const double* x, Rcomplex* y, const size_t& nelem);

void cplxToReal(Rcomplex* x, double* y, size_t nelem);

void realToFloat(double* x, float* y, size_t nelem);

void floatToReal(float* x, double* y, size_t nelem);

/**********************************************************
 * Transform functions
 ***********************************************************/

template <typename T>
inline void transform_asis(const T* x, T* y) {
    *y = *x;
}
void transform_float(const float* x, double* y);
void transform_logical(const Rbyte* x, int* y);
void transform_complex(const double* x, Rcomplex* y);


template <typename T>
inline void transforms_asis(const T* x, T* y, const int& nelem){
    memcpy(y, x, nelem * sizeof(T));
}

void transforms_float(const float* x, double* y, const int& nelem);
void transforms_logical(const Rbyte* x, int* y, const int& nelem);
void transforms_complex(const double* x, Rcomplex* y, const int& nelem);

    
#endif  // FARR_CONVERSION_H