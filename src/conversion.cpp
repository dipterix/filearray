#include "conversion.h"
using namespace Rcpp;

static double NA_COMPLEX_DBL = 0;

double na_cplx_dbl(){
    if( NA_COMPLEX_DBL == 0 ){
        Rcomplex na_cplx; na_cplx.i = NA_REAL; na_cplx.r = NA_REAL;
        cplxToReal(&(na_cplx), &(NA_COMPLEX_DBL), 1);
    }
    return(NA_COMPLEX_DBL);
}


// [[Rcpp::export]]
SEXP realToInt64(NumericVector x, const double min_, const double max_, const int strict){
    R_xlen_t len = x.length();
    SEXP re = PROTECT(Rf_allocVector(REALSXP, len));
    Rf_setAttrib(re, R_ClassSymbol, wrap("integer64"));
    
    int64_t *reptr = (int64_t *) REAL(re);
    
    NumericVector::iterator xptr = x.begin();
    
    for(; xptr != x.end(); xptr++, reptr++ ){
        if( ISNAN(*xptr) ){
            *reptr = NA_INTEGER64;
            continue;
        }
        if( (min_ != NA_REAL && *xptr < min_) || (max_ != NA_REAL && *xptr > max_) ){
            if( strict ){
                stop("Index out of margin bound");
            }
            *reptr = NA_INTEGER64;
            continue;
        }
        *reptr = (int64_t) *xptr;
    }
    
    UNPROTECT(1);
    return re;
}

SEXP realToInt64_inplace(SEXP x, const double min_, const double max_, const int strict){
    R_xlen_t len = Rf_xlength(x);
    Rf_setAttrib(x, R_ClassSymbol, wrap("integer64"));
    
    int64_t *reptr = (int64_t *) REAL(x);
    double* xptr = REAL(x);
    
    for(R_xlen_t ii = 0; ii < len; ii++, xptr++, reptr++ ){
        if( ISNAN(*xptr) ){
            *reptr = NA_INTEGER64;
            continue;
        }
        if( (min_ != NA_REAL && *xptr < min_) || (max_ != NA_REAL && *xptr > max_) ){
            if( strict ){
                stop("Index out of margin bound");
            }
            *reptr = NA_INTEGER64;
            continue;
        }
        *reptr = (int64_t) *xptr;
    }
    
    return x;
}

SEXP convert_as(SEXP x, SEXPTYPE type) {
    SEXPTYPE xtype = TYPEOF(x);
    
    if( type == FLTSXP && xtype == INTSXP ){
        if( Rf_getAttrib(x, wrap("_float_")) != R_NilValue ){
            return(x);
        }
    }
    R_xlen_t xlen = Rf_xlength(x);
    if( type == FLTSXP ){
        SEXP y = PROTECT(Rf_allocVector(INTSXP, xlen));
        Rf_setAttrib(y, Shield<SEXP>(wrap("_float_")), Shield<SEXP>(wrap(true)));
        
        switch(xtype) {
        case RAWSXP: {
            Rbyte* xptr = RAW(x);
            float* yptr = FLOAT(y);
            for(R_xlen_t ii = 0; ii < xlen; ii++, xptr++, yptr++){
                *yptr = (float) *xptr;
            }
            break;
        }
        case INTSXP: {
            int* xptr = INTEGER(x);
            float* yptr = FLOAT(y);
            for(R_xlen_t ii = 0; ii < xlen; ii++, xptr++, yptr++){
                if(*xptr == NA_INTEGER){
                    *yptr = NA_FLOAT;
                } else {
                    *yptr = *xptr;
                }
            }
            break;
        }
        case LGLSXP: {
            int* xptr = LOGICAL(x);
            float* yptr = FLOAT(y);
            for(R_xlen_t ii = 0; ii < xlen; ii++, xptr++, yptr++){
                if(*xptr == NA_LOGICAL){
                    *yptr = NA_FLOAT;
                } else {
                    *yptr = *xptr;
                }
            }
            break;
        }
        case REALSXP: {
            realToFloat(REAL(x), FLOAT(y), xlen);
            break;
        }
        default: {
            SEXP z = PROTECT(Rf_coerceVector(x, REALSXP));
            realToFloat(REAL(z), FLOAT(y), xlen);
            UNPROTECT(1);
        }
        }
        UNPROTECT(1);
        return(y);
        
    }
    
    if( type == CPLXSXP ){
        SEXP y = PROTECT(Rf_allocVector(REALSXP, xlen));
        if( xtype != CPLXSXP ){
            SEXP z = PROTECT(Rf_coerceVector(x, CPLXSXP));
            cplxToReal(COMPLEX(z), REAL(y), xlen);
            UNPROTECT(1);
        } else {
            cplxToReal(COMPLEX(x), REAL(y), xlen);
        }
        UNPROTECT(1);
        return(y);
    }
    
    if( type == LGLSXP ){
        if( xtype == RAWSXP ){
            return(x);
        }
        SEXP y = PROTECT(Rf_allocVector(RAWSXP, xlen));
        if( xtype != LGLSXP ){
            SEXP z = PROTECT(Rf_coerceVector(x, LGLSXP));
            int* xptr = LOGICAL(z);
            Rbyte* yptr = RAW(y);
            for(R_xlen_t ii = 0; ii < xlen; ii++, xptr++, yptr++){
                if(*xptr == NA_LOGICAL){
                    *yptr = 2;
                } else if (*xptr == TRUE){
                    *yptr = 1;
                } else {
                    *yptr = 0;
                }
            }
            UNPROTECT(1);
        } else {
            int* xptr = LOGICAL(x);
            Rbyte* yptr = RAW(y);
            for(R_xlen_t ii = 0; ii < xlen; ii++, xptr++, yptr++){
                if(*xptr == NA_LOGICAL){
                    *yptr = 2;
                } else if (*xptr == TRUE){
                    *yptr = 1;
                } else {
                    *yptr = 0;
                }
            }
        }
        UNPROTECT(1);
        return(y);
    }
    
    if( xtype == type ){
        return(x);
    }
    
    SEXP y = PROTECT(Rf_coerceVector(x, type));
    UNPROTECT(1);
    return(y);
}



void cplxToReal(Rcomplex* x, double* y, size_t nelem){
    double* yptr = y;
    Rcomplex* xptr = x;
    float* fptr = NULL;
    for(size_t ii = 0; ii < nelem; ii++, xptr++, yptr++){
        fptr = (float*) yptr;
        *fptr++ = (float) (xptr->r);
        *fptr = (float) (xptr->i);
    }
}

void realToCplx(const double* x, Rcomplex* y, const size_t& nelem){
    // double* xptr = x;
    Rcomplex* yptr = y;
    float* fptr = NULL;
    na_cplx_dbl();
    for(size_t ii = 0; ii < nelem; ii++, yptr++){
        if(*(x + ii) == NA_COMPLEX_DBL){
            yptr->r = NA_REAL;
            yptr->i = NA_REAL;
        } else {
            fptr = (float*) (x + ii);
            yptr->r = *fptr++;
            yptr->i = *fptr;
        }
    }
}

// [[Rcpp::export]]
SEXP cplxToReal2(SEXP x){
    if(TYPEOF(x) != CPLXSXP){
        stop("Complex input required.");
    }
    
    R_xlen_t xlen = Rf_xlength(x);
    SEXP y = PROTECT(Rf_allocVector(REALSXP, xlen));
    
    cplxToReal(COMPLEX(x), REAL(y), xlen);
    
    UNPROTECT(1);
    return(y);
}

// [[Rcpp::export]]
SEXP realToCplx2(SEXP x){
    if(TYPEOF(x) != REALSXP){
        stop("Double input required.");
    }
    
    R_xlen_t xlen = Rf_xlength(x);
    SEXP y = PROTECT(Rf_allocVector(CPLXSXP, xlen));
    
    realToCplx(REAL(x), COMPLEX(y), xlen);
    
    UNPROTECT(1);
    return(y);
}


void realToFloat(double* x, float* y, size_t nelem){
    for(R_xlen_t ii = 0; ii < nelem; ii++, x++, y++){
        if(*x == NA_REAL){
            *y = NA_FLOAT;
        } else {
            *y = (float) *x;
        }
    }
}

void floatToReal(float* x, double* y, size_t nelem){
    for(R_xlen_t ii = 0; ii < nelem; ii++, y++, x++){
        if(ISNAN(*x)){
            *y = NA_REAL;
        } else {
            *y = *x;
        }
    }
}

// [[Rcpp::export]]
SEXP realToFloat2(SEXP x){
    if(TYPEOF(x) != REALSXP){
        stop("Double input required.");
    }
    R_xlen_t xlen = Rf_xlength(x);
    SEXP y = PROTECT(Rf_allocVector(INTSXP, xlen));
    realToFloat(REAL(x), FLOAT(y), xlen);
    UNPROTECT(1);
    return(y);
}

// [[Rcpp::export]]
SEXP floatToReal2(SEXP x){
    if(TYPEOF(x) != INTSXP){
        stop("Float input required.");
    }
    R_xlen_t xlen = Rf_xlength(x);
    SEXP y = PROTECT(Rf_allocVector(REALSXP, xlen));
    floatToReal(FLOAT(x), REAL(y), xlen);
    UNPROTECT(1);
    return(y);
}


// [[Rcpp::export]]
SEXP get_float_na(){
    SEXP re = PROTECT(Rf_allocVector(INTSXP, 1));
    float* ptr = FLOAT(re);
    *ptr = NA_FLOAT;
    UNPROTECT(1);
    return(re);
}
