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
    Rf_setAttrib(re, R_ClassSymbol, Shield<SEXP>(wrap("integer64")));
    
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
    Rf_setAttrib(x, R_ClassSymbol, Shield<SEXP>(wrap("integer64")));
    
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
        if( Rf_getAttrib(x, Rf_install("_float_")) != R_NilValue ){
            return(x);
        }
    }
    R_xlen_t xlen = Rf_xlength(x);
    if( type == FLTSXP ){
        SEXP y = PROTECT(Rf_allocVector(INTSXP, xlen));
        Rf_setAttrib(y, Rf_install("_float_"), Shield<SEXP>(wrap(true)));
        
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

SEXP convert_as2(SEXP x, SEXP y, SEXPTYPE type) {
    SEXPTYPE xtype = TYPEOF(x);
    
    R_xlen_t xlen = Rf_xlength(x);
    R_xlen_t ylen = Rf_xlength(y);
    if( xlen > ylen ){
        xlen = ylen;
    }
    
    if( type == FLTSXP ){
        if( TYPEOF(y) != INTSXP ){ stop("`convert_as2` inconsistent y type"); }
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
        return(y);
    }
    
    if( type == CPLXSXP ){
        if( TYPEOF(y) != REALSXP ){ stop("`convert_as2` inconsistent y type"); }
        if( xtype != CPLXSXP ){
            SEXP z = PROTECT(Rf_coerceVector(x, CPLXSXP));
            cplxToReal(COMPLEX(z), REAL(y), xlen);
            UNPROTECT(1);
        } else {
            cplxToReal(COMPLEX(x), REAL(y), xlen);
        }
        return(y);
    }
    
    if( type == LGLSXP ){
        if( TYPEOF(y) != RAWSXP ){ stop("`convert_as2` inconsistent y type"); }
        if( xtype == RAWSXP ){
            memcpy(RAW(y), RAW(x), xlen);
            return(y);
        }
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
        return(y);
    }
    
    if( TYPEOF(y) != type ){
        stop("`convert_as2` inconsistent y type"); 
    }
    
    if( type == REALSXP ){
        if( xtype == type ){
            memcpy(REAL(y), REAL(x), sizeof(double) * xlen );
        } else {
            SEXP z = PROTECT(Rf_coerceVector(x, type));
            memcpy(REAL(y), REAL(z), sizeof(double) * xlen );
            UNPROTECT(1);
        }
    } else if( type == INTSXP ){
        if( xtype == type ){
            memcpy(INTEGER(y), INTEGER(x), sizeof(int) * xlen );
        } else {
            SEXP z = PROTECT(Rf_coerceVector(x, type));
            memcpy(INTEGER(y), INTEGER(z), sizeof(int) * xlen );
            UNPROTECT(1);
        }
    } else if( type == RAWSXP ){
        if( xtype == type ){
            memcpy(RAW(y), RAW(x), sizeof(Rbyte) * xlen );
        } else {
            SEXP z = PROTECT(Rf_coerceVector(x, type));
            memcpy(RAW(y), RAW(z), sizeof(Rbyte) * xlen );
            UNPROTECT(1);
        }
    }
    
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

void realToCplx(const double* x, Rcomplex* y, const size_t& nelem, const bool swap_endian){
    // double* xptr = x;
    Rcomplex* yptr = y;
    float* fptr = NULL;
    na_cplx_dbl();
    
    double tmp;
    double* tmpptr = &(tmp);
    unsigned char* tmpptr2 = (unsigned char*) tmpptr;
    size_t size = sizeof(double) - 1;
    size_t ix = 0;
    unsigned char* x2;
    
    for(size_t ii = 0; ii < nelem; ii++, yptr++){
        
        if( swap_endian ){
            x2 = (unsigned char*) (x + ii);
            for(ix = 0; ix <= size; ix++){
                *(tmpptr2 + (size - ix)) = *(x2 + ix);
            }
        } else {
            tmp = *(x + ii);
        }
        
        
        if(tmp == NA_COMPLEX_DBL){
            yptr->r = NA_REAL;
            yptr->i = NA_REAL;
        } else {
            fptr = (float*) (tmpptr);
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



/**********************************************************
 * Transform functions
 ***********************************************************/

void transform_float(const float* x, double* y, const bool& swap_endian){
    if( swap_endian ){
        size_t size = sizeof(float);
        float tmp;
        
        unsigned char *buffer_src = (unsigned char*) x;
        unsigned char *buffer_dst = (unsigned char*) (&tmp);
        
        for (size_t ix = 0; ix < size; ix++) {
            *(buffer_dst + (size - ix - 1)) = *(buffer_src + ix);
        }
        if(tmp == NA_FLOAT){
            *y = NA_REAL;
        } else {
            *y = tmp;
        }
        
    } else {
        if(*x == NA_FLOAT){
            *y = NA_REAL;
        } else {
            *y = *x;
        }
    }
}
void transform_logical(const Rbyte* x, int* y, const bool& swap_endian){
    if(*x == 0){
        *y = FALSE;
    } else if (*x == 1){
        *y = TRUE;
    } else {
        *y = NA_LOGICAL;
    }
}
void transform_complex(const double* x, Rcomplex* y, const bool& swap_endian){
    if( swap_endian ){
        size_t size = sizeof(double);
        double tmp;
        
        unsigned char *buffer_src = (unsigned char*) x;
        unsigned char *buffer_dst = (unsigned char*) (&tmp);
        
        for (size_t ix = 0; ix < size; ix++) {
            *(buffer_dst + (size - ix - 1)) = *(buffer_src + ix);
        }
        y->r = *((float*) buffer_dst);
        y->i = *(((float*) buffer_dst) + 1);
        
    } else {
        y->r = *((float*) x);
        y->i = *(((float*) x) + 1);
    }
    
    if( ISNAN(y->r) || ISNAN(y->i) ){
        y->r = NA_REAL;
        y->i = NA_REAL;
    }
}

void transforms_float(const float* x, double* y, const int& nelem, const bool& swap_endian){
    double* y2 = y;
    float tmp;
    float* tmpptr = &(tmp);
    unsigned char* tmpptr2 = (unsigned char*) tmpptr;
    size_t size = sizeof(float) - 1;
    size_t ix = 0;
    unsigned char* x2;
    
    for(int i = 0; i < nelem; i++, y2++){
        
        if( swap_endian ){
            x2 = (unsigned char*) (x + i);
            for(ix = 0; ix <= size; ix++){
                *(tmpptr2 + (size - ix)) = *(x2 + ix);
            }
            if(ISNAN(tmp)){
                *y2 = NA_REAL;
            } else {
                *y2 = tmp;
            }
        } else {
            if(ISNAN(*(x + i))){
                *y2 = NA_REAL;
            } else {
                *y2 = *(x + i);
            }
        }
    }
}
void transforms_logical(const Rbyte* x, int* y, const int& nelem, const bool& swap_endian){
    int* y2 = y;
    for(int i = 0; i < nelem; i++, y2++){
        *y2 = *(x + i);
        if( *y2 == 2 ){
            *y2 = NA_LOGICAL;
        }
    }
}
void transforms_complex(const double* x, Rcomplex* y, const int& nelem, const bool& swap_endian){
    realToCplx(x, y, nelem, swap_endian);
}
