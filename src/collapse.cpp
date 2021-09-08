#include "common.h"
using namespace Rcpp;

template <typename T>
void collapse(
        FILE* conn, const SEXP& dim, SEXP keep_dim, T* bufptr, 
        int buf_size, double* ret, T na, SEXP loc, int method, 
        bool remove_na){
    int elem_size = sizeof(T);
    int ndims = Rf_length(dim);
    
    // dim are int64_t, keep_dim are integers
    int64_t* dimptr = (int64_t*) REAL(dim);
    int64_t partlen = 1;
    for(int i = 0; i < ndims; i++){
        partlen *= *(dimptr + i);
    }
    
    // calculate loc in original array
    // SEXP loc = PROTECT(Rf_allocVector(REALSXP, ndims));
    int64_t* locptr = (int64_t*) REAL(loc);
    
    int keeplen = Rf_length(keep_dim);
    int* ptrkeep = INTEGER(keep_dim);
    
    int64_t buflen = buf_size / elem_size;
    
    if(conn != NULL){
        fseek(conn, FARR_HEADER_LENGTH, SEEK_SET);
    } else {
        T* bufptr_ii = bufptr;
        for(int64_t kk = 0; kk < buflen; kk++){
            *bufptr_ii++ = na;
        }
    }
    
    int64_t niter = partlen % buflen;
    if( niter == 0 ){
        niter = partlen / buflen;
    } else {
        niter = (partlen - niter) / buflen + 1;
    }
    
    int64_t buf_idx = 0;
    int64_t readlen = 0;
    int ii = 0;
    int64_t rem = 0, fct = 0, tmp = 0;
    int64_t* locptr_ii = locptr;
    int64_t* dimptr_ii = dimptr;
    int* ptrkeep_ii = ptrkeep;
    T v = 0;
    
    for(int64_t iter = 0; iter < niter; iter++){
        buf_idx = iter * buflen;
        readlen = buflen;
        if( iter == niter - 1 ){
            readlen = partlen - buf_idx;
        }
        if(conn != NULL){
            lendian_fread(bufptr, elem_size, readlen, conn);
        }
        
        for(int64_t jj = 0; jj < readlen; jj++){
            v = *(bufptr + jj);
            if( v == na && remove_na ){
                continue;
            }
            rem = jj + buf_idx; 
            locptr_ii = locptr; 
            dimptr_ii = dimptr;
            for(ii = 0; ii < ndims; ii++, dimptr_ii++, locptr_ii++){
                *locptr_ii = rem % (*dimptr_ii);
                rem = (rem - *locptr_ii) / *dimptr_ii;
            }
            // if( rem > 0 ){ continue; }
            rem = 0; fct = 1; ptrkeep_ii = ptrkeep;
            for(ii = 0; ii < keeplen; ii++, ptrkeep_ii++){
                tmp = *ptrkeep_ii - 1;
                rem += fct * (*(locptr + tmp));
                fct *= *(dimptr + tmp);
            }
            
            if( v == na ){ // remove_na must be false
                *(ret + rem) = NA_REAL;
                continue;
            }
            // rem is index of ret
            // need to be atom
            switch(method){
            case 1: 
                *(ret + rem) += v;
                break;
            case 2:
                *(ret + rem) += std::log10((double) v) * 10.0;
                break;
            case 3:
                *(ret + rem) += ((double) v) * ((double) v);
                break;
            case 4:
                *(ret + rem) += std::sqrt((double) v);
                break;
            }
        }
    }
    
}


// [[Rcpp::export]]
SEXP FARR_collapse(
        const std::string& filebase, 
        const NumericVector& dim,
        const IntegerVector& keep, 
        const NumericVector& cum_part,
        SEXPTYPE array_type,
        int method = 1,
        bool remove_na = false,
        double scale = 1.0
){
    int ndims = dim.length();
    SEXP dim_int64 = PROTECT(realToUint64(dim, 0, 1200000000000000000, 1));
    
    // 1. check if keep has lastdim
    // 2. calculate ret size
    R_xlen_t retlen = 1;
    R_xlen_t retlen_ii = 1;
    int tmp;
    bool keep_lastdim = false;
    for(R_xlen_t i = 0; i < keep.length(); i++){
        tmp = keep[i];
        if( tmp == ndims ){
            keep_lastdim = true;
        }
        if( i+1 == keep.length() ){
            retlen_ii = retlen;
        }
        retlen *= dim[tmp - 1];
    }
    SEXP ret = PROTECT(Rf_allocVector(REALSXP, retlen));
    Rf_setAttrib(ret, R_DimSymbol, dim[keep - 1]);
    
    
    int64_t* last_dimptr = ((int64_t*) REAL(dim_int64)) + (ndims - 1);
    
    SEXP cum_part64 = PROTECT(realToUint64(cum_part, 0, 1200000000000000000, 1));
    int64_t* cum_part64ptr = (int64_t*) REAL(cum_part64);
    R_xlen_t nparts = Rf_xlength(cum_part64);
    
    int64_t part_size = 0, last_size = 0;
    std::string partition_path = "";
    FILE* conn = NULL;
    double* retptr = REAL(ret);
    for(R_xlen_t i = 0; i < retlen; i++){
        *retptr++ = 0;
    }
    
    int buf_size = get_buffer_size();
    SEXP buffer = R_NilValue;
    switch(array_type){
    case REALSXP:
        buffer = PROTECT(Rf_allocVector(REALSXP, buf_size / 8));
        break;
    case INTSXP:
        buffer = PROTECT(Rf_allocVector(INTSXP, buf_size / 4));
        break;
    case LGLSXP:
    case RAWSXP:
        buffer = PROTECT(Rf_allocVector(RAWSXP, buf_size));
        break;
    default:
        UNPROTECT(3);
    stop("Unsupported array type.");
    }
    
    
    SEXP loc = PROTECT(Rf_allocVector(REALSXP, ndims));
    Rf_setAttrib(loc, R_ClassSymbol, wrap("integer64"));
    
    for(R_xlen_t part = 0; part < nparts; part++){
        part_size = *(cum_part64ptr + part) - last_size;
        if( keep_lastdim ){
            retptr = REAL(ret) + last_size * retlen_ii;
        } else {
            retptr = REAL(ret);
        }
        last_size += part_size;
        
        *last_dimptr = part_size;
        partition_path = filebase + std::to_string(part) + ".farr";
        
        conn = fopen(partition_path.c_str(), "rb"); 
        
        try{
            // collapse_double(conn, dim_int64, keep, retptr);
            switch(array_type){
            case REALSXP:
                collapse(conn, dim_int64, keep, 
                         REAL(buffer), buf_size,
                         retptr, NA_REAL, loc, method, 
                         remove_na);
                break;
            case INTSXP:
                collapse(conn, dim_int64, keep, 
                         INTEGER(buffer), buf_size,
                         retptr, NA_INTEGER, loc, method, 
                         remove_na);
                break;
            case LGLSXP: {
                Rbyte na_lgl = 2;
                collapse(conn, dim_int64, keep, 
                         RAW(buffer), buf_size,
                         retptr, na_lgl, loc, method, 
                         remove_na);
                break;
            }
            case RAWSXP: {
                Rbyte na_lgl = 0;
                collapse(conn, dim_int64, keep, 
                         RAW(buffer), buf_size,
                         retptr, na_lgl, loc, method, 
                         true);
                break;
            }
            }
        }catch(...){}
        
        if(conn != NULL){
            fclose(conn);
            conn = NULL;
        }
    }
    
    retptr = REAL(ret);
    for(R_xlen_t i = 0; i < retlen; i++){
        *retptr++ *= scale;
    }
    
    
    UNPROTECT(5);
    return(ret);
}

void collapse_complex(
        FILE* conn, const SEXP& dim, SEXP keep_dim, 
        double* bufreal_ptr, Rcomplex* bufcplx_ptr,
        int buf_size, Rcomplex* ret, SEXP loc, int method, 
        bool remove_na){
    int elem_size = sizeof(double);
    int ndims = Rf_length(dim);
    
    // dim are int64_t, keep_dim are integers
    int64_t* dimptr = (int64_t*) REAL(dim);
    int64_t partlen = 1;
    for(int i = 0; i < ndims; i++){
        partlen *= *(dimptr + i);
    }
    
    // calculate loc in original array
    // SEXP loc = PROTECT(Rf_allocVector(REALSXP, ndims));
    int64_t* locptr = (int64_t*) REAL(loc);
    
    int keeplen = Rf_length(keep_dim);
    int* ptrkeep = INTEGER(keep_dim);
    
    int64_t buflen = buf_size / elem_size;
    Rcomplex na_cplx; na_cplx.i = NA_REAL; na_cplx.r = NA_REAL;
    
    
    if(conn != NULL){
        fseek(conn, FARR_HEADER_LENGTH, SEEK_SET);
    } else {
        Rcomplex* bufcplx_ptr_ii = bufcplx_ptr;
        for(int64_t kk = 0; kk < buflen; kk++){
            *bufcplx_ptr_ii++ = na_cplx;
        }
    }
    
    int64_t niter = partlen % buflen;
    if( niter == 0 ){
        niter = partlen / buflen;
    } else {
        niter = (partlen - niter) / buflen + 1;
    }
    
    int64_t buf_idx = 0;
    int64_t readlen = 0;
    int ii = 0;
    int64_t rem = 0, fct = 0, tmp = 0;
    int64_t* locptr_ii = locptr;
    int64_t* dimptr_ii = dimptr;
    int* ptrkeep_ii = ptrkeep;
    Rcomplex v = na_cplx;
    
    Rcomplex* ret_ii = ret;
    double tmp_v = 0;
    
    for(int64_t iter = 0; iter < niter; iter++){
        buf_idx = iter * buflen;
        readlen = buflen;
        if( iter == niter - 1 ){
            readlen = partlen - buf_idx;
        }
        if(conn != NULL){
            lendian_fread(bufreal_ptr, elem_size, readlen, conn);
            realToCplx(bufreal_ptr, bufcplx_ptr, readlen);
        }
        
        for(int64_t jj = 0; jj < readlen; jj++){
            v = *(bufcplx_ptr + jj);
            if( remove_na && (v.i == NA_REAL || v.r == NA_REAL) ){
                continue;
            }
            rem = jj + buf_idx; 
            locptr_ii = locptr; 
            dimptr_ii = dimptr;
            for(ii = 0; ii < ndims; ii++, dimptr_ii++, locptr_ii++){
                *locptr_ii = rem % (*dimptr_ii);
                rem = (rem - *locptr_ii) / *dimptr_ii;
            }
            // if( rem > 0 ){ continue; }
            rem = 0; fct = 1; ptrkeep_ii = ptrkeep;
            for(ii = 0; ii < keeplen; ii++, ptrkeep_ii++){
                tmp = *ptrkeep_ii - 1;
                rem += fct * (*(locptr + tmp));
                fct *= *(dimptr + tmp);
            }
            ret_ii = ret + rem;
            if( v.i == NA_REAL || v.r == NA_REAL ){ // remove_na must be false
                *ret_ii = na_cplx;
                continue;
            }
            // rem is index of ret
            // need to be atom
            switch(method){
            case 1: 
                ret_ii->i += v.i;
                ret_ii->r += v.r;
                break;
            case 2:
                ret_ii->r += std::log10(v.i * v.i + v.r * v.r) * 10.0;
                ret_ii->i += 1.0;
                break;
            case 3:
                ret_ii->r += v.i * v.i + v.r * v.r;
                ret_ii->i += 1.0;
                break;
            case 4:
                ret_ii->r += std::sqrt(v.i * v.i + v.r * v.r);
                ret_ii->i += 1.0;
                break;
            case 5:
                tmp_v = std::sqrt(v.i * v.i + v.r * v.r);
                ret_ii->r += v.r / tmp_v;
                ret_ii->i += v.i / tmp_v;
                break;
            }
        }
    }
    
}

// [[Rcpp::export]]
SEXP FARR_collapse_complex(
        const std::string& filebase, 
        const NumericVector& dim,
        const IntegerVector& keep, 
        const NumericVector& cum_part,
        int method = 1,
        bool remove_na = false,
        double scale = 1.0
){
    int nprot = 0;
    int ndims = dim.length();
    
    // 1. check if keep has lastdim
    // 2. calculate ret size
    R_xlen_t retlen = 1;
    R_xlen_t retlen_ii = 1;
    int tmp;
    bool keep_lastdim = false;
    for(R_xlen_t i = 0; i < keep.length(); i++){
        tmp = keep[i];
        if( tmp == ndims ){
            keep_lastdim = true;
        }
        if( i+1 == keep.length() ){
            retlen_ii = retlen;
        }
        retlen *= dim[tmp - 1];
    }
    SEXP ret_cplx = PROTECT(Rf_allocVector(CPLXSXP, retlen)); 
    Rf_setAttrib(ret_cplx, R_DimSymbol, dim[keep - 1]);
    
    SEXP dim_int64 = PROTECT(realToUint64(dim, 0, 1200000000000000000, 1)); nprot++;
    int64_t* last_dimptr = ((int64_t*) REAL(dim_int64)) + (ndims - 1);
    
    SEXP cum_part64 = PROTECT(realToUint64(cum_part, 0, 1200000000000000000, 1)); nprot++;
    int64_t* cum_part64ptr = (int64_t*) REAL(cum_part64);
    R_xlen_t nparts = Rf_xlength(cum_part64);
    
    int64_t part_size = 0, last_size = 0;
    std::string partition_path = "";
    FILE* conn = NULL;
    Rcomplex* retcplx_ptr = COMPLEX(ret_cplx);
    for(R_xlen_t i = 0; i < retlen; i++, retcplx_ptr++){
        retcplx_ptr->i = 0.0;
        retcplx_ptr->r = 0.0;
    }
    
    int buf_size = get_buffer_size();
    int buflen = buf_size / 8;
    SEXP buffer_real = PROTECT(Rf_allocVector(REALSXP, buflen)); nprot++;
    SEXP buffer_cplx = PROTECT(Rf_allocVector(CPLXSXP, buflen)); nprot++;
    
    SEXP loc = PROTECT(Rf_allocVector(REALSXP, ndims)); nprot++;
    // Rf_setAttrib(loc, R_ClassSymbol, wrap("integer64"));
    
    for(R_xlen_t part = 0; part < nparts; part++){
        part_size = *(cum_part64ptr + part) - last_size;
        if( keep_lastdim ){
            retcplx_ptr = COMPLEX(ret_cplx) + last_size * retlen_ii;
        } else {
            retcplx_ptr = COMPLEX(ret_cplx);
        }
        last_size += part_size;
        
        *last_dimptr = part_size;
        partition_path = filebase + std::to_string(part) + ".farr";
        
        conn = fopen(partition_path.c_str(), "rb"); 
        
        try{
            // collapse_complex(
            //     FILE* conn, const SEXP& dim, SEXP keep_dim, 
            //     double* bufreal_ptr, Rcomplex* bufcplx_ptr,
            //     int buf_size, Rcomplex* ret, SEXP loc, int method, 
            //     bool remove_na)
            collapse_complex(
                conn, dim_int64, keep,
                REAL(buffer_real), COMPLEX(buffer_cplx),
                buf_size, retcplx_ptr, loc, method,
                remove_na);
        }catch(...){}
        
        if(conn != NULL){
            fclose(conn);
            conn = NULL;
        }
    }
    
    UNPROTECT(nprot);
    nprot = 1;
    
    SEXP ret = ret_cplx;
    if(method >= 2 && method <= 4){
        ret = PROTECT(Rf_allocVector(REALSXP, retlen)); nprot++;
        Rf_setAttrib(ret, R_DimSymbol, dim[keep - 1]);
        double* ret_ptr = REAL(ret);
        retcplx_ptr = COMPLEX(ret_cplx);
        for(R_xlen_t i = 0; i < retlen; i++, retcplx_ptr++){
            *ret_ptr++ = retcplx_ptr->r * scale;
        }
    } else {
        retcplx_ptr = COMPLEX(ret_cplx);
        for(R_xlen_t i = 0; i < retlen; i++, retcplx_ptr++){
            retcplx_ptr->r *= scale;
            retcplx_ptr->i *= scale;
        }
    }
    
    
    UNPROTECT(nprot); // ret_cplx, ret
    return(ret);
}

/*** R
# devtools::load_all()
require(filearray); require(bit64)
filearray_threads(4)
# filearray:::set_buffer_size(16384 *4)
# dim <- c(15, 100, 301, 287)
dim <- c(6,6,6,6)
set.seed(1); file <- tempfile(); unlink(file, recursive = TRUE)
x <- filearray_create(file, dim)
y <- array(rnorm(prod(dim)), dim)
# y <- array(1:(prod(dim)), dim)
x[] <- y

filebase <- paste0(x$.filebase, x$.sep)
keep = c(3,2)
system.time({
    b <- dipsaus::collapse(x[], keep, FALSE)
}, gcFirst = TRUE)

system.time({
    dim1 <- dim
    a <-
        FARR_collapse(
            filebase = filebase,
            dim = dim1,
            keep = keep,
            cum_part = x$.partition_info[, 3],
            array_type = x$sexp_type(),
            method = 1L,
            remove_na = FALSE,
            scale = 1.0
        )
}, gcFirst = TRUE)

range(a-b)
*/
