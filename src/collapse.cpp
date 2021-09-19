#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>
// [[Rcpp::depends(BH)]]

#include "core.h"
#include "openmp.h"
#include "conversion.h"
#include "serialize.h"
using namespace Rcpp;

template <typename T>
void collapse(
        const std::string& partition_path, 
        const SEXP& dim, SEXP keep_dim, 
        double* ret, T na, int method, 
        bool remove_na, const double& scale){
    
    bool swap_endian = !isLittleEndian();
    
    int elem_size = sizeof(T);
    int ndims = Rf_length(dim);
    
    const boost::interprocess::mode_t mode = boost::interprocess::read_only;
    
    // dim are int64_t, keep_dim are integers
    int64_t* dimptr = INTEGER64(dim);
    int64_t partlen = 1;
    for(int i = 0; i < ndims; i++){
        partlen *= *(dimptr + i);
    }
    
    int keeplen = Rf_length(keep_dim);
    int* ptrkeep = INTEGER(keep_dim);
    
    boost::interprocess::file_mapping fm;
    boost::interprocess::mapped_region region;
    bool is_open = false;
    try {
        fm = boost::interprocess::file_mapping(partition_path.c_str(), mode);
        region = boost::interprocess::mapped_region(
                fm, mode, FARR_HEADER_LENGTH);
        region.advise(boost::interprocess::mapped_region::advice_sequential);
        
        if( region.get_size() >= partlen * sizeof(T)){
            is_open = true;
        }
    } catch (...) {
    }
    
    // int ncores = 1;
    // int ncores = getThreads();
    // ncores = ncores > partlen ? partlen : ncores;
    
    // calculate loc in original array
    // std::vector<SEXP> locs(ncores);
    // for(int ii = 0; ii < ncores; ii++){
    //     locs[ii] = PROTECT(Rf_allocVector(INT64SXP, ndims));
    // }
    SEXP loc = PROTECT(Rf_allocVector(INT64SXP, ndims));
    int64_t* locptr = INTEGER64(loc);
    
// #pragma omp parallel num_threads(ncores)
// {
    int64_t rem = 0, fct = 1, tmp = 0;
    int64_t* locptr_ii;
    int64_t* dimptr_ii = dimptr;
    int* ptrkeep_ii = ptrkeep;
    int ii = 0;
    T* mmap_ptr = static_cast<T*>(region.get_address());
    T v = 0;
    double v2 = 0.0;
    
// #pragma omp for schedule(static, 1) nowait
    for(int64_t idx = 0; idx < partlen; idx++){
        if( is_open ){
            v = *(mmap_ptr + idx);
            if(swap_endian){
                swap_endianess(&(v), elem_size, 1);
            }
        } else {
            v = na;
        }
        
        if( remove_na && ( ISNAN(v) || v == na )){
            continue;
        }
        
        // int thread = (int) (idx % ncores);
        // int64_t* locptr = INTEGER64(locs[thread]);
        locptr = INTEGER64(loc);
        
        // calculate position in ret
        rem = idx; 
        locptr_ii = locptr; 
        dimptr_ii = dimptr;
        for(ii = 0; ii < ndims; ii++, dimptr_ii++, locptr_ii++){
            *locptr_ii = rem % (*dimptr_ii);
            rem = (rem - *locptr_ii) / *dimptr_ii;
        }
        // if( rem > 0 ){ continue; }
        rem = 0; 
        fct = 1, tmp = 0; 
        ptrkeep_ii = ptrkeep;
        for(ii = 0; ii < keeplen; ii++, ptrkeep_ii++){
            tmp = *ptrkeep_ii - 1;
            rem += fct * (*(locptr + tmp));
            fct *= *(dimptr + tmp);
        }
        
        if( ISNAN(v) || v == na ){ // remove_na must be false
            v2 = NA_REAL;
        } else if (!remove_na && *(ret + rem) == NA_REAL){
            continue;
        } else {
            switch(method){
            case 1: 
                v2 = ((double) v) * scale;
                break;
            case 2:
                v2 = std::log10((double) v) * (10.0 * scale);
                break;
            case 3:
                v2 = std::pow((double) v, 2.0) * scale;
                break;
            case 4:
                v2 = std::sqrt((double) v) * scale;
                break;
            default:
                continue;
            }
        }
        
        // rem is index of ret
        // need to be atom
// #pragma omp atomic
        *(ret + rem) += v2;
    }
// }

    UNPROTECT(1);
    
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
    std::string fbase = correct_filebase(filebase);
    int ndims = dim.length();
    SEXP dim_int64 = PROTECT(realToInt64(dim, 0, NA_REAL, 1));
    
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
    
    SEXP cum_part64 = PROTECT(realToInt64(cum_part, 0, NA_REAL, 1));
    int64_t* cum_part64ptr = (int64_t*) REAL(cum_part64);
    R_xlen_t nparts = Rf_xlength(cum_part64);
    
    int64_t part_size = 0, last_size = 0;
    std::string partition_path = "";
    
    double* retptr = REAL(ret);
    for(R_xlen_t i = 0; i < retlen; i++){
        *retptr++ = 0;
    }
    
    for(R_xlen_t part = 0; part < nparts; part++){
        part_size = *(cum_part64ptr + part) - last_size;
        if( keep_lastdim ){
            retptr = REAL(ret) + last_size * retlen_ii;
        } else {
            retptr = REAL(ret);
        }
        last_size += part_size;
        
        *last_dimptr = part_size;
        partition_path = fbase + std::to_string(part) + ".farr";
        
        try{
            // collapse_double(conn, dim_int64, keep, retptr);
            switch(array_type){
            case REALSXP:
                collapse(partition_path, dim_int64, keep, 
                         retptr, NA_REAL, method, 
                         remove_na, scale);
                break;
            case INTSXP:
                collapse(partition_path, dim_int64, keep, 
                         retptr, NA_INTEGER, method, 
                         remove_na, scale);
                break;
            case FLTSXP: {
                collapse(partition_path, dim_int64, keep, 
                         retptr, NA_FLOAT, method, 
                         remove_na, scale);
                break;
            }
            case LGLSXP: {
                Rbyte na_lgl = 2;
                collapse(partition_path, dim_int64, keep, 
                         retptr, na_lgl, method, 
                         remove_na, scale);
                break;
            }
            case RAWSXP: {
                Rbyte na_lgl = 0;
                collapse(partition_path, dim_int64, keep, 
                         retptr, na_lgl, method, 
                         true, scale);
                break;
            }
            }
        }catch(...){}
        
    }
    
    // retptr = REAL(ret);
    // for(R_xlen_t i = 0; i < retlen; i++){
    //     *retptr++ *= scale;
    // }
    
    
    UNPROTECT(3);
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
    std::string fbase = correct_filebase(filebase);
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
    
    SEXP dim_int64 = PROTECT(realToInt64(dim, 0, NA_REAL, 1)); nprot++;
    int64_t* last_dimptr = ((int64_t*) REAL(dim_int64)) + (ndims - 1);
    
    SEXP cum_part64 = PROTECT(realToInt64(cum_part, 0, NA_REAL, 1)); nprot++;
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
        partition_path = fbase + std::to_string(part) + ".farr";
        
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
