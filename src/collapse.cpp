#include "common.h"
#include "openmp.h"
using namespace Rcpp;


void collapse_double(
        FILE* conn, const SEXP& dim, SEXP keep_dim, 
        double* ret){
    
    int elem_size = sizeof(double);
    int ndims = Rf_length(dim);
    
    // dim are int64_t, keep_dim are integers
    int64_t* dimptr = (int64_t*) REAL(dim);
    int64_t retlen = 1;
    for(int ii = 0; ii < ndims; ii++){
        retlen *= *(dimptr + ii);
    }
    
    // calculate loc in original array
    SEXP loc = PROTECT(Rf_allocVector(REALSXP, ndims));
    int64_t* locptr = (int64_t*) REAL(loc);
    
    int keeplen = Rf_length(keep_dim);
    int* ptrkeep = INTEGER(keep_dim);
    int* ptrkeep_ii = ptrkeep;
    
    
    if(conn != NULL){
        fseek(conn, FARR_HEADER_LENGTH, SEEK_SET);
    }
    
    int64_t buflen = get_buffer_size() / elem_size;
    
    int64_t niter = retlen % buflen;
    if( niter == 0 ){
        niter = retlen / buflen;
    } else {
        niter = (retlen - niter) / buflen + 1;
    }
    
    int ncores = getThreads();
    if( ncores > niter ){
        ncores = niter;
    }
    
    SEXP buffer = PROTECT(Rf_allocVector(REALSXP, buflen));
    double* bufptr = REAL(buffer);
    int64_t current_pos = 0;
    int64_t readlen = 0;
    int ii = 0;
    int64_t rem = 0, fct = 0, tmp = 0;
    int64_t* locptr_ii = locptr;
    int64_t* dimptr_ii = dimptr;
    
    for(int64_t iter = 0; iter < niter; iter++){
        current_pos = iter * buflen;
        readlen = buflen;
        if( iter == niter - 1 ){
            readlen = retlen - current_pos;
        }
        if(conn != NULL){
            lendian_fread(bufptr, elem_size, readlen, conn);
        } else {
            double* bufptr_ii = bufptr;
            for(int64_t kk = 0; kk < readlen; kk++){
                *bufptr_ii++ = NA_REAL;
            }
        }
        
        for(int64_t jj = 0; jj < readlen; jj++){
            rem = jj + current_pos; 
            locptr_ii = locptr; 
            dimptr_ii = dimptr;
            for(ii = 0; ii < ndims; ii++, dimptr_ii++, locptr_ii++){
                *locptr_ii = rem % (*dimptr_ii);
                rem = (rem - *locptr_ii) / *dimptr_ii;
            }
            rem = 0; fct = 1; ptrkeep_ii = ptrkeep;
            for(ii = 0; ii < keeplen; ii++, ptrkeep_ii++){
                tmp = *ptrkeep_ii - 1;
                rem += fct * (*(locptr + tmp));
                fct *= *(dimptr + tmp);
            }
            // rem is index of ret
            // need to be atom
            *(ret + rem) += *(bufptr + jj);
        }
    }
    UNPROTECT(2);
}


// [[Rcpp::export]]
SEXP FARR_collapse(
        const std::string& filebase, 
        const NumericVector& dim,
        const IntegerVector& keep, 
        const NumericVector& cum_part
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
    
    for(R_xlen_t part = 0; part < nparts; part++){
        if( keep_lastdim ){
            retptr = REAL(ret) + last_size * retlen_ii;
        } else {
            retptr = REAL(ret);
        }
        part_size = *(cum_part64ptr + part) - last_size;
        last_size += part_size;
        
        *last_dimptr = part_size;
        partition_path = filebase + std::to_string(part) + ".farr";
        
        conn = fopen(partition_path.c_str(), "rb"); 
        
        try{
            collapse_double(conn, dim_int64, keep, retptr);
        }catch(...){}
        
        if(conn != NULL){
            fclose(conn);
            conn = NULL;
        }
    }
    
    
    UNPROTECT(3);
    return(ret);
}

/*** R
# devtools::load_all()
require(filearray)
filearray_threads(1)
dim <- c(287, 100, 301, 15)
set.seed(1); file <- tempfile(); unlink(file, recursive = TRUE)
x <- filearray_create(file, dim)
y <- array(rnorm(prod(dim)), dim)
x[] <- y

filebase <- paste0(x$.filebase, x$.sep)
keep = c(3,4)
system.time({
    a <- FARR_collapse(filebase, dim, keep, 1:(dim[length(dim)]))
})
system.time({
    b <- apply(y, keep, sum)
})
system.time({
    dipsaus::collapse(y, keep, FALSE)
})

range(a-b)
*/
