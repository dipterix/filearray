#include "common.h"
using namespace Rcpp;

template <typename T>
void collapse(
        FILE* conn, const SEXP& dim, SEXP keep_dim, 
        T* bufptr, int buf_size,
        T* ret, T na, SEXP loc){
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
            // rem is index of ret
            // need to be atom
            *(ret + rem) += *(bufptr + jj);
        }
    }
    
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
    
    int buf_size = get_buffer_size();
    SEXP buffer = PROTECT(Rf_allocVector(REALSXP, buf_size / 8));
    
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
            collapse(conn, dim_int64, keep, 
                REAL(buffer), buf_size,
                retptr, NA_REAL, loc);
        }catch(...){}
        
        if(conn != NULL){
            fclose(conn);
            conn = NULL;
        }
    }
    
    
    UNPROTECT(5);
    return(ret);
}

/*** R
# devtools::load_all()
require(filearray); require(bit64)
filearray_threads(4)
filearray:::set_buffer_size(16384 *4)
dim <- c(15, 100, 301, 287)
# dim <- c(6,6,6,6)
set.seed(1); file <- tempfile(); unlink(file, recursive = TRUE)
x <- filearray_create(file, dim)
y <- array(rnorm(prod(dim)), dim)
# y <- array(1:(prod(dim)), dim)
x[] <- y

filebase <- paste0(x$.filebase, x$.sep)
keep = c(2:3)
system.time({
    b <- dipsaus::collapse(y, keep, FALSE)
})

system.time({
    dim1 <- dim
    a <- FARR_collapse(filebase, dim1, keep, x$.partition_info[,3])
}, gcFirst = TRUE)
# a

# system.time({
#     b <- apply(y, keep, sum)
# })


range(a-b)
*/
