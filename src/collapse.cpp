#include "common.h"
using namespace Rcpp;

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
keep = c(3,2)
system.time({
    b <- dipsaus::collapse(x[], keep, FALSE)
}, gcFirst = TRUE)

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
