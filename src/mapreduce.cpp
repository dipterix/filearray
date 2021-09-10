#include "common.h"
using namespace Rcpp;

SEXP each_partition_integer(
        FILE* conn, const int64_t exp_len, const SEXP& buffer, 
        const Function fun, int64_t* count, List ret){
    
    size_t elem_size = sizeof(int);
    
    fseek(conn, FARR_HEADER_LENGTH, SEEK_SET);
    
    int64_t read_len = 0, current_pos = 0;
    
    int* bufptr = INTEGER(buffer);
    R_xlen_t bufferlen = Rf_xlength(buffer);
    
    int64_t rest_len = 0;
    
    while(current_pos < exp_len){
        bufptr = INTEGER(buffer);
        read_len = lendian_fread(bufptr, elem_size, bufferlen, conn);
        for(; read_len < bufferlen; read_len++){
            *(bufptr + read_len) = NA_INTEGER;
        }
        rest_len = exp_len - current_pos;
        if( rest_len > bufferlen ){
            rest_len = bufferlen;
        }
        ret.push_back( fun(buffer, wrap(rest_len), wrap(*count)) );
        current_pos += rest_len;
        *count += rest_len;
    }
    
    return( ret );
    
}

SEXP each_partition_float(
        FILE* conn, const int64_t exp_len, const SEXP& buffer, 
        const Function fun, int64_t* count, List ret){
    
    size_t elem_size = sizeof(float);
    
    fseek(conn, FARR_HEADER_LENGTH, SEEK_SET);
    
    int64_t read_len = 0, current_pos = 0;
    
    R_xlen_t bufferlen = Rf_xlength(buffer);
    double* bufptr = REAL(buffer);
    float* bufflt_ptr = ((float*) REAL(buffer)) + bufferlen;
    
    int64_t rest_len = 0;
    
    while(current_pos < exp_len){
        read_len = lendian_fread(bufflt_ptr, elem_size, bufferlen, conn);
        floatToReal(bufflt_ptr, bufptr, bufferlen);
        
        for(; read_len < bufferlen; read_len++){
            *(bufptr + read_len) = NA_REAL;
        }
        rest_len = exp_len - current_pos;
        if( rest_len > bufferlen ){
            rest_len = bufferlen;
        }
        ret.push_back( fun(buffer, wrap(rest_len), wrap(*count)) );
        current_pos += rest_len;
        *count += rest_len;
    }
    
    return( ret );
    
}

SEXP each_partition_double(
        FILE* conn, const int64_t exp_len, const SEXP& buffer, 
        const Function fun, int64_t* count, List ret){
    
    size_t elem_size = sizeof(double);
    
    fseek(conn, FARR_HEADER_LENGTH, SEEK_SET);
    
    int64_t read_len = 0, current_pos = 0;
    
    double* bufptr = REAL(buffer);
    R_xlen_t bufferlen = Rf_xlength(buffer);
    
    int64_t rest_len = 0;
    
    while(current_pos < exp_len){
        bufptr = REAL(buffer);
        read_len = lendian_fread(bufptr, elem_size, bufferlen, conn);
        for(; read_len < bufferlen; read_len++){
            *(bufptr + read_len) = NA_REAL;
        }
        rest_len = exp_len - current_pos;
        if( rest_len > bufferlen ){
            rest_len = bufferlen;
        }
        ret.push_back( fun(buffer, wrap(rest_len), wrap(*count)) );
        current_pos += rest_len;
        *count += rest_len;
    }
    
    return( ret );
    
}

SEXP each_partition_raw(
        FILE* conn, const int64_t exp_len, const SEXP& buffer, 
        const Function fun, int64_t* count, List ret){
    
    size_t elem_size = sizeof(Rbyte);
    
    fseek(conn, FARR_HEADER_LENGTH, SEEK_SET);
    
    int64_t read_len = 0, current_pos = 0;
    
    Rbyte* bufptr = RAW(buffer);
    R_xlen_t bufferlen = Rf_xlength(buffer);
    
    int64_t rest_len = 0;
    
    while(current_pos < exp_len){
        bufptr = RAW(buffer);
        read_len = lendian_fread(bufptr, elem_size, bufferlen, conn);
        for(; read_len < bufferlen; read_len++){
            *(bufptr + read_len) = 0;
        }
        rest_len = exp_len - current_pos;
        if( rest_len > bufferlen ){
            rest_len = bufferlen;
        }
        ret.push_back( fun(buffer, wrap(rest_len), wrap(*count)) );
        current_pos += rest_len;
        *count += rest_len;
    }
    
    return( ret );
    
}

SEXP each_partition_logical(
        FILE* conn, const int64_t exp_len, const SEXP& buffer, 
        const Function fun, int64_t* count, List ret){
    
    size_t elem_size = sizeof(Rbyte);
    int* bufptr = LOGICAL(buffer);
    R_xlen_t bufferlen = Rf_xlength(buffer);
    
    SEXP buf2 = PROTECT(Rf_allocVector(RAWSXP, bufferlen));
    Rbyte* buf2ptr = RAW(buf2);
    
    fseek(conn, FARR_HEADER_LENGTH, SEEK_SET);
    
    int64_t read_len = 0, current_pos = 0;
    int64_t rest_len = 0, jj = 0;
    
    while(current_pos < exp_len){
        buf2ptr = RAW(buf2);
        read_len = lendian_fread(buf2ptr, elem_size, bufferlen, conn);
        
        bufptr = LOGICAL(buffer);
        for(jj = 0; jj < read_len; jj++, bufptr++, buf2ptr++){
            if(*buf2ptr >= 2){
                *bufptr = NA_LOGICAL;
            } else {
                *bufptr = *buf2ptr;
            }
        }
        for(; jj < bufferlen; jj++, bufptr++){
            *bufptr = NA_LOGICAL;
        }
        rest_len = exp_len - current_pos;
        if( rest_len > bufferlen ){
            rest_len = bufferlen;
        }
        ret.push_back( fun(buffer, wrap(rest_len), wrap(*count)) );
        current_pos += rest_len;
        *count += rest_len;
    }
    
    UNPROTECT(1);
    
    return( ret );
    
}

SEXP each_partition_complex(
        FILE* conn, const int64_t exp_len, 
        const SEXP& buffer_cplx, const SEXP& buffer_real, 
        const Function fun, int64_t* count, List ret){
    
    size_t elem_size = sizeof(double);
    
    fseek(conn, FARR_HEADER_LENGTH, SEEK_SET);
    
    int64_t read_len = 0, current_pos = 0;
    
    double* bufreal_ptr = REAL(buffer_real);
    Rcomplex* bufcplx_ptr = COMPLEX(buffer_cplx);
    R_xlen_t bufferlen = Rf_xlength(buffer_real);
    
    int64_t rest_len = 0;
    Rcomplex* bufcplx_ptr2 = bufcplx_ptr;
    
    while(current_pos < exp_len){
        bufreal_ptr = REAL(buffer_real);
        read_len = lendian_fread(bufreal_ptr, elem_size, bufferlen, conn);
        realToCplx(bufreal_ptr, bufcplx_ptr, bufferlen);
        bufcplx_ptr2 = bufcplx_ptr + read_len;
        for(; read_len < bufferlen; read_len++, bufcplx_ptr2++){
            bufcplx_ptr2->i = NA_REAL;
            bufcplx_ptr2->r = NA_REAL;
        }
        rest_len = exp_len - current_pos;
        if( rest_len > bufferlen ){
            rest_len = bufferlen;
        }
        ret.push_back( fun(buffer_cplx, wrap(rest_len), wrap(*count)) );
        current_pos += rest_len;
        *count += rest_len;
    }
    
    return( ret );
    
}


// [[Rcpp::export]]
SEXP FARR_buffer_mapreduce(
    const std::string& filebase, 
    const Function map, const Nullable<Function> reduce,
    const NumericVector& dim,
    const NumericVector& partition_cumlens, 
    const int bufferlen, const SEXPTYPE x_type
){
    int nprot = 0;
    std::string fbase = correct_filebase(filebase);
    int64_t count = 1;
    int64_t count2 = 1;
    R_xlen_t nparts = partition_cumlens.length();
    
    // int64_t bufferlen = get_buffer_size() / elem_size;
    SEXP buffer = R_NilValue;
    if( x_type == FLTSXP ){
        buffer = PROTECT(Rf_allocVector(REALSXP, bufferlen)); nprot++;
    } else {
        buffer = PROTECT(Rf_allocVector(x_type, bufferlen)); nprot++;
    }
    SEXP buffer_mid = R_NilValue;
    if( x_type == CPLXSXP ){
        buffer_mid = PROTECT(Rf_allocVector(REALSXP, bufferlen)); nprot++;
    }
    
    List ret = List::create();
    
    SEXP dim_ = PROTECT(realToInt64(dim, 0, NA_REAL, 1)); nprot++;
    SEXP pcumlens = PROTECT(realToInt64(partition_cumlens, 0, NA_REAL, 1)); nprot++;
    int64_t* dimptr = (int64_t*) REAL(dim_);
    int64_t plen = 1;
    for(R_xlen_t ii = 0; ii < dim.length() - 1; ii++, dimptr++){
        plen *= *dimptr;
    }
    int64_t* pclptr = (int64_t*) REAL(pcumlens);
    int64_t psize = 0;
    
    std::string partition_path = "";
    FILE* conn = NULL;
    for(R_xlen_t part = 0; part < nparts; part++){
        partition_path = fbase + std::to_string(part) + ".farr";
        
        if(part == 0){
            count = count2;
            psize = (*pclptr + part);
        } else {
            psize = (*pclptr + part) - (*pclptr + (part-1));
            count = count2 + plen * (*pclptr + (part-1));
        }
        
        
        conn = fopen(partition_path.c_str(), "rb");
        
        if( conn ){
            try{
                switch(x_type){
                case INTSXP:
                    ret = each_partition_integer(conn, psize * plen, buffer, map, &(count), ret);
                    break;
                case REALSXP:
                    ret = each_partition_double(conn, psize * plen, buffer, map, &(count), ret);
                    break;
                case FLTSXP:
                    ret = each_partition_float(conn, psize * plen, buffer, map, &(count), ret);
                    break;
                case RAWSXP:
                    ret = each_partition_raw(conn, psize * plen, buffer, map, &(count), ret);
                    break;
                case LGLSXP:
                    ret = each_partition_logical(conn, psize * plen, buffer, map, &(count), ret);
                    break;
                case CPLXSXP:
                    ret = each_partition_complex(conn, psize * plen, buffer, buffer_mid, map, &(count), ret);
                    break;
                default: 
                    fclose(conn);
                    conn = NULL;
                }
            } catch(...){}
            if(conn != NULL){
                fclose(conn);
                conn = NULL;
            }
        }
    }
    
    if(reduce == R_NilValue){
        UNPROTECT( nprot );
        return ret;
    }
    
    Function reduce2 = (Function) reduce;
    SEXP re = PROTECT(reduce2(ret));
    nprot++;
    UNPROTECT( nprot );
    return(re);
}

/*** R
# devtools::load_all()
dim <- c(100,100,100,100)
set.seed(1); file <- tempfile(); unlink(file, recursive = TRUE)
x <- filearray_create(file, dim)
tmp <- seq_len(1e8)
setThreads(8)
system.time({
    x[] <- tmp
}, gcFirst = TRUE)
rm(tmp); gc()

mapreduce(x, \(data){
    max(data, na.rm = TRUE)
}, \(x){
    do.call('max', x)
})

system.time=profvis::profvis
gc()
system.time({
    filebase <- paste0(x$.filebase, x$.sep)
    set_buffer_size(max_buffer_size())
    FARR_buffer_mapreduce(filebase, function(buffer, size, idx){
        if(size != length(buffer)){
            buffer <- buffer[seq_len(size)]
        }
        re <- which(buffer == 5000)
        if(length(re)){
            re <- re + (idx-1)
        }
        # re <- max(buffer, na.rm = TRUE)
        re
    }, function(ret){
        do.call(c, ret)
    }, x$dimension(), x$.partition_info[,3], 1000000, 14L)
})

setThreads(1)
gc()
system.time({
    max(sapply(1:100, function(i){
        a <- x[,,,i]
        max(a, na.rm = TRUE)
    }))
})

*/
