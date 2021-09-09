#include "unserialize.h"
#include "common.h"
#include "load.h"
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

// [[Rcpp::export]]
List FARR_meta(const std::string& filebase) {
    // read information from meta
    std::string meta = correct_filebase(filebase) + "meta";

    FILE* conn = fopen(meta.c_str(), "rb");

    if( conn == NULL ){
        stop("`FileArray`: cannot find meta information. Make sure `filebase` is a directory containing 'meta' file.");
    }
    
    void* buf = malloc(FARR_HEADER_LENGTH);
    if(buf == NULL) {
        fclose(conn);
        conn = NULL;
        free(buf);
        buf = NULL;
        stop("Cannot allocate 1KB of memory to read meta file");
    }
    
    int nread = lendian_fread(buf, 1, FARR_HEADER_LENGTH, conn);
    if( nread < FARR_HEADER_LENGTH ){
        fclose(conn);
        conn = NULL;
        free(buf);
        buf = NULL;
        stop("Invalid header length");
    }
    
    char* buf2 = (char*) buf;
    double content_len = 0.0;
    memcpy(&(content_len), buf2 + (FARR_HEADER_LENGTH - 8), 8);
    size_t content_len2 = (size_t) content_len;
    
    SEXP dimnames = R_NilValue;
    if( content_len2 > 0 ){
        PROTECT(dimnames = unserialize_connection(conn, content_len2));
    }
    fclose(conn);
    conn = NULL;
    
    SEXP file_version = PROTECT(Rf_allocVector(INTSXP, 3)); 
    memcpy(INTEGER(file_version), buf2 + 8, 3 * 4); // 20
    
    int sexp_type = 0;
    memcpy(&sexp_type, buf2 + 20, 4); // 24
    
    int elem_size = 0;
    memcpy(&elem_size, buf2 + 24, 4); // 28
    
    double partition_size = 0.0;
    memcpy(&partition_size, buf2 + 28, 8); // 36
    
    double total_len = 0.0;
    memcpy(&total_len, buf2 + 36, 8); // 44
    
    int ndims = 0;
    memcpy(&ndims, buf2 + 44, 4); // 48
    
    SEXP dimension = PROTECT(Rf_allocVector(REALSXP, ndims));
    memcpy(REAL(dimension), buf2 + 48, ndims * 8);
    
    free(buf);
    buf = NULL;
    buf2 = NULL;
    
    // R_gc();
    
    List re = List::create(
        _["file_version"] = file_version,
        _["dimension"] = dimension,
        _["sexp_type"] = sexp_type,
        _["elem_size"] = elem_size,
        _["partition_size"] = partition_size,
        _["length"] = total_len,
        _["dimnames"] = dimnames
    );
    UNPROTECT(2 + (content_len2 > 0));
    
    return re;
    
}

// [[Rcpp::export]]
SEXP FARR_subset2(
        const std::string& filebase,
        const SEXP listOrEnv,
        const SEXP reshape = R_NilValue,
        const bool drop = false,
        const bool use_dimnames = true,
        const size_t thread_buffer = 2097152,
        int split_dim = 0,
        const int strict = 1
) {
    const std::string fbase = correct_filebase(filebase);
    // List re = List::create(
    //     _["file_version"] = file_ver,
    //     _["dimension"] = dim,
    //     _["sexp_type"] = sexp_type,
    //     _["elem_size"] = elem_size,
    //     _["partition_size"] = partition_size,
    //     _["length"] = total_len,
    //     _["dimnames"] = dimnames
    // );
    List meta = FARR_meta(fbase);
    const int elem_size = meta["elem_size"];
    const SEXPTYPE sexp_type = meta["sexp_type"];
    SEXP dim = meta["dimension"]; // double
    double partition_size = meta["partition_size"];
    
    R_len_t ndims = Rf_length(dim);
    double last_margin = *(REAL(dim) + (ndims - 1));
    
    double nparts = std::ceil(last_margin / partition_size);
    SEXP cum_part_size = PROTECT(Rf_allocVector(REALSXP, (uint64_t) nparts));
    double tmp = 0;
    for( R_xlen_t part = 0; part < Rf_xlength(cum_part_size); part++ ){
        tmp += partition_size;
        if( tmp > last_margin ){
            tmp = last_margin;
        }
        if( tmp <= last_margin ){
            *(REAL(cum_part_size) + part) = tmp;
        }
    }
    
    // calculate split_dim
    int ii;
    if( split_dim == NA_INTEGER || split_dim == 0 ){
        int dim_ii;
        double idx1len, idx2len, nloops, buffer_sz;
        double nops, min_ops = -1.0;
        split_dim = 1;
        for(dim_ii = 1; dim_ii <= ndims - 1; dim_ii++){
            idx1len = 1.0;
            idx2len = 1.0;
            for(ii = 0; ii < dim_ii; ii++){
                idx1len *= *(REAL(dim) + ii);
            }
            for(ii = dim_ii; ii < ndims - 1; ii++ ){
                idx2len *= *(REAL(dim) + ii);
            }
            if( idx1len * elem_size - (double) thread_buffer > 0.0 ){
                buffer_sz = (double) (thread_buffer / elem_size);
            } else {
                buffer_sz = idx1len;
            }
            nloops = std::ceil(idx1len / buffer_sz);
            nops = (idx1len * nloops + idx2len) * idx2len;
            if( min_ops <= 0 || min_ops >= nops ){
                min_ops = nops;
                split_dim = dim_ii;
            }
        }
    } else if (split_dim < 1 || split_dim > ndims-1 ){
        stop("Incorrect `split_dim`: must be an integer from 1 to ndims-1 ");
    }
    int buf_bytes = elem_size;
    for(ii = 0; ii < split_dim; ii++){
        buf_bytes *= (int) (*(REAL(dim) + ii));
        if( buf_bytes > thread_buffer ){
            buf_bytes = thread_buffer;
            break;
        }
    }
    if(buf_bytes == NA_INTEGER || buf_bytes <= 16){
        buf_bytes = 65536;
    }
    set_buffer_size(buf_bytes);
    
    // derive dimnames
    SEXP dnames = R_NilValue;
    SEXP sliceIdx = PROTECT(locationList(listOrEnv, dim, 1));
    Rf_setAttrib(sliceIdx, wrap("_asis_"), wrap(true));
    
    if( use_dimnames ){
        dnames = meta["dimnames"];
        if( TYPEOF(dnames) == VECSXP && Rf_length(dnames) == ndims ){
            for(ii = 0; ii < ndims; ii++){
                SEXP dn = VECTOR_ELT(dnames, ii);
                if( dn == R_NilValue ){
                    continue;
                }
                SEXP idx = VECTOR_ELT(sliceIdx, ii);
                SEXP sub_el = PROTECT(sub_vec(dn, idx));
                SET_VECTOR_ELT(dnames, ii, sub_el);
                UNPROTECT(1);
            }
        }
    }
    SEXP res = PROTECT(FARR_subset(fbase, sexp_type, sliceIdx, dim, cum_part_size,
                                   split_dim, reshape, drop, strict, dnames));
    
    // R_gc();
    
    UNPROTECT(3);
    return(res);
}


/*** R
set.seed(1)
require(filearray)
file <- tempfile(); unlink(file, recursive = TRUE)
x <- filearray_create(file, 3:5, type = "float")
dimnames(x) = list(1:3, 1:4, 1:5)

# FARR_subset2(
#     x$.filebase,
#     list(1:2, 3, 2), use_dimnames = T, drop = FALSE
# )

FARR_meta(x$.filebase)
*/
