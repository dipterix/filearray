#include "common.h"
#include "unserialize.h"
#include "core.h"
using namespace Rcpp;

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
    
    double last_margin = *(REAL(dimension) + (ndims - 1));
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
    
    // R_gc();
    
    List re = List::create(
        _["file_version"] = file_version,
        _["dimension"] = dimension,
        _["sexp_type"] = sexp_type,
        _["elem_size"] = elem_size,
        _["partition_size"] = partition_size,
        _["length"] = total_len,
        _["cumsum_part_sizes"] = cum_part_size,
        _["dimnames"] = dimnames
    );
    UNPROTECT(3 + (content_len2 > 0));
    
    return re;
    
}


/*** R

*/
