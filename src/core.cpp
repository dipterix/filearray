#include "serialize.h"
#include "conversion.h"
#include "core.h"
using namespace Rcpp;

static int nbuffers = 65536;

int system_buffer_size(){
    return(BUFSIZ);
}

// [[Rcpp::export]]
int set_buffer_size(int size){
    if( size <= 0 || size == NA_INTEGER ){
        stop("Buffer size must be positive. (non-positive or NA detected)");
    }
    int count = 0;
    int k = size;
    while( k != 1 ){
        k = k >> 1;
        count++;
    }
    k = k << count;
    if(k != size){
        k = k << 1;
    }
    if(k < 16){
        k = 16;
    }
    nbuffers = k;
    return nbuffers;
}

// [[Rcpp::export]]
int get_buffer_size(){
    return nbuffers;
}



std::string correct_filebase(const std::string& filebase){
#ifdef Win32
    std::string filesep = "\\";
#else
    std::string filesep = "/";
#endif
    if(filebase.compare(filebase.length() - 1, 1, filesep) != 0){
        return( filebase + filesep );
    } else {
        return( filebase );
    }
}

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
    
    int nread = fread(buf, 1, FARR_HEADER_LENGTH, conn);
    if( nread < FARR_HEADER_LENGTH ){
        fclose(conn);
        conn = NULL;
        free(buf);
        buf = NULL;
        stop("Invalid header length");
    }
    
    bool swap_endian = !isLittleEndian();
    
    char* buf2 = (char*) buf;
    double content_len = 0.0;
    memcpy(&(content_len), buf2 + (FARR_HEADER_LENGTH - 8), 8);
    if( swap_endian ){ swap_endianess(&(content_len), 8, 1); }
    size_t content_len2 = (size_t) content_len;
    
    SEXP dimnames = R_NilValue;
    if( content_len2 > 0 ){
        SEXP extra_headers = PROTECT(unserialize_connection(conn, content_len2));
        if(TYPEOF(extra_headers) == VECSXP){
            
            // extra_headers is protected
            SEXP extra_names = PROTECT(Rf_getAttrib(extra_headers, R_NamesSymbol));
            R_xlen_t extra_length = Rf_xlength(extra_names);
            
            if(extra_length){
                double header_version = 0.0;
                const char* st = "";
                const char* hv_name = "__header_version__";
                const char* dn_name = "__dimnames__";
                for (R_xlen_t i = 0 ; i < extra_length ; i = i + 1) {
                    SEXP target = STRING_ELT(extra_names, i);
                    switch (TYPEOF(target)) {
                    case SYMSXP:
                        st = CHAR(PRINTNAME(target));
                        break;
                    case CHARSXP:
                        st = Rf_translateChar(target);
                        break;
                    }
                    if(strncmp(st, hv_name, 19) == 0){
                        target = VECTOR_ELT(extra_headers, i);
                        switch (TYPEOF(target)) {
                        case REALSXP:
                            header_version = REAL(target)[0];
                            break;
                        case INTSXP:
                            header_version = (double)(INTEGER(target)[0]);
                            break;
                        }
                    } else if (strncmp(st, dn_name, 13) == 0) {
                        dimnames = VECTOR_ELT(extra_headers, i);
                    }
                }
                if(header_version < 1.0){
                    // extra_headers itself is the dimnames
                    dimnames = extra_headers;
                }
            }
            
            // extra_names
            UNPROTECT(1);
            
        }
    }
    fclose(conn);
    conn = NULL;
    
    SEXP file_version = PROTECT(Rf_allocVector(INTSXP, 3)); 
    memcpy(INTEGER(file_version), buf2 + 8, 3 * 4); // 20
    if( swap_endian ){ swap_endianess(INTEGER(file_version), 4, 3); }
    
    int sexp_type = 0;
    memcpy(&sexp_type, buf2 + 20, 4); // 24
    if( swap_endian ){ swap_endianess(&sexp_type, 4, 1); }
    
    int elem_size = 0;
    memcpy(&elem_size, buf2 + 24, 4); // 28
    if( swap_endian ){ swap_endianess(&elem_size, 4, 1); }
    
    double partition_size = 0.0;
    memcpy(&partition_size, buf2 + 28, 8); // 36
    if( swap_endian ){ swap_endianess(&partition_size, 8, 1); }
    
    double total_len = 0.0;
    memcpy(&total_len, buf2 + 36, 8); // 44
    if( swap_endian ){ swap_endianess(&total_len, 8, 1); }
    
    int ndims = 0;
    memcpy(&ndims, buf2 + 44, 4); // 48
    if( swap_endian ){ swap_endianess(&ndims, 4, 1); }
    
    SEXP dimension = PROTECT(Rf_allocVector(REALSXP, ndims));
    memcpy(REAL(dimension), buf2 + 48, ndims * 8);
    if( swap_endian ){ swap_endianess(REAL(dimension), 8, ndims); }
    
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

// [[Rcpp::export]]
SEXP loc2idx(const List sliceIdx, const NumericVector& dim){
    // sliceIdx[[i]] REALSEXP but stored with long long
    // List sliceIdx = locationList(listOrEnv, dim, 1);
    
    R_xlen_t exp_len = 1;
    R_xlen_t idx = 0;
    
    if( dim.size() == 0 ){
        exp_len = 0;
    }
    
    for( ; idx < sliceIdx.length(); idx++ ){
        SEXP tmp = sliceIdx[idx];
        exp_len *= Rf_xlength(tmp);
    }
    
    SEXP re = PROTECT( Rf_allocVector(REALSXP, exp_len) );
    
    Rf_setAttrib(re, R_ClassSymbol, Shield<SEXP>(wrap("integer64")));
    if( exp_len == 0 ){
        UNPROTECT(1);
        return( re );
    }
    
    // initialize
    int64_t* reptr_const = (int64_t*) REAL(re);
    int64_t* reptr = reptr_const;
    R_xlen_t ii = 0;
    for(; ii < exp_len; ii++, reptr++){
        *reptr = 0;
    }
    
    reptr = reptr_const;
    
    R_xlen_t step = 1;
    R_xlen_t mag = 1;
    // addCycle(SEXP x, SEXP ret, const R_xlen_t step = 1)
    SEXP el;
    
    for( idx = 0; idx < sliceIdx.length(); idx++ ){
        el = sliceIdx[idx];
        addCycle(el, re, step, mag);
        step *= Rf_xlength(el);
        mag *= dim[idx];
    }
    
    UNPROTECT(1);
    return( re );
}

// [[Rcpp::export]]
SEXP locationList(const SEXP listOrEnv, const NumericVector& dim, const int strict){
    int n_protected = 0;
    const R_xlen_t ndims = dim.length();
    SEXP sliceIdx;
    R_xlen_t idx_size = 0;
    switch(TYPEOF(listOrEnv)) {
    case ENVSXP: {
        sliceIdx = PROTECT(Rf_allocVector(VECSXP, ndims));
        SEXP dots = Rf_findVarInFrame(listOrEnv, R_DotsSymbol);
        n_protected++;
        for(; (dots != R_NilValue) && (dots != R_MissingArg); dots = CDR(dots), idx_size++ ){
            if(idx_size >= ndims){
                UNPROTECT(n_protected); // sliceIdx, dots
                stop("Incorrect subscript dimensions, required: 0, ndim.");
            }
            SET_VECTOR_ELT(sliceIdx, idx_size, PROTECT(CAR(dots)));
            n_protected++;
        }
        break;
    }
    case VECSXP: {
        if( Rf_getAttrib(listOrEnv, Rf_install("_asis_")) == R_NilValue ){
        sliceIdx = listOrEnv;
        idx_size = Rf_xlength(sliceIdx);
    } else {
        return( listOrEnv );
    }
    break;
    }
    default:
        Rcpp::stop("Input `listOrEnv` must be either a list of indices or an environment");
    }
    
    if( !(idx_size == 0 || idx_size == ndims) ){
        UNPROTECT(n_protected); // sliceIdx
        stop("Incorrect subscript dimensions, required: 0, ndim.");
    }
    
    // Evaluate sliceIdx
    SEXP el;
    if( Rf_xlength(sliceIdx) > 0 ){
        R_xlen_t dl;
        for(idx_size = 0; idx_size < ndims; idx_size++ ){
            dl = dim[idx_size];
            el = VECTOR_ELT(sliceIdx, idx_size);
            
            // el might be promise SEXP, if so, evaluate
            if ( TYPEOF(el) == PROMSXP ){
                // This is a promise, need to evaluate
                el = PROTECT(Rf_eval( PREXPR(el), PRENV( el )));
                n_protected++;
                
                if( el == R_NilValue ){
                    el = PROTECT(Rf_allocVector(REALSXP, 0));
                    n_protected++;
                } else {
                    el = PROTECT(realToInt64(as<NumericVector>(el), 1, dl, strict));
                    n_protected++;
                }
                SET_VECTOR_ELT(sliceIdx, idx_size, el);
            } else {
                if( el == R_MissingArg || el == R_NilValue ){
                    el = PROTECT(seq_len_int64( dl ));
                    n_protected++;
                } else {
                    el = PROTECT(realToInt64(as<NumericVector>(el), 1, dl, strict));
                    n_protected++;
                }
                SET_VECTOR_ELT(sliceIdx, idx_size, el);
            }
        }
    } else {
        sliceIdx = PROTECT(Rf_allocVector(VECSXP, ndims));
        n_protected++;
        for(idx_size = 0; idx_size < ndims; idx_size++ ){
            el = PROTECT(seq_len_int64(dim[idx_size]));
            SET_VECTOR_ELT(sliceIdx, idx_size, el);
            n_protected++;
        }
    }
    Rf_setAttrib(sliceIdx, Rf_install("_asis_"), Shield<SEXP>(wrap(true)));
    
    UNPROTECT(n_protected); // sliceIdx
    
    return( sliceIdx );
}

SEXP addCycle(SEXP x, SEXP ret, const R_xlen_t step, const R_xlen_t mag){
    int64_t* retptr = (int64_t*) REAL(ret);
    int64_t* xptr = (int64_t*) REAL(x);
    R_xlen_t retlen = Rf_xlength(ret);
    R_xlen_t xlen = Rf_xlength(x);
    
    if( retlen == 0 || xlen == 0 ){
        return(ret);
    }
    if( xlen > retlen ){
        stop("`addCycle` wrong length");
    }
    
    R_xlen_t ii = 0, jj = 0, kk = 0;
    int64_t v = 0;
    for(ii = 0; ii < retlen; ii += step){
        if( jj == xlen ){
            jj = 0;
            xptr = (int64_t*) REAL(x);
        }
        if( *xptr == NA_INTEGER64 ){
            for( kk = 0; kk < step; kk++ ){
                *retptr = NA_INTEGER64;
                retptr++;
            }
        } else {
            v = (*xptr - 1) * mag;
            for( kk = 0; kk < step; kk++ ){
                if( *retptr != NA_INTEGER64 ){
                    *retptr += v;
                }
                retptr++;
            }
        }
        xptr++;
        jj++;
    }
    
    return( ret );
}


// [[Rcpp::export]]
List schedule(const SEXP listOrEnv, 
              const NumericVector& dim,
              const NumericVector& cum_part_sizes,
              const int split_dim, const int strict){
    // std::vector<R_xlen_t> partitions(0);
    int n_protected = 0;
    
    
    SEXP sliceIdx = listOrEnv;
    if( Rf_getAttrib(sliceIdx, Rf_install("_asis_")) == R_NilValue ){
        sliceIdx = PROTECT(locationList(listOrEnv, dim, strict));
        n_protected++;
    }
    
    // split dim into 1:split_dim, split_dim+1: ndims
    R_xlen_t ndims = dim.length();
    if(split_dim+1 > ndims || split_dim < 1){
        stop("Invalid `split_dim`");
    }
    
    SEXP result_dim = PROTECT(Rf_allocVector(REALSXP, ndims));
    n_protected++;
    for(R_xlen_t d = 0; d < ndims; d++){
        *(REAL(result_dim) + d) = Rf_xlength(VECTOR_ELT(sliceIdx, d));
    }
    
    NumericVector dim1 = dim[seq(0, split_dim - 1)];
    double block_size = (prod_double(dim1));
    NumericVector dim2 = dim[seq(split_dim, ndims - 1)];
    R_xlen_t ndim2 = dim2.length();
    SEXP sliceIdx1 = PROTECT(Rf_allocVector(VECSXP, split_dim));
    SEXP sliceIdx2 = PROTECT(Rf_allocVector(VECSXP, ndims - split_dim));
    n_protected += 2;
    
    for(R_xlen_t ii = 0; ii < split_dim; ii++){
        SET_VECTOR_ELT(sliceIdx1, ii, VECTOR_ELT(sliceIdx, ii));
    }
    for(R_xlen_t ii = split_dim; ii < ndims; ii++){
        SET_VECTOR_ELT(sliceIdx2, ii - split_dim, VECTOR_ELT(sliceIdx, ii));
    }
    SEXP idx1 = PROTECT(loc2idx(sliceIdx1, dim1));
    n_protected++;
    List idx2s = List::create();
    
    SEXP last_dim = VECTOR_ELT(sliceIdx, ndims - 1);
    R_xlen_t last_len = Rf_xlength(last_dim);
    IntegerVector partitions = no_init(last_len);
    IntegerVector idx2lens = no_init(last_len);
    int64_t* lastptr = (int64_t*) REAL(last_dim);
    int64_t* lastptr2 = lastptr;
    int64_t* tmpptr;
    R_xlen_t l1 = 0, l2 = 0, part_size = 0;
    int part = 0, idx2len_cum= 0;
    for(R_xlen_t ii = 0; ii < last_len; ii++, lastptr++){
        if(*lastptr == NA_INTEGER64){
            l2++;
            continue;
        }
        if(cum_part_sizes[part] >= *lastptr && (part == 0 || cum_part_sizes[part-1] < *lastptr)){
            // still the same partition
            l2++;
            continue;
        }
        // change partition, flush!
        if( l2 > l1 ){
            SEXP tmp = PROTECT(Rf_allocVector(REALSXP, l2 - l1));
            n_protected++;
            Rf_setAttrib(tmp, R_ClassSymbol, Shield<SEXP>(wrap("integer64")));
            lastptr2 = ((int64_t*) REAL(last_dim)) + l1;
            tmpptr = (int64_t*) REAL(tmp);
            R_xlen_t part_start = part > 0 ? cum_part_sizes[part - 1] : 0;
            for(; l1 < l2; l1++, lastptr2++, tmpptr++){
                if(*lastptr2 == NA_INTEGER64){
                    *tmpptr = NA_INTEGER64;
                } else {
                    *tmpptr = (*lastptr2) - part_start;
                }
            }
            SET_VECTOR_ELT(sliceIdx2, ndim2 - 1, tmp);
            dim2[ndim2 - 1] = cum_part_sizes[part] - part_start;
            
            SEXP idx2tmp = PROTECT(loc2idx(sliceIdx2, dim2));
            n_protected++;
            idx2s.push_back(idx2tmp);
            
            // partitions.insert()
            // partitions.push_back(part);
            partitions[part_size] = part;
            idx2len_cum += Rf_xlength(idx2tmp);
            idx2lens[part_size] = idx2len_cum;
            part_size++;
        }
        part = 0;
        while(cum_part_sizes[part] < *lastptr || (part > 0 && cum_part_sizes[part-1] >= *lastptr)){
            part++;
        }
        l2++;
    }
    if( l2 > l1 ){
        SEXP tmp = PROTECT(Rf_allocVector(REALSXP, l2 - l1));
        n_protected++;
        Rf_setAttrib(tmp, R_ClassSymbol, Shield<SEXP>(wrap("integer64")));
        lastptr2 = ((int64_t*) REAL(last_dim)) + l1;
        tmpptr = (int64_t*) REAL(tmp);
        R_xlen_t part_start = part > 0 ? cum_part_sizes[part - 1] : 0;
        for(; l1 < l2; l1++, lastptr2++, tmpptr++){
            if(*lastptr2 == NA_INTEGER64){
                *tmpptr = NA_INTEGER64;
            } else {
                *tmpptr = (*lastptr2) - part_start;
            }
        }
        SET_VECTOR_ELT(sliceIdx2, ndim2 - 1, tmp);
        dim2[ndim2 - 1] = cum_part_sizes[part] - part_start;
        
        SEXP idx2tmp = PROTECT(loc2idx(sliceIdx2, dim2));
        n_protected++;
        idx2s.push_back(idx2tmp);
        // partitions.push_back(part);
        partitions[part_size] = part;
        idx2len_cum += Rf_xlength(idx2tmp);
        idx2lens[part_size] = idx2len_cum;
        part_size++;
        
    }
    if( part_size > 0 ){
        part_size--;
    }
    
    // get idx1 range
    SEXP idx1range = PROTECT(Rf_allocVector(REALSXP, 2));
    Rf_setAttrib(idx1range, R_ClassSymbol, Shield<SEXP>(wrap("integer64")));
    n_protected++;
    int64_t* idx1_start = (int64_t*) REAL(idx1range);
    int64_t* idx1_end = idx1_start + 1;
    *idx1_start = NA_INTEGER64;
    *idx1_end = -1;
    int64_t* ptr = (int64_t*) REAL(idx1); 
    R_xlen_t idx1len = Rf_xlength(idx1);
    R_xlen_t i = 0;
    for(ptr = (int64_t*) REAL(idx1), i = 0; i < idx1len; i++, ptr++ ){
        if( *ptr == NA_INTEGER64 ){
            continue;
        }
        if( *ptr < *idx1_start || *idx1_start == NA_INTEGER64 ){
            *idx1_start = *ptr;
        }
        if( *idx1_end < *ptr ){
            *idx1_end = *ptr;
        }
    }
    
    SEXP result_length = PROTECT(Rf_allocVector(INT64SXP, 1)); n_protected++;
    Rf_setAttrib(result_length, R_ClassSymbol, Shield<SEXP>(wrap("integer64")));
    *(INTEGER64(result_length)) = idx1len * idx2lens[part_size];
    
    List re = List::create(
        _["idx1"] = idx1,
        _["idx2s"] = idx2s,
        _["block_size"] = block_size,
        _["partitions"] = partitions[seq(0, part_size)],
        _["idx2lens"] = idx2lens[seq(0, part_size)],
        _["result_dim"] = result_dim,
        _["idx1range"] = idx1range,
        _["result_length"] = result_length
    );
    
    UNPROTECT(n_protected); // sliceIdx, result_dim, sliceIdx1, sliceIdx2, idx1, idx1range, (maybe) tmp, tmp1idx
    
    return(re);
}

SEXP seq_len_int64(const R_xlen_t len){
    SEXP tmp = PROTECT(Rf_allocVector(REALSXP, len));
    Rf_setAttrib(tmp, R_ClassSymbol, Shield<SEXP>(wrap("integer64")));
    int64_t* tmpptr = (int64_t*) REAL(tmp);
    for(R_xlen_t ii = 1; ii <= len; ii++, tmpptr++){
        *tmpptr = ii;
    }
    UNPROTECT(1);
    return tmp;
}

double prod_double(const NumericVector& x){
    if(x.length() == 0){ return(0.0); }
    double re = 1.0;
    re = std::accumulate(x.begin(), x.end(), re, std::multiplies<double>());
    return(re);
}
