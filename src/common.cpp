#include "common.h"
using namespace Rcpp;

// [[Rcpp::export]]
SEXP check_missing_dots(const SEXP env){
    if( TYPEOF(env) != ENVSXP ){
        Rcpp::stop("`check_missing_dots` is asking for an environment");
    }
    SEXP dots = Rf_findVarInFrame(env, R_DotsSymbol);
    
    std::vector<bool> is_missing(0);
    
    if( dots != R_NilValue ){
        SEXP el = R_NilValue;
        
        for(; (dots != R_NilValue) && (dots != R_MissingArg); dots = CDR(dots) ){
            el = CAR(dots);
            if( el == R_MissingArg ){
                is_missing.push_back(true);
            } else {
                is_missing.push_back(false);
            }
        }
    }
    
    return(Rcpp::wrap(is_missing));
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

// [[Rcpp::export]]
int kinda_sorted(SEXP idx, int64_t min_, int64_t buffer_count){
    int64_t* ptr = (int64_t*) REAL(idx);
    int64_t lb = min_;
    
    // int64_t buffer_count = nbuffers / elem_size;
    int64_t ub = lb + buffer_count;
    for(R_xlen_t ii = 0; ii < Rf_xlength(idx); ii++, ptr++){
        if( *ptr == NA_INTEGER64 ){ continue; }
        
        if( lb == NA_INTEGER64 ){
            lb = *ptr;
            ub = lb + buffer_count;
        }
        
        if(*ptr < lb){
            return(0);
        }
        if(*ptr >= ub){
            while(*ptr >= ub){
                lb = ub;
                ub = lb + buffer_count;
            }
        }
    }
    return(1);
}

bool isLittleEndian(){
    int x = 1;
    bool is_little = *((char*)&x) == 1;
    return ( is_little );
    // DEBUG test big endianess
    // return(!is_little);
}

double prod_double(const NumericVector& x){
    if(x.length() == 0){ return(0.0); }
    double re = 1.0;
    re = std::accumulate(x.begin(), x.end(), re, std::multiplies<double>());
    return(re);
}

// [[Rcpp::export]]
SEXP realToUint64(NumericVector x, const double min_, const double max_, const int strict){
    R_xlen_t len = x.length();
    SEXP re = PROTECT(Rf_allocVector(REALSXP, len));
    Rf_setAttrib(re, R_ClassSymbol, wrap("integer64"));
    
    int64_t *reptr = (int64_t *) REAL(re);
    
    NumericVector::iterator xptr = x.begin();
    
    for(; xptr != x.end(); xptr++, reptr++ ){
        if( ISNAN(*xptr) ){
            *reptr = NA_INTEGER64;
            continue;
        }
        if( *xptr < min_ || *xptr > max_ ){
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

SEXP seq_len_int64(const R_xlen_t len){
    SEXP tmp = PROTECT(Rf_allocVector(REALSXP, len));
    Rf_setAttrib(tmp, R_ClassSymbol, wrap("integer64"));
    int64_t* tmpptr = (int64_t*) REAL(tmp);
    for(R_xlen_t ii = 1; ii <= len; ii++, tmpptr++){
        *tmpptr = ii;
    }
    UNPROTECT(1);
    return tmp;
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
    case VECSXP:
        sliceIdx = listOrEnv;
        idx_size = Rf_xlength(sliceIdx);
        break;
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
                    el = PROTECT(realToUint64(as<NumericVector>(el), 1, dl, strict));
                    n_protected++;
                }
                SET_VECTOR_ELT(sliceIdx, idx_size, el);
            } else {
                if( el == R_MissingArg || el == R_NilValue ){
                    el = PROTECT(seq_len_int64( dl ));
                    n_protected++;
                } else {
                    el = PROTECT(realToUint64(as<NumericVector>(el), 1, dl, strict));
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
    
    
    UNPROTECT(n_protected); // sliceIdx
    
    return( sliceIdx );
}

SEXP dropDimension(SEXP x){
    SEXP dim = Rf_getAttrib(x, R_DimSymbol);
    if(dim == R_NilValue){
        return x;
    }
    SEXP new_dim;
    R_xlen_t ndims = Rf_xlength(dim);
    R_xlen_t xlen = Rf_xlength(x);
    if(ndims == 0){
        new_dim = R_NilValue;
        Rf_setAttrib(x, R_DimSymbol, new_dim);
        return x;
    }
    if(xlen == 0){
        return x;
    }
    
    R_xlen_t ii;
    
    new_dim = PROTECT(Rf_allocVector(TYPEOF(dim), ndims));
    
    switch(TYPEOF(dim)){
    case INTSXP: {
        int *ptr_orig = INTEGER(dim);
        int *ptr_new = INTEGER(new_dim);
        for(ii = 0; ptr_orig != INTEGER(dim) + ndims; ptr_orig++ ){
            if(*ptr_orig > 1){
                *ptr_new++ = *ptr_orig;
                ii++;
            }
        }
        break;
    }
    case REALSXP: {
        double *ptr_orig = REAL(dim);
        double *ptr_new = REAL(new_dim);
        for(ii = 0; ptr_orig != REAL(dim) + ndims; ptr_orig++ ){
            if(*ptr_orig > 1){
                *ptr_new++ = *ptr_orig;
                ii++;
            }
        }
        break;
    }
    default:
        stop("unknown dimension storage type");
    }
    if(ii == ndims){} else if(ii >= 2){
        SETLENGTH(new_dim, ii);
        
        Rf_setAttrib(x, R_DimSymbol, new_dim);
    } else {
        Rf_setAttrib(x, R_DimSymbol, R_NilValue);
    }
    
    UNPROTECT(1);
    return x;
}

int64_t prod2(SEXP x, bool na_rm){
    SEXP x_alt = x;
    
    int n_protected = 0;
    
    if(TYPEOF(x_alt) != REALSXP){
        x_alt = PROTECT(Rf_coerceVector(x_alt, REALSXP));
        n_protected++;
    }
    int64_t res = 1;
    R_xlen_t xlen = Rf_xlength(x) - 1;
    for(; xlen >= 0; xlen-- ){
        int64_t tmp = REAL(x_alt)[xlen];
        if(tmp == NA_REAL || tmp == NA_INTEGER64){
            if(!na_rm){
                res = NA_INTEGER64;
                break;
            }
        } else {
            res *= REAL(x_alt)[xlen];
        }
    }
    
    if( n_protected > 0 ){
        UNPROTECT(n_protected);
    }
    
    return res;
}

// [[Rcpp::export]]
SEXP reshape_or_drop(SEXP x, SEXP reshape, bool drop){
    // SEXP reshape, bool drop = false
    // if reshape is not null, drop is ignored
    if(reshape == R_NilValue && !drop){
        return x;
    }
    
    if(reshape == R_NilValue && drop){
        dropDimension(x);
        return x;
    }
    
    // reshape has length, hence need to check dimension length
    
    // subset_mode=0 => x[i,j,k]
    // subset_mode=1 => x[i]
    // subset_mode=2 => x[]
    SEXP reshape_alt = reshape;
    int n_protected = 0;
    if(TYPEOF(reshape) != REALSXP){
        reshape_alt = PROTECT(Rf_coerceVector(reshape_alt, REALSXP));
        n_protected++;
    }
    const int64_t reshape_length = prod2(reshape_alt, false);
    const int64_t expected_length = Rf_xlength(x);
    
    if(reshape_length == NA_INTEGER64 || reshape_length != expected_length){
        warning("`reshape` has different length than expected. Request to reshape dimension is ignored.");
    } else {
        if(Rf_xlength(reshape_alt) >= 2){
            Rf_setAttrib(x, R_DimSymbol, reshape_alt);
        } else {
            Rf_setAttrib(x, R_DimSymbol, R_NilValue);
        }
    }
    
    if(n_protected > 0){
        UNPROTECT(n_protected);
    }
    
    return x;
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
    Rf_setAttrib(re, R_ClassSymbol, wrap("integer64"));
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
List schedule(const SEXP listOrEnv, 
              const NumericVector& dim,
              const NumericVector& cum_part_sizes,
              const int split_dim, const int strict){
    // std::vector<R_xlen_t> partitions(0);
    int n_protected = 0;
    
    SEXP sliceIdx = PROTECT(locationList(listOrEnv, dim, strict));
    n_protected++;
    
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
            Rf_setAttrib(tmp, R_ClassSymbol, wrap("integer64"));
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
        Rf_setAttrib(tmp, R_ClassSymbol, wrap("integer64"));
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
    
    List re = List::create(
        _["idx1"] = idx1,
        _["idx2s"] = idx2s,
        _["block_size"] = block_size,
        _["partitions"] = partitions[seq(0, part_size)],
        _["idx2lens"] = idx2lens[seq(0, part_size)],
        _["result_dim"] = result_dim
    );
    
    UNPROTECT(n_protected); // sliceIdx, result_dim, sliceIdx1, sliceIdx2, idx1, (maybe) tmp, tmp1idx
    
    return(re);
}

int system_buffer_size(){
    return(BUFSIZ);
}

void swap_endianess(void *ptr, size_t size, size_t nmemb){
    unsigned char *buffer_src = (unsigned char*)ptr;
    unsigned char *buffer_dst = new unsigned char[size];
    size_t ix = 0;
    for (size_t i = 0; i < nmemb; i++, buffer_src += size) {
        for (ix = 0; ix < size; ix++) {
            *(buffer_dst + (size - 1 - ix)) = *(buffer_src + ix);
        }
        memcpy(buffer_src, buffer_dst, size);
    }
    delete[] buffer_dst;
}

size_t lendian_fread(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    const size_t len = fread(ptr, size, nmemb, stream);
    if( !isLittleEndian() ){
        // little endian to big endian
        swap_endianess(ptr, size, nmemb);
    }
    return( len );
}

size_t lendian_fwrite(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    if( !isLittleEndian() ){
        // big endian to little endian
        swap_endianess(ptr, size, nmemb);
    }
    return( fwrite(ptr, size, nmemb, stream) );
}



/*** R
(function(...){ check_missing_dots(environment()) })(,,1)
*/
