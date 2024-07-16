#include "utils.h"
using namespace Rcpp;

int guess_splitdim(SEXP dim, int elem_size, size_t buffer_bytes){
    R_len_t ndims = Rf_length(dim);
    
    // calculate split_dim
    int ii;
    int dim_ii;
    double idx1len, idx2len, nloops, buffer_sz;
    double nops, min_ops = -1.0;
    int split_dim = 1;
    for(dim_ii = 1; dim_ii <= ndims - 1; dim_ii++){
        idx1len = 1.0;
        idx2len = 1.0;
        for(ii = 0; ii < dim_ii; ii++){
            idx1len *= *(REAL(dim) + ii);
        }
        for(ii = dim_ii; ii < ndims - 1; ii++ ){
            idx2len *= *(REAL(dim) + ii);
        }
        if( idx1len * elem_size - (double) buffer_bytes > 0.0 ){
            buffer_sz = (double) (buffer_bytes / elem_size);
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
    
    return( split_dim );
}

void set_buffer(SEXP dim, int elem_size, size_t buffer_bytes, int split_dim){
    int buf_bytes = elem_size;
    for(int ii = 0; ii < split_dim; ii++){
        buf_bytes *= (int) (*(REAL(dim) + ii));
        if( buf_bytes > buffer_bytes ){
            buf_bytes = buffer_bytes;
            break;
        }
    }
    if(buf_bytes == NA_INTEGER || buf_bytes <= 16){
        buf_bytes = 65536;
    }
    set_buffer_size(buf_bytes);
}

SEXPTYPE file_buffer_sxptype(SEXPTYPE array_type) {
    SEXPTYPE buf_sexp_type = array_type;
    switch(array_type) {
    case FLTSXP:
        buf_sexp_type = INTSXP;
        break;
    case LGLSXP:
        buf_sexp_type = RAWSXP;
        break;
    case CPLXSXP:
        buf_sexp_type = REALSXP;
        break;
    }
    return( buf_sexp_type );
}

SEXPTYPE array_memory_sxptype(SEXPTYPE array_type){
    if( array_type == FLTSXP ){ return (REALSXP); }
    return (array_type);
}

int file_element_size(SEXPTYPE array_type) {
    switch(array_type) {
    case FLTSXP:
        return(sizeof(float));
    case LGLSXP:
        return(sizeof(Rbyte));
    case CPLXSXP:
        return(sizeof(double));
    case REALSXP:
        return(sizeof(double));
    case INTSXP:
        return(sizeof(int));
    case RAWSXP:
        return(sizeof(Rbyte));
    default:
        stop("Unsupported sexptype");
    }
}
int memory_element_size(SEXPTYPE array_type) {
    switch(array_type) {
    case FLTSXP:
        return(sizeof(double));
    case LGLSXP:
        return(sizeof(int));
    case CPLXSXP:
        return(sizeof(Rcomplex));
    case REALSXP:
        return(sizeof(double));
    case INTSXP:
        return(sizeof(int));
    case RAWSXP:
        return(sizeof(Rbyte));
    default:
        stop("Unsupported sexptype");
    }
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

SEXP dropDimension(SEXP x){
    
    // Rf_DropDims mutates x, but since x is always internally created
    // and won't be referenced
    // return(Rf_DropDims(x));
    
    PROTECT(x);
    SEXP dims = Rf_getAttrib(x, R_DimSymbol);
    
    if (dims == R_NilValue) {
        UNPROTECT(1);
        return x;
    }
    
    int ndims = Rf_length(dims);
    int *dim = INTEGER(dims); 
    
    int i, n = 0;
    for (i = 0; i < ndims; i++) {
        if (dim[i] != 1) n++;
    }
    if (n == ndims) {
        UNPROTECT(1);
        return x;
    }
        
    SEXP dimnames = PROTECT(Rf_getAttrib(x, R_DimNamesSymbol));
    SEXP newnames = R_NilValue;
    if (n <= 1) {
        if (dimnames != R_NilValue) {
            if(XLENGTH(x) != 1) {
                for (i = 0; i < ndims; i++) {
                    if (dim[i] != 1) {
                        newnames = VECTOR_ELT(dimnames, i);
                        break;
                    }
                }
            } else { /* drop all dims: keep names if unambiguous */
                int cnt = 0;
                for(i = 0; i < ndims; i++) {
                    if(VECTOR_ELT(dimnames, i) != R_NilValue) cnt++;
                }
                if(cnt == 1) {
                    for (i = 0; i < ndims; i++) {
                        newnames = VECTOR_ELT(dimnames, i);
                        if(newnames != R_NilValue) break;
                    }
                }
            }
        }
        PROTECT(newnames);
        Rf_setAttrib(x, R_DimNamesSymbol, R_NilValue);
        Rf_setAttrib(x, R_DimSymbol, R_NilValue);
        Rf_setAttrib(x, R_NamesSymbol, newnames);
        UNPROTECT(1);
    } else {
        // We have a lower dimensional array, and  n == length(newdims)
        SEXP newdims, dnn, newnamesnames = R_NilValue;
        PROTECT(dnn = Rf_getAttrib(dimnames, R_NamesSymbol));
        PROTECT(newdims = Rf_allocVector(INTSXP, n));
        for (i = 0, n = 0; i < ndims; i++) {
            if (dim[i] != 1) {
                INTEGER(newdims)[n++] = dim[i];
            }
        }
        
        if(!Rf_isNull(Rf_getAttrib(dims, R_NamesSymbol))) {
            SEXP new_nms = PROTECT(Rf_allocVector(STRSXP, n));
            SEXP nms_d = Rf_getAttrib(dims, R_NamesSymbol);
            for (i = 0, n = 0; i < ndims; i++) {
                if (dim[i] != 1) {
                    SET_STRING_ELT(new_nms, n++, STRING_ELT(nms_d, i));
                }
            }
            Rf_setAttrib(newdims, R_NamesSymbol, new_nms);
            UNPROTECT(1);
        }
        Rboolean havenames = FALSE;
        if (!Rf_isNull(dimnames)) {
            for (i = 0; i < ndims; i++) {
                if (dim[i] != 1 && VECTOR_ELT(dimnames, i) != R_NilValue) {
                    havenames = TRUE;
                }
            }
            if (havenames) {
                PROTECT(newnames = Rf_allocVector(VECSXP, n));
                PROTECT(newnamesnames = Rf_allocVector(STRSXP, n));
                for (i = 0, n = 0; i < ndims; i++) {
                    if (dim[i] != 1) {
                        if(!Rf_isNull(dnn)) {
                            SET_STRING_ELT(newnamesnames, n, STRING_ELT(dnn, i));
                        }
                        SET_VECTOR_ELT(newnames, n++, VECTOR_ELT(dimnames, i));
                    }
                }
            } else {
                dimnames = R_NilValue;
            }
        }
        Rf_setAttrib(x, R_DimNamesSymbol, R_NilValue);
        Rf_setAttrib(x, R_DimSymbol, newdims);
        if (havenames) {
            if(!Rf_isNull(dnn)) {
                Rf_setAttrib(newnames, R_NamesSymbol, newnamesnames);
            }
            Rf_setAttrib(x, R_DimNamesSymbol, newnames);
            UNPROTECT(2);
        }
        UNPROTECT(2);
    }
    UNPROTECT(2);
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





SEXP subset_dimnames(SEXP dimnames, SEXP sliceIdx){
    if( TYPEOF(dimnames) != VECSXP ) {
        stop("`subset_dimnames` dimnames must be a list");
    }
    int ndims = Rf_length(dimnames);
    
    if( ndims > Rf_length(sliceIdx) ){
        stop("`subset_dimnames` dimnames is larger than array margins?");
    }
    for(int ii = 0; ii < ndims; ii++){
        SEXP dn = VECTOR_ELT(dimnames, ii);
        if( dn == R_NilValue ){
            continue;
        }
        SEXP idx = VECTOR_ELT(sliceIdx, ii);
        SEXP sub_el = PROTECT(sub_vec(dn, idx));
        SET_VECTOR_ELT(dimnames, ii, sub_el);
        UNPROTECT(1);
    }
    return(dimnames);
}


// re = x[min_ + seq_len(len_)]
SEXP sub_vec_range(SEXP x, const R_xlen_t& min_, const R_xlen_t& len_){
    if(min_ < 0){
        stop("`sub_vec_range` invalid min index");
    }
    if(len_ + min_ > Rf_xlength(x)){
        stop("`sub_vec_range` invalid length");
    }
    SEXPTYPE xtype = TYPEOF(x);
    
    switch(xtype) {
    case INTSXP: {
        SEXP ret = PROTECT(Rf_allocVector(xtype, len_));
        memcpy(INTEGER(ret), INTEGER(x) + min_, len_ * sizeof(int));
        UNPROTECT(1);
        return(ret);
    }
    case REALSXP: {
        SEXP ret = PROTECT(Rf_allocVector(xtype, len_));
        memcpy(REAL(ret), REAL(x) + min_, len_ * sizeof(double));
        UNPROTECT(1);
        return(ret);
    }
    case CPLXSXP: {
        SEXP ret = PROTECT(Rf_allocVector(xtype, len_));
        memcpy(COMPLEX(ret), COMPLEX(x) + min_, len_ * sizeof(Rcomplex));
        UNPROTECT(1);
        return(ret);
    }
    case RAWSXP: {
        SEXP ret = PROTECT(Rf_allocVector(xtype, len_));
        memcpy(RAW(ret), RAW(x) + min_, len_ * sizeof(Rbyte));
        UNPROTECT(1);
        return(ret);
    }
    case LGLSXP: {
        SEXP ret = PROTECT(Rf_allocVector(xtype, len_));
        memcpy(LOGICAL(ret), LOGICAL(x) + min_, len_ * sizeof(int));
        UNPROTECT(1);
        return(ret);
    }
    default: {
        stop("Unsupported xtype");
    }
    };
}

SEXP sub_vec(SEXP x, SEXP idx_int64){
    R_xlen_t xlen = Rf_xlength(x);
    R_xlen_t idxlen = Rf_xlength(idx_int64);
    int64_t* idxptr = INTEGER64(idx_int64);
    
    SEXPTYPE xtype = TYPEOF(x);
    
    switch(xtype) {
    case INTSXP: {
        SEXP ret = PROTECT(Rf_allocVector(xtype, idxlen));
        int* retptr = INTEGER(ret);
        for(R_xlen_t ii = 0; ii < idxlen; ii++, idxptr++, retptr++){
            if(*idxptr == NA_INTEGER64 || *idxptr <= 0 || *idxptr > xlen){
                *retptr = NA_INTEGER;
            } else {
                *retptr = *(INTEGER(x) + (*idxptr - 1));
            }
        }
        UNPROTECT(1);
        return( ret );
    }
    case REALSXP: {
        SEXP ret = PROTECT(Rf_allocVector(xtype, idxlen));
        double* retptr = REAL(ret);
        for(R_xlen_t ii = 0; ii < idxlen; ii++, idxptr++, retptr++){
            if(*idxptr == NA_INTEGER64 || *idxptr <= 0 || *idxptr > xlen){
                *retptr = NA_REAL;
            } else {
                *retptr = *(REAL(x) + (*idxptr - 1));
            }
        }
        UNPROTECT(1);
        return( ret );
    }
    case CPLXSXP: {
        SEXP ret = PROTECT(Rf_allocVector(xtype, idxlen));
        Rcomplex* retptr = COMPLEX(ret);
        for(R_xlen_t ii = 0; ii < idxlen; ii++, idxptr++, retptr++){
            if(*idxptr == NA_INTEGER64 || *idxptr <= 0 || *idxptr > xlen){
                retptr->i = NA_REAL;
                retptr->r = NA_REAL;
            } else {
                *retptr = *(COMPLEX(x) + (*idxptr - 1));
            }
        }
        UNPROTECT(1);
        return( ret );
    }
    case RAWSXP: {
        SEXP ret = PROTECT(Rf_allocVector(xtype, idxlen));
        Rbyte* retptr = RAW(ret);
        for(R_xlen_t ii = 0; ii < idxlen; ii++, idxptr++, retptr++){
            if(*idxptr == NA_INTEGER64 || *idxptr <= 0 || *idxptr > xlen){
                *retptr = 0;
            } else {
                *retptr = *(RAW(x) + (*idxptr - 1));
            }
        }
        UNPROTECT(1);
        return( ret );
    }
    case STRSXP: 
    default: {
        SEXP x_ = x;
        int nprot = 0;
        if( xtype != STRSXP )
        {
            x_ = PROTECT(Rf_coerceVector(x, STRSXP));
            nprot = 1;
        }
        StringVector ret = StringVector(idxlen);
        StringVector::iterator retptr = ret.begin();
        for(R_xlen_t ii = 0; ii < idxlen; ii++, idxptr++, retptr++){
            if(*idxptr == NA_INTEGER64 || *idxptr <= 0 || *idxptr > xlen){
                *retptr = NA_STRING;
            } else {
                *retptr = STRING_ELT(x_, *idxptr - 1);
            }
        }
        if(nprot > 0){
            UNPROTECT( nprot );
        }
        return( ret );
    }
    };
    
    // SEXP ret = 
    return(R_NilValue);
}


