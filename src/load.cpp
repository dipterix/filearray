#include "openmp.h"
#include "common.h"
#include "core.h"
#include "load.h"
using namespace Rcpp;

SEXPTYPE get_read_type(SEXPTYPE type){
    if( type == FLTSXP ){ return (REALSXP); }
    return (type);
}

int get_buffer_nelem(SEXPTYPE type){
    int buffer_bytes = get_buffer_size();
    switch(type){
    case INTSXP:
        return( buffer_bytes / sizeof(int) );
    case REALSXP:
        return( buffer_bytes / sizeof(double) );
    case RAWSXP:
        return( buffer_bytes );
    case FLTSXP:
        return( buffer_bytes / sizeof(double) );
    case LGLSXP:
        return( buffer_bytes / sizeof(int) );
    case CPLXSXP:
        return( buffer_bytes / sizeof(Rcomplex) );
    default:
        stop("Unsupported SEXP type");
    }
}


/**********************************************************
 * Transform functions
 ***********************************************************/

template <typename T>
void transform_asis(const T* x, T* y){
    *y = *x;
}
void transform_float(const float* x, double* y){
    *y = *x;
}
void transform_logical(const Rbyte* x, int* y){
    if(*x == 0){
        *y = FALSE;
    } else if (*x == 1){
        *y = TRUE;
    } else {
        *y = NA_LOGICAL;
    }
}
void transform_cplx(const double* x, Rcomplex* y){
    y->r = *((float*) x);
    y->i = *(((float*) x) + 1);
    if( ISNAN(y->r) || ISNAN(y->i) ){
        y->r = NA_REAL;
        y->i = NA_REAL;
    }
}

/**********************************************************
 * Read partition
 ***********************************************************/

template <typename T,  typename B>
inline void subset_partition(
        FILE* conn, B* buffer, int buffer_size, 
        T* retptr, const R_xlen_t block_size, 
        SEXP idx1, int64_t idx1_start, int64_t idx1_end,
        SEXP idx2, int64_t idx2_start, int64_t idx2_end,
        int idx1_sorted, int idx2_sorted,
        void (*transform) (const B*, T*)
) {
    double content_size = 0;
    int elem_size = sizeof(B);
    // R_xlen_t buffer_size = buffer_bytes / elem_size;
    if( buffer_size > block_size ){
        buffer_size = block_size;
    }
    B* bufferptr = (B*) buffer;
    B* bufferptr2 = bufferptr;
    
    fseek(conn, FARR_HEADER_LENGTH - 8, SEEK_SET);
    lendian_fread(&(content_size), 8, 1, conn);
    
    int64_t start_idx = idx1_start;
    int64_t end_idx = 0;
    int64_t conn_pos = 0;
    
    int64_t* idx1ptr = (int64_t*) REAL(idx1);
    R_xlen_t idx1len = Rf_xlength(idx1);
    
    int64_t* idx2ptr = (int64_t*) REAL(idx2);
    R_xlen_t idx2len = Rf_xlength(idx2);
    
    R_xlen_t ii = 0, jj = 0, ll = 0, ii_idx1 = 0;
    T* retptr2 = retptr;
    T* retptr3 = retptr;
    
    // Rcout << idx2_start << "---\n";
    
    for(int64_t block = idx2_start; block <= idx2_end; block++){
        // find block in idx2
        for(ii_idx1 = 0, idx2ptr = (int64_t*) REAL(idx2);
            ii_idx1 < idx2len; ii_idx1++, idx2ptr++){
            if( *idx2ptr == block ){
                break;
            }
        }
        if( *idx2ptr != block ){ continue; }
        
        // Rcout << block << "\n";
        
        // read current block!
        retptr2 = retptr + ii_idx1 * idx1len;
        start_idx = (idx1_start + block_size * block);
        end_idx = start_idx - idx1_start + idx1_end + 1;
        
        if( start_idx >= content_size ){
            if( idx2_sorted ){
                break;
            }
            continue;
        }
        if( end_idx > content_size ){
            end_idx = content_size;
        }
        
        // Rcout << block << " " << ii_idx1 <<  " " << start_idx <<  " " << end_idx << "\n";
        
        // fseek is somehow slow, read to buffer without using it seems faster
        while( conn_pos < start_idx ){
            ii = start_idx - conn_pos;
            ii = ii > buffer_size ? buffer_size : ii;
            lendian_fread(bufferptr, elem_size, ii, conn);
            conn_pos += ii;
        }
        // fseek(conn, FARR_HEADER_LENGTH - 8, SEEK_SET);
        // fseek(conn, (start_idx - conn_pos) * elem_size, SEEK_CUR);
        // conn_pos = start_idx;
        
        
        idx1ptr = (int64_t*) REAL(idx1);
        jj = 0;
        while( conn_pos < end_idx ){
            ii = end_idx - conn_pos;
            ii = ii > buffer_size ? buffer_size : ii;
            lendian_fread(bufferptr, elem_size, ii, conn);
            
            if( !idx1_sorted ){
                idx1ptr = (int64_t*) REAL(idx1);
                jj = 0;
            }
            for(; jj < idx1len; jj++, idx1ptr++) {
                if(*idx1ptr == NA_INTEGER64){ continue; }
                // ll should be [conn_pos, conn_pos + ii)
                
                ll = *idx1ptr - idx1_start + start_idx - conn_pos;
                if( ll < 0 ){ continue; }
                if( ll >= ii ){
                    if( idx1_sorted ) {
                        break;
                    }
                    continue;
                }
                bufferptr2 = bufferptr + ll;
                transform(bufferptr2, retptr2 + jj);
                // *(retptr2 + jj) = (T) *(bufferptr + ll);
            }
            
            conn_pos += ii;
        }
        
        retptr2 = retptr + ii_idx1 * idx1len;
        ii_idx1++;
        idx2ptr = ((int64_t*) REAL(idx2)) + ii_idx1;
        // Rcout << "1\n";
        for(; ii_idx1 < idx2len; ii_idx1++, idx2ptr++){
            if( *idx2ptr == block ){
                retptr3 = retptr + ii_idx1 * idx1len;
                memcpy(retptr3, retptr2, sizeof(T) * idx1len);
            } else if( idx2_sorted && *idx2ptr > block ){
                break;
            }
        }
        // Rcout << "2\n";
    }
}

/**********************************************************
 * Subset - internal (multithread here)
 ***********************************************************/


template <typename T, typename B>
bool FARR_subset_template(
        const std::string& filebase, 
        const List& sch,
        T* ret_ptr, const T na, const R_xlen_t& retlen,
        std::vector<B*> buffer_ptrs, const int& buffer_nelems,
        void (*transform)(const B*, T*)
){
    SEXP idx1 = sch["idx1"];
    SEXP idx1range = sch["idx1range"];
    List idx2s = sch["idx2s"];
    int64_t block_size = (int64_t) (sch["block_size"]);
    IntegerVector partitions = sch["partitions"];
    IntegerVector idx2lens = sch["idx2lens"];
    
    R_xlen_t niter = partitions.length();
    R_xlen_t idx1len = Rf_xlength(idx1);
    
    // // TODO: change
    // SEXP ret = PROTECT(Rf_allocVector(INTSXP, idx1len * idx2lens[niter - 1]));
    // // TODO: change
    // const int na = NA_INTEGER;
    
    int64_t* idx1rangeptr = (int64_t*) REAL(idx1range);
    int64_t idx1_start = *idx1rangeptr, idx1_end = *(idx1rangeptr + 1);
    
    if( idx1_start == NA_INTEGER64 || idx1_end < 0 || idx1_start < 0 ){
        // idx1 are all NAs, no need to subset, return NA
        
        T* retptr = ret_ptr;
        for(R_xlen_t jj = 0; jj < retlen; jj++){
            *retptr++ = na;
        }
        return(false);
    }
    
    const int idx1_sorted = kinda_sorted(idx1, idx1_start, buffer_nelems);
    
    int err = -1;
    // char* buffer[nbuffers];
    
    int ncores = buffer_ptrs.size();
    if(ncores > niter){
        ncores = niter;
    }
    
#pragma omp parallel num_threads(ncores) 
{
#pragma omp for schedule(static, 1) nowait
    for(R_xlen_t ii = 0; ii < niter; ii++){
        // get current buffer
        int thread = ii % ncores;
        
        int part = partitions[ii];
        int64_t skips = 0;
        if(ii > 0){
            skips = idx2lens[ii - 1];
        }
        int64_t idx2len = idx2lens[ii] - skips;
        
        // TODO: change
        T* retptr = ret_ptr + skips * idx1len;
        for(R_xlen_t jj = 0; jj < idx2len * idx1len; jj++, retptr++ ){
            *retptr = na;
        }
        
        // TODO: change
        retptr = ret_ptr + skips * idx1len;
        
        SEXP idx2 = idx2s[ii];
        int64_t idx2_start = NA_INTEGER64, idx2_end = -1;
        int64_t* ptr2 = (int64_t*) REAL(idx1); 
        for(ptr2 = (int64_t*) REAL(idx2); idx2len > 0; idx2len--, ptr2++ ){
            if( *ptr2 == NA_INTEGER64 ){
                continue;
            }
            if( *ptr2 < idx2_start || idx2_start == NA_INTEGER64 ){
                idx2_start = *ptr2;
            }
            if( idx2_end < *ptr2 ){
                idx2_end = *ptr2;
            }
        }
        
        if( idx2_start == NA_INTEGER64 || idx2_end < 0 || idx2_start < 0 ){
            // This is NA partition, no need to subset
            continue;
        }
        
        const int idx2_sorted = kinda_sorted(idx2, idx2_start, 1);
        std::string file = filebase + std::to_string(part) + ".farr";
        
        FILE* conn = fopen( file.c_str(), "rb" );
        if (conn) {
            
            std::string s = "";
            
            // TODO: change
            // int* buffer = INTEGER(buf);
            B* buffer = buffer_ptrs[thread];
            
            try{
                subset_partition(conn, buffer, buffer_nelems, 
                                 retptr, block_size,
                                 idx1, idx1_start, idx1_end,
                                 idx2, idx2_start, idx2_end,
                                 idx1_sorted, idx2_sorted,
                                 transform);
                // subset_partition(conn, buffer, nbuffers, retptr, block_size, 
                //                  idx1, idx1_start, idx1_end,
                //                  idx2, idx2_start, idx2_end,
                //                  0, idx2_sorted);
            } catch(...){
                fclose(conn);
                conn = NULL;
                err = part;
            }
            if( conn != NULL ){
                fclose(conn);
            }
        }
    }
}
    return(true);
}

SEXP FARR_subset(const std::string& filebase, 
                 const List& sch,
                 const SEXPTYPE type,
                 std::vector<SEXP>& buffer_pool,
                 SEXP ret){
    std::string fbase = correct_filebase(filebase);
    
    // SEXP idx1 = sch["idx1"];
    // IntegerVector partitions = sch["partitions"];
    // IntegerVector idx2lens = sch["idx2lens"];
    
    // R_xlen_t niter = partitions.length();
    
    // R_xlen_t idx1len = Rf_xlength(idx1);
    // R_xlen_t retlen = idx1len * idx2lens[niter - 1];
    // 
    // SEXPTYPE ret_type = get_read_type(type);
    // SEXP ret = PROTECT(Rf_allocVector(ret_type, retlen));
    
    R_xlen_t retlen = Rf_xlength(ret);
    SEXP result_dim = sch["result_dim"];
    Rf_setAttrib(ret, R_DimSymbol, result_dim);
    
    // SEXPTYPE buffer_type = file_buffer_sxptype(type);
    // int buffer_nelems = get_buffer_nelem(type);
    int ncores = buffer_pool.size();
    if( ncores < 1 ){
        stop("Thread number and buffer pool size must be positive.");
    }
    int buffer_nelems = Rf_length(buffer_pool[0]);
    // std::vector<SEXP> buffer_pool(ncores);
    // for(int ii = 0; ii < ncores; ii++){
    //     buffer_pool[ii] = PROTECT(Rf_allocVector(buffer_type, buffer_nelems));
    // }
    
    switch(type){
    case INTSXP: {
        std::vector<int*> buffer_ptrs(ncores);
        for(int ii = 0; ii < ncores; ii++){
            buffer_ptrs[ii] = INTEGER(buffer_pool[ii]);
        }
        FARR_subset_template(
            fbase, sch, INTEGER(ret), NA_INTEGER, retlen,
            buffer_ptrs, buffer_nelems, 
            &transform_asis);
        break;
    }
    case REALSXP: {
        std::vector<double*> buffer_ptrs(ncores);
        for(int ii = 0; ii < ncores; ii++){
            buffer_ptrs[ii] = REAL(buffer_pool[ii]);
        }
        FARR_subset_template(
            fbase, sch, REAL(ret), NA_REAL, retlen,
            buffer_ptrs, buffer_nelems, 
            &transform_asis);
        break;
    }
    case FLTSXP: {
        std::vector<float*> buffer_ptrs(ncores);
        for(int ii = 0; ii < ncores; ii++){
            buffer_ptrs[ii] = FLOAT(buffer_pool[ii]);
        }
        // Rcout << "1\n";
        // REAL(ret);
        // Rcout << "2\n";
        FARR_subset_template(
            fbase, sch, REAL(ret), NA_REAL, retlen,
            buffer_ptrs, buffer_nelems, 
            &transform_float);
        break;
    }
    case RAWSXP: {
        std::vector<Rbyte*> buffer_ptrs(ncores);
        for(int ii = 0; ii < ncores; ii++){
            buffer_ptrs[ii] = RAW(buffer_pool[ii]);
        }
        Rbyte na_byte = 2;
        FARR_subset_template(
            fbase, sch, RAW(ret), na_byte, retlen,
            buffer_ptrs, buffer_nelems, 
            &transform_asis);
        break;
    }
    case LGLSXP: {
        std::vector<Rbyte*> buffer_ptrs(ncores);
        for(int ii = 0; ii < ncores; ii++){
            buffer_ptrs[ii] = RAW(buffer_pool[ii]);
        }
        FARR_subset_template(
            fbase, sch, LOGICAL(ret), NA_LOGICAL, retlen,
            buffer_ptrs, buffer_nelems, 
            &transform_logical);
        break;
    }
    case CPLXSXP: {
        std::vector<double*> buffer_ptrs(ncores);
        for(int ii = 0; ii < ncores; ii++){
            buffer_ptrs[ii] = REAL(buffer_pool[ii]);
        }
        na_cplx_dbl();
        Rcomplex na_cplx;
        na_cplx.i = NA_REAL;
        na_cplx.r = NA_REAL;
        FARR_subset_template(
            fbase, sch, COMPLEX(ret), na_cplx, retlen,
            buffer_ptrs, buffer_nelems, 
            &transform_cplx);
        break;
    }
    default:
        stop("Unsupported SEXP type");
    }
    
    return(ret);
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
    List meta = FARR_meta(fbase);
    const int elem_size = meta["elem_size"];
    const SEXPTYPE sexp_type = meta["sexp_type"];
    SEXP dim = meta["dimension"]; // double
    SEXP cum_part_size = meta["cumsum_part_sizes"];
    
    R_len_t ndims = Rf_length(dim);
    
    // calculate split_dim
    if( split_dim == NA_INTEGER || split_dim == 0 ){
        split_dim = guess_splitdim(dim, elem_size, thread_buffer);
    } else if (split_dim < 1 || split_dim > ndims-1 ){
        stop("Incorrect `split_dim`: must be an integer from 1 to ndims-1 ");
    }
    set_buffer(dim, elem_size, thread_buffer, split_dim);
    
    // get dimnames
    SEXP dnames = R_NilValue;
    SEXP sliceIdx = PROTECT(locationList(listOrEnv, dim, 1));
    
    if( use_dimnames ){
        dnames = meta["dimnames"];
        if( TYPEOF(dnames) == VECSXP && Rf_length(dnames) == ndims ){
            subset_dimnames(dnames, sliceIdx);
        }
    }
    
    // schedule indices
    List sch = schedule(sliceIdx, dim, cum_part_size, split_dim, strict);
    
    int ncores = getThreads();
    SEXPTYPE buffer_type = file_buffer_sxptype(sexp_type);
    int buffer_nelems = get_buffer_nelem(sexp_type);
    std::vector<SEXP> buffer_pool(ncores);
    for(int ii = 0; ii < ncores; ii++){
        buffer_pool[ii] = PROTECT(Rf_allocVector(buffer_type, buffer_nelems));
    }
    
    // allocate for returns
    int64_t retlen = *INTEGER64(sch["result_length"]);
    // const SEXP idx1 = sch["idx1"];
    // const IntegerVector idx2lens = sch["idx2lens"];
    // R_xlen_t idx1len = Rf_xlength(idx1);
    // R_xlen_t retlen = idx1len * idx2lens[Rf_length(cum_part_size) - 1];
    // 
    SEXPTYPE ret_type = get_read_type(sexp_type);
    SEXP res = PROTECT(Rf_allocVector(ret_type, retlen));
    
    FARR_subset(fbase, sch, sexp_type, buffer_pool, res);
    if( dnames != R_NilValue ){
        Rf_setAttrib(res, R_DimNamesSymbol, dnames);
    }
    reshape_or_drop(res, reshape, drop);
    // R_gc();
    
    UNPROTECT(2 + ncores);
    return(res);
}


/*** R
# devtools::load_all()
loadNamespace('bit64')

set.seed(1); file <- tempfile(); unlink(file, recursive = TRUE)
x <- filearray_create(file, 3:5, partition_size = 2, type = "float")
x[] <- 1:60

FARR_subset(x$.filebase, x$sexp_type(), list(),
            dim(x), x$.partition_info[,3], 2,
            NULL, FALSE, 1, NULL)


# set_buffer_size(31)

# unlink(file)
set.seed(1)
basefile <- normalizePath(tempdir(check = TRUE), mustWork = TRUE)
file <- file.path(basefile, '0.farr')
unlink(file)
write_partition(file, 1, c(3,4,1), as.double(1:12), "double")
file <- file.path(basefile, '1.farr')
unlink(file)
write_partition(file, 1, c(3,4,2), as.double(13:36), "double")
file <- file.path(basefile, '2.farr')
unlink(file)
write_partition(file, 1, c(3,4,2), as.double(37:60), "double")
# 
# 
# # fid = file(file, "w+b"); write_header(fid, 1, c(400, 100, 500, 5), "double", 8L); close(fid)
# write_partition(file, 1, c(400, 100, 500, 5), as.double(1:1e8), "double")
# 
# idx1 <- bit64::as.integer64(0:39999)
# idx2 <- bit64::as.integer64(sample(0:2499))
# 
# system.time({
#     c_subset(file, 40000, idx1, idx2)
# }, gcFirst = TRUE)
# # unlink(file)


# re <- structure(realToInt64(c(1L,2L,NA_integer_), 1, 3), class = 'integer64')
# re
# 
# a <- bit64::as.integer64.double(c(1,2,NA))
# class(a) <- NULL; a

# loc2idx(locationList(list(),c(3,2), 1), c(3,2))
# loc2idx(list(),c(3,2))
# loc2idx(list(c(1,2,NA,3,4), 1:10), c(4,2), strict = 0)
# (function(...){
#     loc2idx(environment(), c(3,2))
# })(c(1,2,NA), )

# re <- bit64::as.integer64(rep(0.0, 12))
# x <- bit64::as.integer64(as.double(1:3))
# addCycle(x, re, 4)

basefile <- paste0(basefile, '/')

a <- FARR_subset(filebase = basefile, type = 14L, 
           listOrEnv = list(c(1,2,3,3,2,1,NA,2,2), c(2,4,1,3, NA, 1), c(1:5,5:1,NA,3)),  
           dim = c(3:5),
           cum_part_sizes = cumsum(c(1,2,2)), 
           split_dim = 2)

b <- array(as.double(1:60), 3:5)[c(1,2,3,3,2,1,NA,2,2), c(2,4,1,3, NA, 1), c(1:5,5:1,NA,3)]
identical(a, b)
testthat::expect_equal(a, b)
*/
