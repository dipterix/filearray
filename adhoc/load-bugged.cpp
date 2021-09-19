#include "openmp.h"
#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include <map>
#include <fstream>
#include <cassert>
#include <iostream>
#include <iterator>
#include <algorithm>
// [[Rcpp::depends(BH)]]

#include "serialize.h"
#include "core.h"
#include "utils.h"
#include "conversion.h"
#include "load.h"
using namespace Rcpp;

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
 * Read partition
 ***********************************************************/

template <typename T,  typename B>
inline void subset_partition(
        const std::string& file, 
        T* mem_buffer, const R_xlen_t& buffer_nelems, 
        T* retptr, const R_xlen_t block_size, 
        SEXP idx1, int64_t idx1_start, int64_t idx1_end,
        SEXP idx2, int64_t idx2_start, int64_t idx2_end,
        int idx1_sorted, int idx2_sorted,
        void (*transform)(const B*, T*, const int&)
) {
    const boost::interprocess::mode_t mode = boost::interprocess::read_only;
    boost::interprocess::file_mapping file_map(file.c_str(), mode);
    int elem_size = sizeof(B);
    
    const int64_t region_len = idx1_end - idx1_start + 1;
    
    int64_t* idx1ptr = INTEGER64(idx1);
    R_xlen_t idx1len = Rf_xlength(idx1);
    
    int64_t* idx2ptr = INTEGER64(idx2);
    R_xlen_t idx2len = Rf_xlength(idx2);
    
    R_xlen_t ii = 0, jj = 0, ll = 0, ii_idx2 = 0;
    T* retptr2 = retptr;
    T* retptr3 = retptr;
    
    // print(idx2);
    // Rcout << idx2_sorted << "---\n";
    int matched = 0;
    
    for(; ii_idx2 < idx2len; ii_idx2++ , idx2ptr++) {
        
        // read current block!
        retptr2 = retptr + ii_idx2 * idx1len;
        //     (idx1_start + block_size * (*idx2ptr));
        
        boost::interprocess::mapped_region region(
                file_map, mode, 
                FARR_HEADER_LENGTH + elem_size * (idx1_start + block_size * (*idx2ptr)), 
                region_len);
        
        B* mmap_ptr = static_cast<B*>(region.get_address());
        
        if( region.get_size() == 0 ){
            if( idx2_sorted ){
                break;
            }
            continue;
        }
        
        // while(current_pos < start_idx)
        // buffer_nelems
        transform(mmap_ptr, mem_buffer, region_len);
        
        idx1ptr = INTEGER64(idx1);
        
        for(jj = 0; jj < idx1len; jj++, idx1ptr++) {
            if(*idx1ptr == NA_INTEGER64){ continue; }
            // ll should be [conn_pos, conn_pos + ii)
            
            ll = *idx1ptr - idx1_start;
            if( ll < 0 ){ continue; }
            if( ll > idx1_end - idx1_start ){
                if( idx1_sorted ) {
                    break;
                }
                continue;
            }
            // transform(mmap_ptr2 + ll, retptr2 + jj);
            *(retptr2 + jj) = *(mem_buffer + ll);
        }
        
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
        std::vector<T*> buffer_pointers, 
        const R_xlen_t& buffer_nelems,
        void (*transform)(const B*, T*, const int&)
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
    
    int64_t* idx1rangeptr = INTEGER64(idx1range);
    int64_t idx1_start = *idx1rangeptr, idx1_end = *(idx1rangeptr + 1);
    
    if( idx1_start == NA_INTEGER64 || idx1_end < 0 || idx1_start < 0 ){
        // idx1 are all NAs, no need to subset, return NA
        
        T* retptr = ret_ptr;
        for(R_xlen_t jj = 0; jj < retlen; jj++){
            *retptr++ = na;
        }
        return(false);
    }
    
    const int idx1_sorted = 1;// kinda_sorted(idx1, idx1_start, buffer_nelems);
    
    int err = -1;
    std::string error_msg = "";
    // char* buffer[nbuffers];
    
    int ncores = getThreads();
    if(ncores > niter){
        ncores = niter;
    }
    
    const size_t file_elem_size = sizeof(B);
    
    
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
        
        try {
            
            subset_partition(
                file, 
                buffer_pointers[thread], buffer_nelems,
                             retptr, block_size,
                             idx1, idx1_start, idx1_end,
                             idx2, idx2_start, idx2_end,
                             idx1_sorted, idx2_sorted,
                             transform);
            
        } catch (...) {
            err = part;
        }
        
    }
}
    if( err >= 0 ){
        stop("Error while reading partition " + std::to_string(err+1));
    }

    return(true);
}

SEXP FARR_subset(const std::string& filebase, 
                 const List& sch,
                 const SEXPTYPE type,
                 std::vector<SEXP>& buffer_pool,
                 SEXP ret){
    std::string fbase = correct_filebase(filebase);
    
    R_xlen_t retlen = Rf_xlength(ret);
    SEXP result_dim = sch["result_dim"];
    Rf_setAttrib(ret, R_DimSymbol, result_dim);
    
    // buffer_pool should be consistent with R memory types
    int ncores = buffer_pool.size();
    if( ncores < 1 ){
        stop("Thread number and buffer pool size must be positive.");
    }
    int buffer_nelems = Rf_length(buffer_pool[0]);
    
    switch(type){
    case INTSXP: {
        std::vector<int*> buffer_ptrs(ncores);
        for(int ii = 0; ii < ncores; ii++){
            buffer_ptrs[ii] = INTEGER(buffer_pool[ii]);
        }
        FARR_subset_template<int, int>(
            fbase, sch, INTEGER(ret), NA_INTEGER, retlen,
            buffer_ptrs, buffer_nelems, 
            &transforms_asis);
        // FARR_subset_template<int, int>(
        //         fbase, sch, INTEGER(ret), NA_INTEGER, retlen,
        //         &transform_asis);
        break;
    }
    case REALSXP: {
        std::vector<double*> buffer_ptrs(ncores);
        for(int ii = 0; ii < ncores; ii++){
            buffer_ptrs[ii] = REAL(buffer_pool[ii]);
        }
        FARR_subset_template<double, double>(
            fbase, sch, REAL(ret), NA_REAL, retlen,
            buffer_ptrs, buffer_nelems, 
            &transforms_asis);
        break;
    }
    case FLTSXP: {
        std::vector<double*> buffer_ptrs(ncores);
        for(int ii = 0; ii < ncores; ii++){
            buffer_ptrs[ii] = REAL(buffer_pool[ii]);
        }
        // Rcout << "1\n";
        // REAL(ret);
        // Rcout << "2\n";
        FARR_subset_template<double, float>(
            fbase, sch, REAL(ret), NA_REAL, retlen,
            buffer_ptrs, buffer_nelems, 
            &transforms_float);
        break;
    }
    case RAWSXP: {
        std::vector<Rbyte*> buffer_ptrs(ncores);
        for(int ii = 0; ii < ncores; ii++){
            buffer_ptrs[ii] = RAW(buffer_pool[ii]);
        }
        Rbyte na_byte = 2;
        FARR_subset_template<Rbyte, Rbyte>(
            fbase, sch, RAW(ret), na_byte, retlen,
            buffer_ptrs, buffer_nelems, 
            &transforms_asis);
        break;
    }
    case LGLSXP: {
        std::vector<int*> buffer_ptrs(ncores);
        for(int ii = 0; ii < ncores; ii++){
            buffer_ptrs[ii] = LOGICAL(buffer_pool[ii]);
        }
        FARR_subset_template<int, Rbyte>(
            fbase, sch, LOGICAL(ret), NA_LOGICAL, retlen,
            buffer_ptrs, buffer_nelems, 
            &transforms_logical);
        break;
    }
    case CPLXSXP: {
        std::vector<Rcomplex*> buffer_ptrs(ncores);
        for(int ii = 0; ii < ncores; ii++){
            buffer_ptrs[ii] = COMPLEX(buffer_pool[ii]);
        }
        na_cplx_dbl();
        Rcomplex na_cplx;
        na_cplx.i = NA_REAL;
        na_cplx.r = NA_REAL;
        FARR_subset_template<Rcomplex, double>(
            fbase, sch, COMPLEX(ret), na_cplx, retlen,
            buffer_ptrs, buffer_nelems, 
            &transforms_complex);
        break;
    }
    default:
        stop("Unsupported SEXP type");
    }
    
    return(ret);
}

// [[Rcpp::export]]
SEXP FARR_subset_sequential(
        const std::string& filebase, 
        const int64_t& unit_partlen, 
        SEXP cum_partsizes, 
        SEXPTYPE array_type,
        SEXP file_buffer, 
        SEXP ret, 
        const int64_t from = 0, 
        const int64_t len = 1
) {
    if( TYPEOF(ret) != array_memory_sxptype(array_type) ){
        stop("Inconsistent `array_type` and return type");
    }
    if( TYPEOF(file_buffer) != file_buffer_sxptype(array_type) ){
        stop("Inconsistent `array_type` and `file_buffer` type");
    }
    if( len > Rf_xlength(ret) ){
        stop("`ret` size is too small");
    }
    int file_buffer_elemsize = file_element_size(array_type);
    std::string fbase = correct_filebase(filebase);
    R_len_t nparts = Rf_length(cum_partsizes);
    
    // calculate the first partition
    int64_t slice_idx1 = 0;
    int64_t slice_idx2 = 0;
    int64_t tmp = 0;
    for(; tmp <= from; tmp+= unit_partlen, slice_idx1++){}
    for(slice_idx2 = slice_idx1; tmp < from + len; tmp+= unit_partlen, slice_idx2++){}
    // Rcout << slice_idx1 << "  -  " << slice_idx2 << "\n";
    
    int part_start = 0;
    int part_end = 0;
    int64_t skip_start = 0;
    int64_t skip_end = 0;
    
    int64_t* cum_part = INTEGER64(cum_partsizes);
    for(; slice_idx1 > *cum_part; cum_part++, part_start++){}
    if( part_start == 0 ){
        skip_start = from;
    } else {
        skip_start = from - (*(cum_part - 1)) * unit_partlen;
    }
    for(part_end = part_start; slice_idx2 > *cum_part; cum_part++, part_end++){}
    skip_end = (*cum_part) * unit_partlen - (from + len);
    
    // Rcout << part_start << "  -  " << part_end << "\n";
    // Rcout << skip_start << "  -  " << skip_end << "\n";
    
    int64_t read_start = 0;
    int64_t read_len = 0;
    int64_t part_nelem = 0;
    int64_t last_part_nelem = 0;
    cum_part = INTEGER64(cum_partsizes);
    
    int64_t nread = 0;
    FILE* conn = NULL;
    R_len_t buf_nelem = Rf_length(file_buffer);
    R_len_t buf_reads = 0, buf_reads_total = 0;
    for(int part = part_start; part <= part_end; part++, cum_part++, nread += read_len){
        if( part >= nparts ){
            continue;
        }
        // get partition n_elems
        part_nelem = (*cum_part) * unit_partlen - last_part_nelem;
        last_part_nelem = (*cum_part) * unit_partlen;
        
        // skip read_start elements
        read_start = 0;
        if( part == part_start ) {
            read_start = skip_start;
        }
        // Rcout << part_nelem << "--\n";
        // then read read_len elements
        read_len = part_nelem - read_start;
        if( part == part_end ){
            read_len -= skip_end;
        }
        
        std::string part_file = fbase + std::to_string(part) + ".farr";
        conn = fopen(part_file.c_str(), "rb");
        
        if(conn == NULL){ continue; }
        
        // Rcout << part << " " << read_start << " " << read_len << "\n";
        fseek(conn, FARR_HEADER_LENGTH + file_buffer_elemsize * read_start, SEEK_SET);
        
        switch(array_type) {
        case REALSXP: {
            double* fbptr = REAL(file_buffer);
            double* mbptr = REAL(ret) + nread;
            buf_reads_total = 0;
            while(buf_reads_total < read_len){
                buf_reads = read_len - buf_reads_total;
                buf_reads = buf_reads > buf_nelem ? buf_nelem : buf_reads;
                lendian_fread(fbptr, file_buffer_elemsize, buf_reads, conn);
                transforms_asis(fbptr, mbptr, buf_reads);
                fbptr += buf_reads;
                mbptr += buf_reads;
                buf_reads_total += buf_reads;
            }
            
            break;
        }
        case INTSXP: {
            int* fbptr = INTEGER(file_buffer);
            int* mbptr = INTEGER(ret) + nread;
            buf_reads_total = 0;
            while(buf_reads_total < read_len){
                buf_reads = read_len - buf_reads_total;
                buf_reads = buf_reads > buf_nelem ? buf_nelem : buf_reads;
                lendian_fread(fbptr, file_buffer_elemsize, buf_reads, conn);
                transforms_asis(fbptr, mbptr, buf_reads);
                fbptr += buf_reads;
                mbptr += buf_reads;
                buf_reads_total += buf_reads;
            }
            
            break;
        }
        case RAWSXP: {
            Rbyte* fbptr = RAW(file_buffer);
            Rbyte* mbptr = RAW(ret) + nread;
            buf_reads_total = 0;
            while(buf_reads_total < read_len){
                buf_reads = read_len - buf_reads_total;
                buf_reads = buf_reads > buf_nelem ? buf_nelem : buf_reads;
                lendian_fread(fbptr, file_buffer_elemsize, buf_reads, conn);
                transforms_asis(fbptr, mbptr, buf_reads);
                fbptr += buf_reads;
                mbptr += buf_reads;
                buf_reads_total += buf_reads;
            }
            
            break;
        }
        case FLTSXP: {
            float* fbptr = FLOAT(file_buffer);
            double* mbptr = REAL(ret) + nread;
            buf_reads_total = 0;
            while(buf_reads_total < read_len){
                buf_reads = read_len - buf_reads_total;
                buf_reads = buf_reads > buf_nelem ? buf_nelem : buf_reads;
                lendian_fread(fbptr, file_buffer_elemsize, buf_reads, conn);
                transforms_float(fbptr, mbptr, buf_reads);
                fbptr += buf_reads;
                mbptr += buf_reads;
                buf_reads_total += buf_reads;
            }
            
            break;
        }
        case LGLSXP: {
            Rbyte* fbptr = RAW(file_buffer);
            int* mbptr = LOGICAL(ret) + nread;
            buf_reads_total = 0;
            while(buf_reads_total < read_len){
                buf_reads = read_len - buf_reads_total;
                buf_reads = buf_reads > buf_nelem ? buf_nelem : buf_reads;
                lendian_fread(fbptr, file_buffer_elemsize, buf_reads, conn);
                transforms_logical(fbptr, mbptr, buf_reads);
                fbptr += buf_reads;
                mbptr += buf_reads;
                buf_reads_total += buf_reads;
            }
            
            break;
        }
        case CPLXSXP: {
            double* fbptr = REAL(file_buffer);
            Rcomplex* mbptr = COMPLEX(ret) + nread;
            buf_reads_total = 0;
            while(buf_reads_total < read_len){
                buf_reads = read_len - buf_reads_total;
                buf_reads = buf_reads > buf_nelem ? buf_nelem : buf_reads;
                lendian_fread(fbptr, file_buffer_elemsize, buf_reads, conn);
                transforms_complex(fbptr, mbptr, buf_reads);
                fbptr += buf_reads;
                mbptr += buf_reads;
                buf_reads_total += buf_reads;
            }
            
            break;
        }
        default: {
            fclose(conn);
            conn = NULL;
            stop("Unsupported SEXP type");
        }
        }
        // nread += read_len;
        
        fclose(conn);
        conn = NULL;
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
    
    // allocate for returns
    int64_t retlen = *INTEGER64(sch["result_length"]);
    // const SEXP idx1 = sch["idx1"];
    // const IntegerVector idx2lens = sch["idx2lens"];
    // R_xlen_t idx1len = Rf_xlength(idx1);
    // R_xlen_t retlen = idx1len * idx2lens[Rf_length(cum_part_size) - 1];
    // 
    SEXPTYPE ret_type = array_memory_sxptype(sexp_type);
    SEXP res = PROTECT(Rf_allocVector(ret_type, retlen));
    
    // allocate buffers
    int ncores = getThreads();
    SEXP idx1range = sch["idx1range"];
    int64_t* idx1rangeptr = INTEGER64(idx1range);
    int64_t idx1_start = *idx1rangeptr, idx1_end = *(idx1rangeptr + 1);
    int buffer_nelems = get_buffer_nelem(ret_type);
    if( idx1_start == NA_INTEGER64 || idx1_end < 0 || idx1_start < 0 ){
    } else {
        buffer_nelems = idx1_end - idx1_start + 1;
    }
    
    std::vector<SEXP> buffer_pool(ncores);
    for(int ii = 0; ii < ncores; ii++){
        buffer_pool[ii] = PROTECT(Rf_allocVector(ret_type, buffer_nelems));
    }
    
    
    
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
devtools::load_all()
set.seed(1); file <- tempfile(); unlink(file, recursive = TRUE)
x <- filearray_create(file, 3:5, partition_size = 2, type = "complex")
x$initialize_partition()
FARR_subset_assign2(
    filebase = x$.filebase,
    1:60 + 1i,
    listOrEnv = list()
)
x[]



# # devtools::load_all()
# loadNamespace('bit64')
# 
# set.seed(1); file <- tempfile(); unlink(file, recursive = TRUE)
# x <- filearray_create(file, 3:5, partition_size = 2, type = "float")
# x[] <- 1:60
# 
# FARR_subset(x$.filebase, x$sexp_type(), list(),
#             dim(x), x$.partition_info[,3], 2,
#             NULL, FALSE, 1, NULL)
# 
# 
# # set_buffer_size(31)
# 
# # unlink(file)
# set.seed(1)
# basefile <- normalizePath(tempdir(check = TRUE), mustWork = TRUE)
# file <- file.path(basefile, '0.farr')
# unlink(file)
# write_partition(file, 1, c(3,4,1), as.double(1:12), "double")
# file <- file.path(basefile, '1.farr')
# unlink(file)
# write_partition(file, 1, c(3,4,2), as.double(13:36), "double")
# file <- file.path(basefile, '2.farr')
# unlink(file)
# write_partition(file, 1, c(3,4,2), as.double(37:60), "double")
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

# basefile <- paste0(basefile, '/')
# 
# a <- FARR_subset(filebase = basefile, type = 14L, 
#            listOrEnv = list(c(1,2,3,3,2,1,NA,2,2), c(2,4,1,3, NA, 1), c(1:5,5:1,NA,3)),  
#            dim = c(3:5),
#            cum_part_sizes = cumsum(c(1,2,2)), 
#            split_dim = 2)
# 
# b <- array(as.double(1:60), 3:5)[c(1,2,3,3,2,1,NA,2,2), c(2,4,1,3, NA, 1), c(1:5,5:1,NA,3)]
# identical(a, b)
# testthat::expect_equal(a, b)
*/
