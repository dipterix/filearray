#include "openmp.h"
#include "common.h"
using namespace Rcpp;

SEXP FARR_subset_integer(const std::string& filebase, const List sch){
    const int nbuffers = get_buffer_size();
    // Rcout << nbuffers << "\n";
    // List sch = schedule(filebase, listOrEnv, dim, cum_part_sizes, 
    //                     split_dim, strict);
    SEXP idx1 = sch["idx1"];
    List idx2s = sch["idx2s"];
    int64_t block_size = (int64_t) (sch["block_size"]);
    IntegerVector partitions = sch["partitions"];
    IntegerVector idx2lens = sch["idx2lens"];
    
    R_xlen_t niter = partitions.length();
    
    
    R_xlen_t idx1len = Rf_xlength(idx1);
    
    // TODO: change
    SEXP ret = PROTECT(Rf_allocVector(INTSXP, idx1len * idx2lens[niter - 1]));
    // TODO: change
    const int na = NA_INTEGER;
    // TODO: change
    const int elem_size = sizeof(int);
    
    int64_t idx1_start = NA_INTEGER64, idx1_end = -1;
    int64_t* ptr = (int64_t*) REAL(idx1); 
    R_xlen_t i = 0;
    for(ptr = (int64_t*) REAL(idx1), i = 0; i < idx1len; i++, ptr++ ){
        if( *ptr == NA_INTEGER64 ){
            continue;
        }
        if( *ptr < idx1_start || idx1_start == NA_INTEGER64 ){
            idx1_start = *ptr;
        }
        if( idx1_end < *ptr ){
            idx1_end = *ptr;
        }
    }
    
    if( idx1_start == NA_INTEGER64 || idx1_end < 0 || idx1_start < 0 ){
        // idx1 are all NAs, no need to subset, return NA
        
        // TODO: change
        int* retptr = INTEGER(ret);
        R_xlen_t retlen = Rf_xlength(ret);
        for(R_xlen_t jj = 0; jj < retlen; jj++){
            *retptr++ = na;
        }
        UNPROTECT(1);
        return(ret);
    }
    
    const int idx1_sorted = kinda_sorted(idx1, idx1_start, nbuffers / elem_size);
    
    int err = -1;
    // char* buffer[nbuffers];
    
    int ncores = getThreads();
    if(ncores > niter){
        ncores = niter;
    }
    
    
    std::vector<SEXP> buff_pool(ncores);
    for(int i = 0; i < ncores; i++){
        // TODO: change
        buff_pool[i] = PROTECT(Rf_allocVector(INTSXP, nbuffers / elem_size));
    }
    IntegerVector buff_busy(ncores);
    
#pragma omp parallel num_threads(ncores) 
{
#pragma omp for schedule(dynamic) nowait
{
    for(R_xlen_t ii = 0; ii < niter; ii++){
        int part = partitions[ii];
        int64_t skips = 0;
        if(ii > 0){
            skips = idx2lens[ii - 1];
        }
        int64_t idx2len = idx2lens[ii] - skips;
        
        // TODO: change
        int* retptr = INTEGER(ret) + skips * idx1len;
        for(R_xlen_t jj = 0; jj < idx2len * idx1len; jj++, retptr++ ){
            *retptr = na;
        }
        
        // TODO: change
        retptr = INTEGER(ret) + skips * idx1len;
        
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
            
            // get current buffer
            int thread = 0;
#pragma omp critical
{
            for(thread = 0; thread < ncores; thread++){
                if(!buff_busy[thread]){
                    buff_busy[thread] = 1;
                    break;
                }
            }
}
            // TODO: change
            // int* buffer = INTEGER(buf);
            int* buffer = INTEGER(buff_pool[thread]);
            
            try{
                subset_partition(conn, buffer, nbuffers, retptr, block_size,
                                 idx1, idx1_start, idx1_end,
                                 idx2, idx2_start, idx2_end,
                                 idx1_sorted, idx2_sorted);
                // subset_partition(conn, buffer, nbuffers, retptr, block_size, 
                //                  idx1, idx1_start, idx1_end,
                //                  idx2, idx2_start, idx2_end,
                //                  0, idx2_sorted);
            } catch(...){
                fclose(conn);
                conn = NULL;
                err = part;
            }
            buff_busy[thread] = 0;
            if( conn != NULL ){
                fclose(conn);
            }
        }
    }
}
}
    UNPROTECT(1 + ncores);
    return(ret);
}


SEXP FARR_subset_double(const std::string& filebase, const List sch){
    const int nbuffers = get_buffer_size();
    // Rcout << nbuffers << "\n";
    // List sch = schedule(filebase, listOrEnv, dim, cum_part_sizes, 
    //                     split_dim, strict);
    SEXP idx1 = sch["idx1"];
    List idx2s = sch["idx2s"];
    int64_t block_size = (int64_t) (sch["block_size"]);
    IntegerVector partitions = sch["partitions"];
    IntegerVector idx2lens = sch["idx2lens"];
    
    R_xlen_t niter = partitions.length();
    
    
    R_xlen_t idx1len = Rf_xlength(idx1);
    
    // TODO: change
    SEXP ret = PROTECT(Rf_allocVector(REALSXP, idx1len * idx2lens[niter - 1]));
    // TODO: change
    const double na = NA_REAL;
    // TODO: change
    const int elem_size = sizeof(double);
    
    int64_t idx1_start = NA_INTEGER64, idx1_end = -1;
    int64_t* ptr = (int64_t*) REAL(idx1); 
    R_xlen_t i = 0;
    for(ptr = (int64_t*) REAL(idx1), i = 0; i < idx1len; i++, ptr++ ){
        if( *ptr == NA_INTEGER64 ){
            continue;
        }
        if( *ptr < idx1_start || idx1_start == NA_INTEGER64 ){
            idx1_start = *ptr;
        }
        if( idx1_end < *ptr ){
            idx1_end = *ptr;
        }
    }
    
    if( idx1_start == NA_INTEGER64 || idx1_end < 0 || idx1_start < 0 ){
        // idx1 are all NAs, no need to subset, return NA
        
        // TODO: change
        double* retptr = REAL(ret);
        R_xlen_t retlen = Rf_xlength(ret);
        for(R_xlen_t jj = 0; jj < retlen; jj++){
            *retptr++ = na;
        }
        UNPROTECT(1);
        return(ret);
    }
    
    const int idx1_sorted = kinda_sorted(idx1, idx1_start, nbuffers / elem_size);
    
    int err = -1;
    // char* buffer[nbuffers];
    
    int ncores = getThreads();
    if(ncores > niter){
        ncores = niter;
    }
    
    
    std::vector<SEXP> buff_pool(ncores);
    for(int i = 0; i < ncores; i++){
        // TODO: change
        buff_pool[i] = PROTECT(Rf_allocVector(REALSXP, nbuffers / elem_size));
    }
    IntegerVector buff_busy(ncores);
    
#pragma omp parallel num_threads(ncores) 
{
#pragma omp for schedule(dynamic) nowait
{
    for(R_xlen_t ii = 0; ii < niter; ii++){
        int part = partitions[ii];
        int64_t skips = 0;
        if(ii > 0){
            skips = idx2lens[ii - 1];
        }
        int64_t idx2len = idx2lens[ii] - skips;
        
        // TODO: change
        double* retptr = REAL(ret) + skips * idx1len;
        for(R_xlen_t jj = 0; jj < idx2len * idx1len; jj++, retptr++ ){
            *retptr = na;
        }
        
        // TODO: change
        retptr = REAL(ret) + skips * idx1len;
        
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
            
            // get current buffer
            int thread = 0;
#pragma omp critical
{
            for(thread = 0; thread < ncores; thread++){
                if(!buff_busy[thread]){
                    buff_busy[thread] = 1;
                    break;
                }
            }
}
            
            // TODO: change
            // int* buffer = INTEGER(buf);
            double* buffer = REAL(buff_pool[thread]);
            
            try{
                subset_partition(conn, buffer, nbuffers, retptr, block_size, 
                                 idx1, idx1_start, idx1_end,
                                 idx2, idx2_start, idx2_end,
                                 idx1_sorted, idx2_sorted);
            } catch(...){
                fclose(conn);
                conn = NULL;
                err = part;
            }
            buff_busy[thread] = 0;
            if( conn != NULL ){
                fclose(conn);
            }
        }
    }
}
}
    UNPROTECT(1 + ncores);
    return(ret);
}


SEXP FARR_subset_raw(const std::string& filebase, const List sch,
                       const Rbyte na = 0){
    const int nbuffers = get_buffer_size();
    // Rcout << nbuffers << "\n";
    // List sch = schedule(filebase, listOrEnv, dim, cum_part_sizes, 
    //                     split_dim, strict);
    SEXP idx1 = sch["idx1"];
    List idx2s = sch["idx2s"];
    int64_t block_size = (int64_t) (sch["block_size"]);
    IntegerVector partitions = sch["partitions"];
    IntegerVector idx2lens = sch["idx2lens"];
    
    R_xlen_t niter = partitions.length();
    
    
    R_xlen_t idx1len = Rf_xlength(idx1);
    
    // TODO: change
    SEXP ret = PROTECT(Rf_allocVector(RAWSXP, idx1len * idx2lens[niter - 1]));
    // TODO: change
    const int elem_size = sizeof(Rbyte);
    
    int64_t idx1_start = NA_INTEGER64, idx1_end = -1;
    int64_t* ptr = (int64_t*) REAL(idx1); 
    R_xlen_t i = 0;
    for(ptr = (int64_t*) REAL(idx1), i = 0; i < idx1len; i++, ptr++ ){
        if( *ptr == NA_INTEGER64 ){
            continue;
        }
        if( *ptr < idx1_start || idx1_start == NA_INTEGER64 ){
            idx1_start = *ptr;
        }
        if( idx1_end < *ptr ){
            idx1_end = *ptr;
        }
    }
    
    if( idx1_start == NA_INTEGER64 || idx1_end < 0 || idx1_start < 0 ){
        // idx1 are all NAs, no need to subset, return NA
        
        // TODO: change
        Rbyte* retptr = RAW(ret);
        R_xlen_t retlen = Rf_xlength(ret);
        for(R_xlen_t jj = 0; jj < retlen; jj++){
            *retptr++ = na;
        }
        UNPROTECT(1);
        return(ret);
    }
    
    const int idx1_sorted = kinda_sorted(idx1, idx1_start, nbuffers / elem_size);
    
    int err = -1;
    // char* buffer[nbuffers];
    
    int ncores = getThreads();
    if(ncores > niter){
        ncores = niter;
    }
    
    
    std::vector<SEXP> buff_pool(ncores);
    for(int i = 0; i < ncores; i++){
        // TODO: change
        buff_pool[i] = PROTECT(Rf_allocVector(RAWSXP, nbuffers / elem_size));
    }
    IntegerVector buff_busy(ncores);
    
#pragma omp parallel num_threads(ncores) 
{
#pragma omp for schedule(dynamic) nowait
{
    for(R_xlen_t ii = 0; ii < niter; ii++){
        int part = partitions[ii];
        int64_t skips = 0;
        if(ii > 0){
            skips = idx2lens[ii - 1];
        }
        int64_t idx2len = idx2lens[ii] - skips;
        
        // TODO: change
        Rbyte* retptr = RAW(ret) + skips * idx1len;
        for(R_xlen_t jj = 0; jj < idx2len * idx1len; jj++, retptr++ ){
            *retptr = na;
        }
        
        // TODO: change
        retptr = RAW(ret) + skips * idx1len;
        
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
            
            // get current buffer
            int thread = 0;
#pragma omp critical
{
            for(thread = 0; thread < ncores; thread++){
                if(!buff_busy[thread]){
                    buff_busy[thread] = 1;
                    break;
                }
            }
}

            // TODO: change
            // int* buffer = INTEGER(buf);
            Rbyte* buffer = RAW(buff_pool[thread]);
            
            try{
                subset_partition(conn, buffer, nbuffers, retptr, block_size, 
                                 idx1, idx1_start, idx1_end,
                                 idx2, idx2_start, idx2_end,
                                 idx1_sorted, idx2_sorted);
            } catch(...){
                fclose(conn);
                conn = NULL;
                err = part;
            }
            buff_busy[thread] = 0;
            if( conn != NULL ){
                fclose(conn);
            }
        }
    }
}
}
    UNPROTECT(1 + ncores);
    return(ret);
}

SEXP FARR_subset_logical(const std::string& filebase, const List sch){
    SEXP x = PROTECT(FARR_subset_raw(
        filebase, sch, 2));
    R_xlen_t len = Rf_xlength(x);
    SEXP ret = PROTECT(Rf_allocVector(LGLSXP, len));
    
    int* retptr = LOGICAL(ret);
    Rbyte* xptr = RAW(x);
    for(; len > 0; len--, xptr++, retptr++){
        if(*xptr == 0){
            *retptr = FALSE;
        } else if (*xptr == 1){
            *retptr = TRUE;
        } else {
            *retptr = NA_LOGICAL;
        }
    }
    
    UNPROTECT(2);
    return ret;
}

// [[Rcpp::export]]
SEXP FARR_subset(const std::string& filebase, 
                const SEXPTYPE type,
                const SEXP listOrEnv, 
                const NumericVector& dim, 
                const NumericVector& cum_part_sizes,
                const int split_dim, 
                const SEXP reshape = R_NilValue, 
                const bool drop = false,
                const int strict = 1){
    List sch = schedule(listOrEnv, dim, cum_part_sizes, 
                        split_dim, strict);
    
    SEXP ret = R_NilValue;
    
    
    switch(type){
    case INTSXP:
        ret = PROTECT(FARR_subset_integer(filebase, sch));
        break;
    case REALSXP: 
        ret = PROTECT(FARR_subset_double(filebase, sch));
        break;
    case RAWSXP: 
        ret = PROTECT(FARR_subset_raw(filebase, sch));
        break;
    case LGLSXP: 
        ret = PROTECT(FARR_subset_logical(filebase, sch));
        break;
    default:
        stop("Unsupported SEXP type");
    }
    
    SEXP result_dim = sch["result_dim"];
    Rf_setAttrib(ret, R_DimSymbol, result_dim);
    reshape_or_drop(ret, reshape, drop);
    
    UNPROTECT(1);
    
    return(ret);
}

/*** R
# devtools::load_all()
loadNamespace('bit64')

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


# re <- structure(realToUint64(c(1L,2L,NA_integer_), 1, 3), class = 'integer64')
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
