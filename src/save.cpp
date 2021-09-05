#include "common.h"
#include "openmp.h"
using namespace Rcpp;

// [[Rcpp::export]]
SEXP FARR_subset_assign_integer(
        const std::string& filebase, 
        const List& sch, SEXP value){
    
    SEXP idx1 = sch["idx1"];
    List idx2s = sch["idx2s"];
    int64_t block_size = (int64_t) (sch["block_size"]);
    IntegerVector partitions = sch["partitions"];
    IntegerVector idx2lens = sch["idx2lens"];
    
    R_xlen_t idx1len = Rf_xlength(idx1);
    int has_error = -1;
    
    int ncores = getThreads();
    if(ncores > idx2s.length()){
        ncores = idx2s.length();
    }
    // ncores = 1;
    
    std::vector<SEXP> buff_pool(ncores);
    std::vector<int*> buff_ptrs(ncores);
    std::vector<bool> buff_busy(ncores);
    for(int i = 0; i < ncores; i++){
        // TODO: change
        buff_pool[i] = PROTECT(Rf_allocVector(INTSXP, block_size));
        buff_ptrs[i] = INTEGER(buff_pool[i]);
        buff_busy[i] = false;
    }
    
    int* value_ptr = INTEGER(value);
    int64_t* idx1ptr0 = (int64_t*) REAL(idx1);
    
    // std::vector<int64_t*> idx2_ptrs(idx2s.length());
    // for(R_xlen_t ii = 0; ii < idx2s.length(); ii++){
    //     idx2_ptrs[ii] = (int64_t*) REAL(idx2s[ii]);
    // }
    
#pragma omp parallel num_threads(ncores)
{
#pragma omp for schedule(dynamic) nowait
{
    for(R_xlen_t iter = 0; iter < idx2s.length(); iter++){
        R_xlen_t part = partitions[iter];
        int64_t skips = 0;
        if(iter > 0){
            skips = idx2lens[iter - 1];
        }
        
        std::string file = filebase + std::to_string(part) + ".farr";
        FILE* conn = fopen( file.c_str(), "r+b" );
        if (conn) {
            // get current buffer
            int thread = 0;
#pragma omp critical
{
            for(thread = 0; thread < ncores; thread++){
                if(!buff_busy[thread]){
                    buff_busy[thread] = true;
                    break;
                }
            }
}
            try{
                SEXP idx2 = idx2s[iter];
                
                // TODO: change
                // subset_assign_partition_old(
                //     conn, REAL(value) + (idx1len * skips),
                //     block_size, idx1, idx2, REAL(buff_pool[thread]));
                
                int64_t* idx2_ptr = (int64_t*) REAL(idx2);
                R_xlen_t idx2_len = Rf_xlength(idx2);
                int* value_ptr2 = value_ptr + (idx1len * skips);
                int64_t* idx1ptr = idx1ptr0;
                subset_assign_partition(
                    conn, value_ptr2,
                    block_size, idx1ptr, idx1len,
                    idx2_ptr, idx2_len,
                    buff_ptrs[thread] );
                fflush(conn);
                fclose(conn);
                conn = NULL;
            } catch(...){
                fclose(conn);
                conn = NULL;
                has_error = part;
            }
            if( conn != NULL ){
                fclose(conn);
            }
            buff_busy[thread] = false;
        }
    }
}
}

    UNPROTECT(ncores);
    if( has_error >= 0 ){
        stop("Cannot write to partition " + std::to_string(has_error + 1));
    }
    
    return( R_NilValue );
}

// [[Rcpp::export]]
SEXP FARR_subset_assign_double(
        const std::string& filebase, 
        const List& sch, SEXP value){
    
    SEXP idx1 = sch["idx1"];
    List idx2s = sch["idx2s"];
    int64_t block_size = (int64_t) (sch["block_size"]);
    IntegerVector partitions = sch["partitions"];
    IntegerVector idx2lens = sch["idx2lens"];
    
    R_xlen_t idx1len = Rf_xlength(idx1);
    int has_error = -1;
    
    int ncores = getThreads();
    if(ncores > idx2s.length()){
        ncores = idx2s.length();
    }
    // ncores = 1;
    
    std::vector<SEXP> buff_pool(ncores);
    std::vector<double*> buff_ptrs(ncores);
    std::vector<bool> buff_busy(ncores);
    for(int i = 0; i < ncores; i++){
        // TODO: change
        buff_pool[i] = PROTECT(Rf_allocVector(REALSXP, block_size));
        buff_ptrs[i] = REAL(buff_pool[i]);
        buff_busy[i] = false;
    }
    
    double* value_ptr = REAL(value);
    int64_t* idx1ptr0 = (int64_t*) REAL(idx1);
    
    // std::vector<int64_t*> idx2_ptrs(idx2s.length());
    // for(R_xlen_t ii = 0; ii < idx2s.length(); ii++){
    //     idx2_ptrs[ii] = (int64_t*) REAL(idx2s[ii]);
    // }
    
#pragma omp parallel num_threads(ncores)
{
#pragma omp for schedule(dynamic) nowait
{
    for(R_xlen_t iter = 0; iter < idx2s.length(); iter++){
        R_xlen_t part = partitions[iter];
        int64_t skips = 0;
        if(iter > 0){
            skips = idx2lens[iter - 1];
        }
        
        std::string file = filebase + std::to_string(part) + ".farr";
        FILE* conn = fopen( file.c_str(), "r+b" );
        if (conn) {
            // get current buffer
            int thread = 0;
#pragma omp critical
{
            for(thread = 0; thread < ncores; thread++){
                if(!buff_busy[thread]){
                    buff_busy[thread] = true;
                    break;
                }
            }
}
            try{
                SEXP idx2 = idx2s[iter];
                
                // TODO: change
                // subset_assign_partition_old(
                //     conn, REAL(value) + (idx1len * skips),
                //     block_size, idx1, idx2, REAL(buff_pool[thread]));
                
                int64_t* idx2_ptr = (int64_t*) REAL(idx2);
                R_xlen_t idx2_len = Rf_xlength(idx2);
                double* value_ptr2 = value_ptr + (idx1len * skips);
                int64_t* idx1ptr = idx1ptr0;
                subset_assign_partition(
                    conn, value_ptr2,
                    block_size, idx1ptr, idx1len,
                    idx2_ptr, idx2_len,
                    buff_ptrs[thread] );
                fflush(conn);
                fclose(conn);
                conn = NULL;
            } catch(...){
                fclose(conn);
                conn = NULL;
                has_error = part;
            }
            if( conn != NULL ){
                fclose(conn);
            }
            buff_busy[thread] = false;
        }
    }
}
}
    
    UNPROTECT(ncores);
    if( has_error >= 0 ){
        stop("Cannot write to partition " + std::to_string(has_error + 1));
    }

    return( R_NilValue );
}

// [[Rcpp::export]]
SEXP FARR_subset_assign_raw(
        const std::string& filebase, 
        const List& sch, SEXP value){
    
    SEXP idx1 = sch["idx1"];
    List idx2s = sch["idx2s"];
    int64_t block_size = (int64_t) (sch["block_size"]);
    IntegerVector partitions = sch["partitions"];
    IntegerVector idx2lens = sch["idx2lens"];
    
    R_xlen_t idx1len = Rf_xlength(idx1);
    int has_error = -1;
    
    int ncores = getThreads();
    if(ncores > idx2s.length()){
        ncores = idx2s.length();
    }
    // ncores = 1;
    
    std::vector<SEXP> buff_pool(ncores);
    std::vector<Rbyte*> buff_ptrs(ncores);
    std::vector<bool> buff_busy(ncores);
    for(int i = 0; i < ncores; i++){
        // TODO: change
        buff_pool[i] = PROTECT(Rf_allocVector(RAWSXP, block_size));
        buff_ptrs[i] = RAW(buff_pool[i]);
        buff_busy[i] = false;
    }
    
    Rbyte* value_ptr = RAW(value);
    int64_t* idx1ptr0 = (int64_t*) REAL(idx1);
    
    // std::vector<int64_t*> idx2_ptrs(idx2s.length());
    // for(R_xlen_t ii = 0; ii < idx2s.length(); ii++){
    //     idx2_ptrs[ii] = (int64_t*) REAL(idx2s[ii]);
    // }
    
#pragma omp parallel num_threads(ncores)
{
#pragma omp for schedule(dynamic) nowait
{
    for(R_xlen_t iter = 0; iter < idx2s.length(); iter++){
        R_xlen_t part = partitions[iter];
        int64_t skips = 0;
        if(iter > 0){
            skips = idx2lens[iter - 1];
        }
        
        std::string file = filebase + std::to_string(part) + ".farr";
        FILE* conn = fopen( file.c_str(), "r+b" );
        if (conn) {
            // get current buffer
            int thread = 0;
#pragma omp critical
{
            for(thread = 0; thread < ncores; thread++){
                if(!buff_busy[thread]){
                    buff_busy[thread] = true;
                    break;
                }
            }
}
            try{
                SEXP idx2 = idx2s[iter];
                
                // TODO: change
                // subset_assign_partition_old(
                //     conn, REAL(value) + (idx1len * skips),
                //     block_size, idx1, idx2, REAL(buff_pool[thread]));
                
                int64_t* idx2_ptr = (int64_t*) REAL(idx2);
                R_xlen_t idx2_len = Rf_xlength(idx2);
                Rbyte* value_ptr2 = value_ptr + (idx1len * skips);
                int64_t* idx1ptr = idx1ptr0;
                subset_assign_partition(
                    conn, value_ptr2,
                    block_size, idx1ptr, idx1len,
                    idx2_ptr, idx2_len,
                    buff_ptrs[thread] );
                fflush(conn);
                fclose(conn);
                conn = NULL;
            } catch(...){
                fclose(conn);
                conn = NULL;
                has_error = part;
            }
            if( conn != NULL ){
                fclose(conn);
            }
            buff_busy[thread] = false;
        }
    }
}
}

    UNPROTECT(ncores);
    if( has_error >= 0 ){
        stop("Cannot write to partition " + std::to_string(has_error + 1));
    }
    
    return( R_NilValue );
}

// [[Rcpp::export]]
SEXP FARR_subset_assign_logical(
        const std::string& filebase,
        const List& sch, SEXP value){

    R_xlen_t len = Rf_xlength(value);

    SEXP value_raw = PROTECT(Rf_allocVector(RAWSXP, len));
    int* valenptr = LOGICAL(value);
    Rbyte* rawptr = RAW(value_raw);
    for(R_xlen_t ii = 0; ii < len; ii++, valenptr++, rawptr++){
        if(*valenptr == NA_LOGICAL){
            *rawptr = 2;
        } else if (*valenptr == TRUE){
            *rawptr = 1;
        } else {
            *rawptr = 0;
        }
    }
    FARR_subset_assign_raw(filebase, sch, value_raw);

    UNPROTECT(1);

    return R_NilValue;
}

// [[Rcpp::export]]
SEXP FARR_subset_assign(
        const std::string& filebase,
        const SEXP listOrEnv,
        const NumericVector& dim,
        const NumericVector& cum_part_sizes,
        const int split_dim,
        const SEXPTYPE type,
        SEXP value_){
    List sch = schedule(listOrEnv, dim, cum_part_sizes,
                        split_dim, 1);
    
    int n_protected = 0;
    SEXP value = value_;
    if( TYPEOF(value) != type ){
        value = PROTECT(Rf_coerceVector(value, type));
        n_protected = 1;
    }
    
    switch(type) {
    case INTSXP:
        FARR_subset_assign_integer(filebase, sch, value);
        break;
    case REALSXP:
        FARR_subset_assign_double(filebase, sch, value);
        break;
    case RAWSXP:
        FARR_subset_assign_raw(filebase, sch, value);
        break;
    case LGLSXP:
        FARR_subset_assign_logical(filebase, sch, value);
        break;
    default:
        stop("SEXP type not supported.");
    }
    if( n_protected ){
        UNPROTECT(n_protected);
    }
    return(R_NilValue);
}


/*** R
devtools::load_all()
dim <- c(100,100,100,100)
set.seed(1); file <- tempfile(); unlink(file, recursive = TRUE)
x <- filearray_create(file, dim, type = 'double')
tmp <- as.double(seq_len(1e8))
setThreads(3)
system.time({
    x[] <- tmp
}, gcFirst = TRUE)
identical(as.vector(x[]), as.double(tmp))
pryr::object_size(x)
*/
