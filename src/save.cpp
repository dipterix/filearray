#include "common.h"
#include "openmp.h"
using namespace Rcpp;

SEXP FARR_subset_assign_integer(
        const std::string& filebase, 
        const List& sch, SEXP value){
    
    SEXP idx1 = sch["idx1"];
    SEXP idx1range = sch["idx1range"];
    List idx2s = sch["idx2s"];
    int64_t block_size = (int64_t) (sch["block_size"]);
    IntegerVector partitions = sch["partitions"];
    IntegerVector idx2lens = sch["idx2lens"];
    
    int has_error = -1;
    
    R_xlen_t idx1len = Rf_xlength(idx1);
    int64_t* idx1rangeptr = (int64_t*) REAL(idx1range);
    int64_t idx1_start = *idx1rangeptr, idx1_end = *(idx1rangeptr + 1);
    
    if( idx1_start == NA_INTEGER64 || idx1_end < 0 || idx1_start < 0 ){
        return( R_NilValue );
    }
    
    
    
    int ncores = getThreads();
    if(ncores > idx2s.length()){
        ncores = idx2s.length();
    }
    // ncores = 1;
    
    std::vector<SEXP> buff_pool(ncores);
    std::vector<int*> buff_ptrs(ncores);
    for(int i = 0; i < ncores; i++){
        // TODO: change
        buff_pool[i] = PROTECT(Rf_allocVector(INTSXP, idx1_end - idx1_start + 1));
        buff_ptrs[i] = INTEGER(buff_pool[i]);
    }
    
    int* value_ptr = INTEGER(value);
    int64_t* idx1ptr0 = (int64_t*) REAL(idx1);
    
    // std::vector<int64_t*> idx2_ptrs(idx2s.length());
    // for(R_xlen_t ii = 0; ii < idx2s.length(); ii++){
    //     idx2_ptrs[ii] = (int64_t*) REAL(idx2s[ii]);
    // }
    
#pragma omp parallel num_threads(ncores)
{
#pragma omp for schedule(static, 1) nowait
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
            int thread = iter % ncores;
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
                    idx1_start, idx1_end, 
                    idx2_ptr, idx2_len,
                    buff_ptrs[thread], NA_INTEGER, NA_INTEGER );
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
        }
    }
}

    UNPROTECT(ncores);
    if( has_error >= 0 ){
        stop("Cannot write to partition " + std::to_string(has_error + 1));
    }
    
    return( R_NilValue );
}

SEXP FARR_subset_assign_double(
        const std::string& filebase, 
        const List& sch, SEXP value){
    
    SEXP idx1 = sch["idx1"];
    SEXP idx1range = sch["idx1range"];
    List idx2s = sch["idx2s"];
    int64_t block_size = (int64_t) (sch["block_size"]);
    IntegerVector partitions = sch["partitions"];
    IntegerVector idx2lens = sch["idx2lens"];
    
    int has_error = -1;
    R_xlen_t idx1len = Rf_xlength(idx1);
    int64_t* idx1rangeptr = (int64_t*) REAL(idx1range);
    int64_t idx1_start = *idx1rangeptr, idx1_end = *(idx1rangeptr + 1);
    
    if( idx1_start == NA_INTEGER64 || idx1_end < 0 || idx1_start < 0 ){
        return( R_NilValue );
    }
    
    int ncores = getThreads();
    if(ncores > idx2s.length()){
        ncores = idx2s.length();
    }
    // ncores = 1;
    
    std::vector<SEXP> buff_pool(ncores);
    std::vector<double*> buff_ptrs(ncores);
    for(int i = 0; i < ncores; i++){
        // TODO: change
        buff_pool[i] = PROTECT(Rf_allocVector(REALSXP, idx1_end - idx1_start + 1));
        buff_ptrs[i] = REAL(buff_pool[i]);
    }
    
    double* value_ptr = REAL(value);
    int64_t* idx1ptr0 = (int64_t*) REAL(idx1);
    
    // std::vector<int64_t*> idx2_ptrs(idx2s.length());
    // for(R_xlen_t ii = 0; ii < idx2s.length(); ii++){
    //     idx2_ptrs[ii] = (int64_t*) REAL(idx2s[ii]);
    // }
    
#pragma omp parallel num_threads(ncores)
{
#pragma omp for schedule(static, 1) nowait
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
            int thread = iter % ncores;
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
                    idx1_start, idx1_end, 
                    idx2_ptr, idx2_len,
                    buff_ptrs[thread], NA_REAL, NA_REAL );
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
        }
    }
}
    
    UNPROTECT(ncores);
    if( has_error >= 0 ){
        stop("Cannot write to partition " + std::to_string(has_error + 1));
    }

    return( R_NilValue );
}

SEXP FARR_subset_assign_float(
        const std::string& filebase, 
        const List& sch, SEXP value){
    
    SEXP idx1 = sch["idx1"];
    SEXP idx1range = sch["idx1range"];
    List idx2s = sch["idx2s"];
    int64_t block_size = (int64_t) (sch["block_size"]);
    IntegerVector partitions = sch["partitions"];
    IntegerVector idx2lens = sch["idx2lens"];
    
    int has_error = -1;
    R_xlen_t idx1len = Rf_xlength(idx1);
    int64_t* idx1rangeptr = (int64_t*) REAL(idx1range);
    int64_t idx1_start = *idx1rangeptr, idx1_end = *(idx1rangeptr + 1);
    
    if( idx1_start == NA_INTEGER64 || idx1_end < 0 || idx1_start < 0 ){
        return( R_NilValue );
    }
    
    int ncores = getThreads();
    if(ncores > idx2s.length()){
        ncores = idx2s.length();
    }
    // ncores = 1;
    
    std::vector<SEXP> buff_pool(ncores);
    std::vector<float*> buff_ptrs(ncores);
    for(int i = 0; i < ncores; i++){
        // TODO: change
        buff_pool[i] = PROTECT(Rf_allocVector(INTSXP, (idx1_end - idx1_start + 1)));
        buff_ptrs[i] = FLOAT(buff_pool[i]);
    }
    
    double* value_ptr = REAL(value);
    int64_t* idx1ptr0 = (int64_t*) REAL(idx1);
    
    // std::vector<int64_t*> idx2_ptrs(idx2s.length());
    // for(R_xlen_t ii = 0; ii < idx2s.length(); ii++){
    //     idx2_ptrs[ii] = (int64_t*) REAL(idx2s[ii]);
    // }
    
#pragma omp parallel num_threads(ncores)
{
#pragma omp for schedule(static, 1) nowait
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
            int thread = iter % ncores;
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
                    idx1_start, idx1_end, 
                    idx2_ptr, idx2_len,
                    buff_ptrs[thread], NA_FLOAT, NA_REAL );
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
        }
    }
}

    UNPROTECT(ncores);
    if( has_error >= 0 ){
        stop("Cannot write to partition " + std::to_string(has_error + 1));
    }
    
    return( R_NilValue );
}

SEXP FARR_subset_assign_raw(
        const std::string& filebase, 
        const List& sch, SEXP value){
    
    SEXP idx1 = sch["idx1"];
    SEXP idx1range = sch["idx1range"];
    List idx2s = sch["idx2s"];
    int64_t block_size = (int64_t) (sch["block_size"]);
    IntegerVector partitions = sch["partitions"];
    IntegerVector idx2lens = sch["idx2lens"];
    
    Rbyte na_raw = 0;
    
    int has_error = -1;
    R_xlen_t idx1len = Rf_xlength(idx1);
    int64_t* idx1rangeptr = (int64_t*) REAL(idx1range);
    int64_t idx1_start = *idx1rangeptr, idx1_end = *(idx1rangeptr + 1);
    
    if( idx1_start == NA_INTEGER64 || idx1_end < 0 || idx1_start < 0 ){
        return( R_NilValue );
    }
    
    int ncores = getThreads();
    if(ncores > idx2s.length()){
        ncores = idx2s.length();
    }
    // ncores = 1;
    
    std::vector<SEXP> buff_pool(ncores);
    std::vector<Rbyte*> buff_ptrs(ncores);
    for(int i = 0; i < ncores; i++){
        // TODO: change
        buff_pool[i] = PROTECT(Rf_allocVector(RAWSXP, idx1_end - idx1_start + 1));
        buff_ptrs[i] = RAW(buff_pool[i]);
    }
    
    Rbyte* value_ptr = RAW(value);
    int64_t* idx1ptr0 = (int64_t*) REAL(idx1);
    
#pragma omp parallel num_threads(ncores)
{
#pragma omp for schedule(static, 1) nowait
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
            int thread = iter % ncores;
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
                    idx1_start, idx1_end, 
                    idx2_ptr, idx2_len,
                    buff_ptrs[thread], na_raw, na_raw );
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
        }
    }
}

    UNPROTECT(ncores);
    if( has_error >= 0 ){
        stop("Cannot write to partition " + std::to_string(has_error + 1));
    }
    
    return( R_NilValue );
}

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

void subset_assign_partition_complex(
        FILE* conn, Rcomplex* value, 
        const R_xlen_t block_size, 
        int64_t* idx1ptr0, R_xlen_t idx1len, 
        int64_t idx1_start, int64_t idx1_end, 
        int64_t* idx2ptr0, R_xlen_t idx2len,
        Rcomplex* buffer1, double* buffer2 ) {
    int elem_size = sizeof(double);
    
    fseek(conn, FARR_HEADER_LENGTH, SEEK_SET);
    
    int64_t* idx1ptr = idx1ptr0;
    int64_t* idx2ptr = idx2ptr0;
    
    Rcomplex* valptr2 = value;
    Rcomplex* buf1 = buffer1;
    double* buf2 = buffer2;
    int64_t buf_size = idx1_end - idx1_start + 1;
    if( buf_size > block_size ){
        buf_size = block_size;
    }
    
    // Rcout << idx2_start << "---\n";
    R_xlen_t idx2ii = 0;
    R_xlen_t idx1ii = 0;
    int64_t start_loc = 0;
    
    for(idx2ii = 0; idx2ii < idx2len; idx2ii++, idx2ptr++){
        
        if(*idx2ptr == NA_INTEGER64){
            continue;
        }
        
        // idx1ptr = (int64_t*) REAL(idx1);
        idx1ptr = idx1ptr0;
        start_loc = (*idx2ptr) * block_size + idx1_start;
        // valptr2 = value + (*idx2ptr) * idx1len;
        
        // load current block
        fseek(conn, start_loc * elem_size + FARR_HEADER_LENGTH, SEEK_SET);
        // buf = buffer;
        lendian_fread(buf2, elem_size, buf_size, conn);
        
        // translate to buf1
        realToCplx(buf2, buf1, buf_size);
        
        for(idx1ii = 0; idx1ii < idx1len; idx1ii++, idx1ptr++, valptr2++){
            // calculate pointer location in the file
            // no check here, but tmp_loc should be >=0
            if(*idx1ptr != NA_INTEGER64){
                *(buffer1 + (*idx1ptr - idx1_start)) = *valptr2;
            }
        }
        cplxToReal(buf1, buf2, buf_size);
        fseek(conn, start_loc * elem_size + FARR_HEADER_LENGTH, SEEK_SET);
        lendian_fwrite(buf2, elem_size, buf_size, conn);
        
    }
    
}


SEXP FARR_subset_assign_complex(
        const std::string& filebase, 
        const List& sch, SEXP value){
    
    SEXP idx1 = sch["idx1"];
    SEXP idx1range = sch["idx1range"];
    List idx2s = sch["idx2s"];
    int64_t block_size = (int64_t) (sch["block_size"]);
    IntegerVector partitions = sch["partitions"];
    IntegerVector idx2lens = sch["idx2lens"];
    
    int has_error = -1;
    R_xlen_t idx1len = Rf_xlength(idx1);
    int64_t* idx1rangeptr = (int64_t*) REAL(idx1range);
    int64_t idx1_start = *idx1rangeptr, idx1_end = *(idx1rangeptr + 1);
    
    if( idx1_start == NA_INTEGER64 || idx1_end < 0 || idx1_start < 0 ){
        return( R_NilValue );
    }
    
    int ncores = getThreads();
    if(ncores > idx2s.length()){
        ncores = idx2s.length();
    }
    // ncores = 1;
    
    std::vector<SEXP> buff_pool(ncores);
    std::vector<SEXP> buff_pool_cplx(ncores);
    for(int i = 0; i < ncores; i++){
        // TODO: change
        buff_pool[i] = PROTECT(Rf_allocVector(REALSXP, idx1_end - idx1_start + 1));
        buff_pool_cplx[i] = PROTECT(Rf_allocVector(CPLXSXP, idx1_end - idx1_start + 1));
    }
    
    Rcomplex* value_ptr = COMPLEX(value);
    int64_t* idx1ptr0 = (int64_t*) REAL(idx1);
    
    // std::vector<int64_t*> idx2_ptrs(idx2s.length());
    // for(R_xlen_t ii = 0; ii < idx2s.length(); ii++){
    //     idx2_ptrs[ii] = (int64_t*) REAL(idx2s[ii]);
    // }
    
#pragma omp parallel num_threads(ncores)
{
#pragma omp for schedule(static, 1) nowait
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
            int thread = iter % ncores;
            try{
                SEXP idx2 = idx2s[iter];
                
                // TODO: change
                // subset_assign_partition_old(
                //     conn, REAL(value) + (idx1len * skips),
                //     block_size, idx1, idx2, REAL(buff_pool[thread]));
                
                int64_t* idx2_ptr = (int64_t*) REAL(idx2);
                R_xlen_t idx2_len = Rf_xlength(idx2);
                Rcomplex* value_ptr2 = value_ptr + (idx1len * skips);
                int64_t* idx1ptr = idx1ptr0;
                // subset_assign_partition(
                //     conn, value_ptr2,
                //     block_size, idx1ptr, idx1len, 
                //     idx1_start, idx1_end, 
                //     idx2_ptr, idx2_len,
                //     buff_ptrs[thread] );
                subset_assign_partition_complex(
                    conn, value_ptr2, 
                    block_size, idx1ptr, idx1len,
                    idx1_start, idx1_end, 
                    idx2_ptr, idx2_len,
                    COMPLEX(buff_pool_cplx[thread]), 
                    REAL(buff_pool[thread]) );
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
        }
    }
}

    UNPROTECT(ncores * 2);
    if( has_error >= 0 ){
        stop("Cannot write to partition " + std::to_string(has_error + 1));
    }
    
    return( R_NilValue );
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
    std::string fbase = correct_filebase(filebase);
    
    int n_protected = 0;
    SEXP value = value_;
    SEXPTYPE type_ = type == FLTSXP ? REALSXP : type;
    if( TYPEOF(value) != type_ ){
        value = PROTECT(Rf_coerceVector(value, type_));
        n_protected = 1;
    }
    
    switch(type) {
    case INTSXP:
        FARR_subset_assign_integer(fbase, sch, value);
        break;
    case REALSXP:
        FARR_subset_assign_double(fbase, sch, value);
        break;
    case FLTSXP:
        FARR_subset_assign_float(fbase, sch, value);
        break;
    case RAWSXP:
        FARR_subset_assign_raw(fbase, sch, value);
        break;
    case LGLSXP:
        FARR_subset_assign_logical(fbase, sch, value);
        break;
    case CPLXSXP:
        FARR_subset_assign_complex(fbase, sch, value);
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
