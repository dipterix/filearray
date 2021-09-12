#include "openmp.h"
#include "serialize.h"
#include "core.h"
#include "utils.h"
#include "conversion.h"
#include "save.h"
using namespace Rcpp;

template <typename T>
void subset_assign_partition(
        FILE* conn, T* value, const R_xlen_t block_size, 
        int64_t* idx1ptr0, R_xlen_t idx1len, 
        int64_t idx1_start, int64_t idx1_end, 
        int64_t* idx2ptr0, R_xlen_t idx2len,
        T* buffer ) {
    // TODO: swap_endian
    int elem_size = sizeof(T);
    
    fseek(conn, FARR_HEADER_LENGTH, SEEK_SET);
    
    // int64_t* idx1ptr = (int64_t*) REAL(idx1);
    // R_xlen_t idx1len = Rf_xlength(idx1);
    
    // int64_t* idx2ptr = (int64_t*) REAL(idx2);
    // R_xlen_t idx2len = Rf_xlength(idx2);
    
    int64_t* idx1ptr = idx1ptr0;
    int64_t* idx2ptr = idx2ptr0;
    
    T* valptr2 = value;
    T* buf = buffer;
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
        lendian_fread(buf, elem_size, buf_size, conn);
        
        for(idx1ii = 0; idx1ii < idx1len; idx1ii++, idx1ptr++, valptr2++){
            // calculate pointer location in the file
            // no check here, but tmp_loc should be >=0
            if(*idx1ptr != NA_INTEGER64){
                *(buffer + (*idx1ptr - idx1_start)) = (*valptr2);
            }
        }
        fseek(conn, start_loc * elem_size + FARR_HEADER_LENGTH, SEEK_SET);
        lendian_fwrite(buf, elem_size, buf_size, conn);
        
    }
    
}

template <typename T>
SEXP FARR_subset_assign_template(
        const std::string& filebase, 
        const List& sch, T* value_ptr,
        const std::vector<T*>& buff_ptrs){
    
    SEXP idx1 = sch["idx1"];
    SEXP idx1range = sch["idx1range"];
    List idx2s = sch["idx2s"];
    int64_t block_size = (int64_t) (sch["block_size"]);
    IntegerVector partitions = sch["partitions"];
    IntegerVector idx2lens = sch["idx2lens"];
    
    int has_error = -1;
    
    R_xlen_t idx1len = Rf_xlength(idx1);
    
    // Check whether indices are valid
    int64_t* idx1rangeptr = (int64_t*) REAL(idx1range);
    int64_t idx1_start = *idx1rangeptr, idx1_end = *(idx1rangeptr + 1);
    
    if( idx1_start == NA_INTEGER64 || idx1_end < 0 || idx1_start < 0 ){
        return( R_NilValue );
    }
    
    
    int ncores = buff_ptrs.size();
    if(ncores > idx2s.length()){
        ncores = idx2s.length();
    }
    
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
                T* value_ptr2 = value_ptr + (idx1len * skips);
                int64_t* idx1ptr = idx1ptr0;
                subset_assign_partition(
                    conn, value_ptr2,
                    block_size, idx1ptr, idx1len, 
                    idx1_start, idx1_end, 
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
        }
    }
}

    // UNPROTECT(ncores);
    if( has_error >= 0 ){
        stop("Cannot write to partition " + std::to_string(has_error + 1));
    }
    
    return( R_NilValue );
}

SEXP FARR_subset_assign_internal(
        const std::string& fbase,
        const List sch, 
        const SEXPTYPE type,
        std::vector<SEXP>& buff_pool,
        SEXP value){
    
    int ncores = buff_pool.size();
    
    
    switch(type) {
    case INTSXP: {
        std::vector<int*> buff_ptrs(ncores);
        for(int i = 0; i < ncores; i++){
            buff_ptrs[i] = INTEGER(buff_pool[i]);
        }
        FARR_subset_assign_template(fbase, sch, INTEGER(value), buff_ptrs);
        break;
    }
    case CPLXSXP:
    case REALSXP: {
        std::vector<double*> buff_ptrs(ncores);
        for(int i = 0; i < ncores; i++){
            buff_ptrs[i] = REAL(buff_pool[i]);
        }
        FARR_subset_assign_template(fbase, sch, REAL(value), buff_ptrs);
        break;
    }
    case FLTSXP: {
        std::vector<float*> buff_ptrs(ncores);
        for(int i = 0; i < ncores; i++){
            buff_ptrs[i] = FLOAT(buff_pool[i]);
        }
        FARR_subset_assign_template(fbase, sch, FLOAT(value), buff_ptrs);
        break;
    }
    case LGLSXP:
    case RAWSXP: {
        std::vector<Rbyte*> buff_ptrs(ncores);
        for(int i = 0; i < ncores; i++){
            buff_ptrs[i] = RAW(buff_pool[i]);
        }
        FARR_subset_assign_template(fbase, sch, RAW(value), buff_ptrs);
        break;
    }
    default:
        stop("SEXP type not supported.");
    }
    return( R_NilValue );
}

// [[Rcpp::export]]
SEXP FARR_subset_assign2(
        const std::string& filebase,
        SEXP value,
        const SEXP listOrEnv,
        const size_t thread_buffer,
        int split_dim
) {
    // Get meta information
    const std::string fbase = correct_filebase(filebase);
    List meta = FARR_meta(fbase);
    const int elem_size = meta["elem_size"];
    const SEXPTYPE sexp_type = meta["sexp_type"];
    SEXP dim = meta["dimension"]; // double
    SEXP cum_part_size = meta["cumsum_part_sizes"];
    
    // calculate split_dim
    R_len_t ndims = Rf_length(dim);
    if( split_dim == NA_INTEGER || split_dim == 0 ){
        split_dim = guess_splitdim(dim, elem_size, thread_buffer);
    } else if (split_dim < 1 || split_dim > ndims-1 ){
        stop("Incorrect `split_dim`: must be an integer from 1 to ndims-1 ");
    }
    set_buffer(dim, elem_size, thread_buffer, split_dim);
    
    // schedule indices
    List sch = schedule(listOrEnv, dim, cum_part_size,
                        split_dim, 1);
    
    // Check whether indices are valid
    SEXP idx1range = sch["idx1range"];
    int64_t* idx1rangeptr = (int64_t*) REAL(idx1range);
    int64_t idx1_start = *idx1rangeptr, idx1_end = *(idx1rangeptr + 1);
    
    if( idx1_start == NA_INTEGER64 || idx1_end < 0 || idx1_start < 0 ){
        return( R_NilValue );
    }
    
    // coerce vector to desired SEXP type
    SEXP value_ = PROTECT(convert_as(value, sexp_type));
    SEXPTYPE valtype = TYPEOF(value_);
    
    // allocate buffers
    int ncores = getThreads();
    std::vector<SEXP> buff_pool(ncores);
    for(int i = 0; i < ncores; i++){
        buff_pool[i] = PROTECT(Rf_allocVector(
            valtype, idx1_end - idx1_start + 1));
    }
    
    FARR_subset_assign_internal(fbase, sch, sexp_type, buff_pool, value_);
    
    UNPROTECT( 1 + ncores );
    return(R_NilValue);
    
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

# dim <- c(100,100,100,100)
# x <- filearray_create(file, dim, type = 'double')
# tmp <- as.double(seq_len(1e8))
# setThreads(3)
# system.time({
#     x[] <- tmp
# }, gcFirst = TRUE)
# identical(as.vector(x[]), as.double(tmp))
# pryr::object_size(x)
*/
