#ifndef FARR_COMMON_H
#define FARR_COMMON_H

#include <Rcpp.h>

using namespace Rcpp;

#ifndef NA_INTEGER64
#define NA_INTEGER64 LLONG_MIN
#endif

#ifndef FARR_HEADER_LENGTH
#define FARR_HEADER_LENGTH 1024
#endif

/**********************************************************
 * Extended types
 ***********************************************************/

#define FLTSXP 26
#define FLOAT(x) ((float*) INTEGER(x))

const static float NA_FLOAT = NAN;


#define INT64SXP REALSXP
#define INTEGER64(x) ((int64_t*) REAL(x))



/**********************************************************
 * Buffer
 ***********************************************************/

int system_buffer_size();

int set_buffer_size(int size);

int get_buffer_size();

int kinda_sorted(SEXP idx, int64_t min_, int64_t buffer_count);

void set_buffer(SEXP dim, int elem_size, size_t buffer_bytes, int split_dim);

SEXPTYPE file_buffer_sxptype(SEXPTYPE array_type);

/**********************************************************
 * Endianess
 ***********************************************************/
bool isLittleEndian();

void swap_endianess(void *ptr, size_t size, size_t nmemb);

size_t lendian_fwrite(void *ptr, size_t size, size_t nmemb, FILE *stream);

size_t lendian_fread(void *ptr, size_t size, size_t nmemb, FILE *stream);


/**********************************************************
 * Utils
 ***********************************************************/

double prod_double(const NumericVector& x);

SEXP check_missing_dots(const SEXP env);

void realToCplx(double* x, Rcomplex* y, size_t nelem);

void cplxToReal(Rcomplex* x, double* y, size_t nelem);

void realToFloat(double* x, float* y, size_t nelem);

void floatToReal(float* x, double* y, size_t nelem);

double na_cplx_dbl();

SEXP sub_vec(SEXP x, SEXP idx_int64);

std::string correct_filebase(const std::string& filebase);

SEXP reshape_or_drop(SEXP x, SEXP reshape, bool drop);

SEXP subset_dimnames(SEXP dimnames, SEXP sliceIdx);

SEXP convert_as(SEXP x, SEXPTYPE type);

/**********************************************************
 * Converts locations [A, B, C] to vector indices
 ***********************************************************/
SEXP realToInt64(NumericVector x, const double min_ = NA_REAL, const double max_ = NA_REAL, const int strict = 1);
SEXP realToInt64_inplace(SEXP x, const double min_ = NA_REAL, const double max_ = NA_REAL, const int strict = 1);

SEXP seq_len_int64(const R_xlen_t len);

SEXP locationList(const SEXP listOrEnv, const NumericVector& dim, const int strict);

SEXP addCycle(SEXP x, SEXP ret, const R_xlen_t step = 1, const R_xlen_t mag = 1);

SEXP loc2idx(const List sliceIdx, const NumericVector& dim);

/**********************************************************
 * Schedule reading
 ***********************************************************/
List schedule(const SEXP listOrEnv, 
              const NumericVector& dim,
              const NumericVector& cum_part_sizes,
              const int split_dim, const int strict = 1);

int guess_splitdim(SEXP dim, int elem_size, size_t buffer_bytes);



/**********************************************************
 * Write partition
 * Assuming partition has been initialized
 ***********************************************************/
template <typename T>
inline void subset_assign_partition(
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
inline void subset_assign_partition_old(
        FILE* conn, T* value, const R_xlen_t block_size, 
        SEXP idx1, SEXP idx2, T* buffer ) {
    // TODO: swap_endian
    int elem_size = sizeof(T);
    
    fseek(conn, FARR_HEADER_LENGTH, SEEK_SET);
    
    int64_t* idx1ptr = (int64_t*) REAL(idx1);
    R_xlen_t idx1len = Rf_xlength(idx1);
    
    int64_t* idx2ptr = (int64_t*) REAL(idx2);
    R_xlen_t idx2len = Rf_xlength(idx2);
    
    T* valptr2 = value;
    T* buf = buffer;
    
    // Rcout << idx2_start << "---\n";
    R_xlen_t idx2ii = 0;
    R_xlen_t idx1ii = 0;
    int64_t start_loc = 0;
    
    for(idx2ii = 0; idx2ii < idx2len; idx2ii++, idx2ptr++){
        
        if(*idx2ptr == NA_INTEGER64){
            continue;
        }
        
        idx1ptr = (int64_t*) REAL(idx1);
        start_loc = (*idx2ptr) * block_size;
        // valptr2 = value + (*idx2ptr) * idx1len;
        
        // load current block
        fseek(conn, start_loc * elem_size + FARR_HEADER_LENGTH, SEEK_SET);
        // buf = buffer;
        lendian_fread(buf, elem_size, block_size, conn);
        
        for(idx1ii = 0; idx1ii < idx1len; idx1ii++, idx1ptr++, valptr2++){
            // calculate pointer location in the file
            // no check here, but tmp_loc should be >=0
            if(*idx1ptr != NA_INTEGER64){
                *(buffer + (*idx1ptr)) = *valptr2;
            }
        }
        fseek(conn, start_loc * elem_size + FARR_HEADER_LENGTH, SEEK_SET);
        lendian_fwrite(buf, elem_size, block_size, conn);
    }
    
}


#endif // FARR_COMMON_H
