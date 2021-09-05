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
 * Buffer size
 ***********************************************************/
static int nbuffers = 65536;

int system_buffer_size();

int set_buffer_size(int size);

int get_buffer_size();

int kinda_sorted(SEXP idx, int64_t min_, int64_t buffer_count);

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


/**********************************************************
 * Converts locations [A, B, C] to vector indices
 ***********************************************************/
SEXP realToUint64(NumericVector x, const double min_, const double max_, const int strict);

SEXP seq_len_int64(const R_xlen_t len);

SEXP locationList(const SEXP listOrEnv, const NumericVector& dim, const int strict);

SEXP addCycle(SEXP x, SEXP ret, const R_xlen_t step = 1, const R_xlen_t mag = 1);

SEXP loc2idx(const List sliceIdx, const NumericVector& dim);

SEXP reshape_or_drop(SEXP x, SEXP reshape, bool drop);

/**********************************************************
 * Schedule reading
 ***********************************************************/
List schedule(const SEXP listOrEnv, 
              const NumericVector& dim,
              const NumericVector& cum_part_sizes,
              const int split_dim, const int strict = 1);

/**********************************************************
 * Read partition
 ***********************************************************/
template <typename T>
inline void subset_partition(
        FILE* conn, void* buffer, int buffer_bytes,
        T* retptr, const R_xlen_t block_size, 
        SEXP idx1, int64_t idx1_start, int64_t idx1_end,
        SEXP idx2, int64_t idx2_start, int64_t idx2_end,
        const int idx1_sorted, const int idx2_sorted,
        int swap_endian = 0
) {
    // TODO: swap_endian
    double content_size = 0;
    int elem_size = sizeof(T);
    R_xlen_t buffer_size = buffer_bytes / elem_size;
    if( buffer_size > block_size ){
        buffer_size = block_size;
    }
    T* bufferptr = (T*) buffer;
    
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
                
                *(retptr2 + jj) = *(bufferptr + ll);
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
                memcpy(retptr3, retptr2, elem_size * idx1len);
            } else if( idx2_sorted && *idx2ptr > block ){
                break;
            }
        }
        // Rcout << "2\n";
    }
}



/**********************************************************
 * Write partition
 * Assuming partition has been initialized
 ***********************************************************/
template <typename T>
inline void subset_assign_partition(
        FILE* conn, T* value, const R_xlen_t block_size, 
        int64_t* idx1ptr0, R_xlen_t idx1len,
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