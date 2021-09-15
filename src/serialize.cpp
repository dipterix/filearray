#include "serialize.h"
using namespace Rcpp;

int read_byte(R_inpstream_t stream){
    buffer_t *buf = (buffer_t *)stream->data;
    if (buf->pos >= buf->length) {
        stop("Overflow in `read_byte()`");
    }
    return buf->data[buf->pos++];
}

void read_bytes(R_inpstream_t stream, void *dst, int length) {
    buffer_t *buf = (buffer_t *)stream->data;
    if (buf->pos + length > buf->length) {
        stop("Overflow in `read_bytes()`");
    }
    memcpy(dst, buf->data + buf->pos, length);
    buf->pos += length;
}

SEXP unserialize_raw(SEXP x) {
    if (TYPEOF(x) != RAWSXP) {
        stop("`unserialize_raw` requires raw input");
    }
    Rbyte *vec = RAW(x);
    R_xlen_t len = Rf_xlength(x);
    
    buffer_t *buf = (buffer_t*) malloc(sizeof(buffer_t));
    if (buf == NULL) {
        stop("`unserialize_raw` Cannot allocate memory for buffer");
    }
    buf->length = len;
    buf->pos    = 0;
    buf->data   = (unsigned char *) vec;
    
    // Treat the data buffer as an input stream
    struct R_inpstream_st input_stream;
    
    R_InitInPStream(
        &input_stream,                 
        (R_pstream_data_t) buf,      
        R_pstream_any_format,        
        read_byte, 
        read_bytes, 
        NULL,       
        NULL        
    );
    
    // Unserialize!
    SEXP ret  = PROTECT(R_Unserialize(&input_stream));
    
    free(buf);
    buf = NULL;
    UNPROTECT(1);
    return ret;
}

SEXP unserialize_connection(FILE* conn, size_t len) {
    SEXP raw = PROTECT(Rf_allocVector(RAWSXP, len));
    lendian_fread(RAW(raw), 1, len, conn);
    SEXP re = PROTECT(unserialize_raw(raw));
    UNPROTECT(2);
    return(re);
}

bool isLittleEndian(){
    int x = 1;
    bool is_little = *((char*)&x) == 1;
    return ( is_little );
    // DEBUG test big endianess
    // return(!is_little);
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

