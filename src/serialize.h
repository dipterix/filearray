#ifndef FARR_UNSERIALIZE_H
#define FARR_UNSERIALIZE_H

#include <Rcpp.h>

// buffer type
typedef struct {
    size_t length;
    size_t pos;
    unsigned char *data;
} buffer_t;

int read_byte(R_inpstream_t stream);
void read_bytes(R_inpstream_t stream, void *dst, int length);

SEXP unserialize_raw(SEXP x);
SEXP unserialize_connection(FILE* conn, size_t len);

/**********************************************************
 * Endianess
 ***********************************************************/
bool isLittleEndian();

void swap_endianess(void *ptr, size_t size, size_t nmemb);

size_t lendian_fwrite(void *ptr, size_t size, size_t nmemb, FILE *stream);

size_t lendian_fread(void *ptr, size_t size, size_t nmemb, FILE *stream);

void lendian_assign(void* dst, const void* src, const size_t& size);

#endif // FARR_UNSERIALIZE_H