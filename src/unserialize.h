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

#endif // FARR_UNSERIALIZE_H