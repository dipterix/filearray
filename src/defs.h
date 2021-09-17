#ifndef FARR_DEF_H
#define FARR_DEF_H

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
const static Rbyte NA_RBYTE = 2;

#define INT64SXP REALSXP
#define INTEGER64(x) ((int64_t*) REAL(x))

#endif  // FARR_DEF_H