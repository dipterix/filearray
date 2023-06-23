#include "threadSettings.h"

#include <tthread/tinythread.h>
#include <TinyParallel.h>

// [[Rcpp::export]]
SEXP getDefaultNumThreads() {
    SEXP threadsSEXP = PROTECT(Rf_allocVector(INTSXP, 1));
    INTEGER(threadsSEXP)[0] = tthread::thread::hardware_concurrency();
    UNPROTECT(1);
    return threadsSEXP;
}

// [[Rcpp::export]]
int getThreads(const bool& max){
    int maxThreads = tthread::thread::hardware_concurrency();
    if( max ) {
        return( maxThreads );
    }
    int n = TinyParallel::resolveValue("FILEARRAY_NUM_THREADS", -1, maxThreads);
    if( n <= 0 || n > maxThreads ) {
        n = maxThreads;
    }
    return( n );
}
