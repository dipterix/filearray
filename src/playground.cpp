#include <Rcpp.h>
#include "mio.hpp"
using namespace Rcpp;

int handle_error(const std::error_code& error)
{
    const auto& errmsg = error.message();
    std::printf("error mapping file: %s, exiting...\n", errmsg.c_str());
    return error.value();
}

// [[Rcpp::export]]
SEXP timesTwo(std::string path) {
    std::error_code error;
    mio::mmap_sink rw_mmap = mio::make_mmap_sink(
        path, 0, mio::map_entire_file, error);
    
    if (error) {
        handle_error(error);
        return(R_NilValue);
    }
    
    // You can use any iterator based function.
    double* iter = (double*) rw_mmap.begin();
    Rcout << *iter << " " << *(iter+1) << "\n";
    
    // Don't forget to flush changes to disk before unmapping. However, if
    // `rw_mmap` were to go out of scope at this point, the destructor would also
    // automatically invoke `sync` before `unmap`.
    rw_mmap.sync(error);
    if (error) {
        handle_error(error);
        return(R_NilValue);
    }
    
    // We can then remove the mapping, after which rw_mmap will be in a default
    // constructed state, i.e. this and the above call to `sync` have the same
    // effect as if the destructor had been invoked.
    rw_mmap.unmap();
    
    return(R_NilValue);
}


/*** R
x <- filearray::filearray_create(tempfile(), 3:5)
x[] <- 1:60
f <- x$partition_path(1)
timesTwo(f)
*/
