#ifndef FARR_SAVE_H
#define FARR_SAVE_H

#include <Rcpp.h>
/**
 * @param filebase root path of array, must be absolute and "corrected" (ends with "/" or "\\", depending on system)
 * @param sch returned from `schedule`, scheduled indices
 * @param value value to set, must be coerced
 * @param buff_ptrs buffer points: length of `buff_ptrs` are the number of cores; each element must be at least buffer size in bytes (get_buffer_size())
 */
SEXP FARR_subset_assign_internal(
        const std::string& fbase,
        const Rcpp::List sch, 
        const SEXPTYPE type,
        std::vector<SEXP>& buff_pool,
        SEXP value);

SEXP FARR_subset_assign2(
        const std::string& filebase,
        SEXP value,
        const SEXP listOrEnv,
        const size_t thread_buffer = 2097152,
        int split_dim = 0
);

#endif //FARR_SAVE_H
    
