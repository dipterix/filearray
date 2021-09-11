#ifndef FARR_SAVE_H
#define FARR_SAVE_H

#include <Rcpp.h>

/**
 * @param filebase root path of array, must be absolute and "corrected" (ends with "/" or "\\", depending on system)
 * @param sch returned from `schedule`, scheduled indices
 * @param value value to set, must be coerced
 * @param buff_ptrs buffer points: length of `buff_ptrs` are the number of cores; each element must be at least buffer size in bytes (get_buffer_size())
 */
SEXP FARR_subset_assign_integer(
        const std::string& filebase, 
        const Rcpp::List& sch, SEXP value,
        const std::vector<int*>& buff_ptrs);

// SEXP FARR_subset_assign_integer(
//         const std::string& filebase, 
//         const Rcpp::List& sch, SEXP value,
//         const std::vector<int*>& buff_ptrs);

#endif //FARR_SAVE_H
    
