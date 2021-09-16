#ifndef FARR_SAVE_H
#define FARR_SAVE_H

#include "common.h"
/**
 * @param filebase root path of array, must be absolute and "corrected" (ends with "/" or "\\", depending on system)
 * @param sch returned from `schedule`, scheduled indices
 * @param value value to set, must be coerced
 * @param buff_ptrs buffer points: length of `buff_ptrs` are the number of cores; each element must be at least buffer size in bytes (get_buffer_size())
 */

SEXP FARR_subset_assign2(
        const std::string& filebase,
        SEXP value,
        const SEXP listOrEnv,
        const size_t thread_buffer,
        int split_dim
);

SEXP FARR_subset_assign_sequential(
        const std::string& filebase, 
        const int64_t& unit_partlen, 
        SEXP cum_partsizes, 
        SEXPTYPE array_type,
        SEXP value, 
        const int64_t from
);

SEXP FARR_subset_assign_sequential_bare(
        const std::string& filebase, 
        const int64_t& unit_partlen, 
        SEXP cum_partsizes, 
        SEXPTYPE array_type,
        SEXP value_, 
        const int64_t from
);

#endif //FARR_SAVE_H
    
