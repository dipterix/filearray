#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>
// [[Rcpp::depends(BH)]]

#include "core.h"
#include "serialize.h"
#include "conversion.h"
#include "utils.h"
#include "load.h"
#include "save.h"
#include "threadSettings.h"
#include "TinyParallel.h"
using namespace Rcpp;

struct FARRSequentialSubsetter : public TinyParallel::Worker {
    
    const std::vector<std::string>& input_filebases;
    std::vector<int64_t> slice_sizes;
    std::vector<SEXP> cumparts;
    std::vector<SEXPTYPE> arr_types;
    SEXP argbuffers;
    
    // current_pos * buffer_nelems is the pointer position in the array
    int64_t current_pos;
    std::vector<int> buffer_nelems;
    
    FARRSequentialSubsetter(
        const std::vector<std::string>& input_filebases,
        std::vector<int64_t> slice_sizes,
        std::vector<SEXP> cumparts,
        std::vector<SEXPTYPE> arr_types,
        SEXP argbuffers,
        int64_t current_pos,
        std::vector<int> buffer_nelems
    ) : input_filebases(input_filebases), slice_sizes(slice_sizes),
    cumparts(cumparts), arr_types(arr_types), argbuffers(argbuffers),
    current_pos(current_pos), buffer_nelems(buffer_nelems) {}
        
    void operator()(std::size_t begin, std::size_t end) {
        for(std::size_t ii = begin; ii < end; ii++) {
            FARR_subset_sequential(
                input_filebases[ii],
                slice_sizes[ii],
                cumparts[ii],
                arr_types[ii],
                VECTOR_ELT(argbuffers, ii),
                current_pos * buffer_nelems[ii], buffer_nelems[ii]
            );
        }
    }
};


/**
# DIPSAUS DEBUG START
# devtools::load_all()
# e1 <- as_filearray(1:240, dimension = c(2,3,4,10), type = "integer", partition_size = 3L)
# e2 <- as_filearray(rnorm(240), dimension = c(2,3,4,10), type = "double", partition_size = 3L)
# 
# x <- e1+e2
# print(x[][240])
**/
// [[Rcpp::export]]
SEXP FARR_buffer_map(
        std::vector<std::string>& input_filebases,
        const std::string& output_filebase,
        const Function& map,
        std::vector<int>& buffer_nelems,
        int result_nelems = 0
){
    // Prepare outputs
    std::string out_fbase = correct_filebase(output_filebase);
    List out_meta = FARR_meta(out_fbase);
    SEXP out_cumpart = realToInt64_inplace(out_meta["cumsum_part_sizes"]);
    SEXPTYPE out_array_type = out_meta["sexp_type"];
    SEXPTYPE out_file_type = file_buffer_sxptype(out_array_type);
    
    SEXP out_dim = out_meta["dimension"];
    realToInt64_inplace(out_dim);
    R_xlen_t out_ndims = Rf_length(out_dim);
    int64_t* out_dimptr = INTEGER64(out_dim);
    int64_t out_unit_partlen = 1;
    for(R_xlen_t jj = 0; jj <out_ndims - 1; jj++, out_dimptr++){
        out_unit_partlen *= *out_dimptr;
    }
    
    // prepare inputs
    int narrays = input_filebases.size();
    
    if(buffer_nelems.size() - narrays < 0) {
        Rcpp::stop("C++: `FARR_buffer_map`: vector buffer_nelems is too short. The length must be consistent with number of input arrays.");
    }
    
    std::vector<List> metas(narrays);
    std::vector<SEXPTYPE> arr_types(narrays);
    std::vector<SEXPTYPE> file_buffer_types(narrays);
    std::vector<SEXPTYPE> memory_buffer_types(narrays);
    
    std::vector<SEXP> cumparts(narrays);
    std::vector<int64_t> slice_sizes(narrays);
    std::vector<int64_t> input_lens(narrays);
    
    // dimensions of the first input array
    SEXP in_dim = R_NilValue;
    R_xlen_t in_ndims;
    int64_t* in_dimptr;
    
    for(int ii = 0; ii < narrays; ii++){
        std::string fbase = correct_filebase(input_filebases[ii]);
        input_filebases[ii] = fbase;
        List meta = FARR_meta(fbase);
        metas[ii] = meta;
        arr_types[ii] = meta["sexp_type"];
        file_buffer_types[ii] = file_buffer_sxptype(arr_types[ii]);
        memory_buffer_types[ii] = array_memory_sxptype(arr_types[ii]);
        cumparts[ii] = realToInt64_inplace(meta["cumsum_part_sizes"]);
        
        in_dim = meta["dimension"];
        if( in_dim == R_NilValue ){
            stop("Cannot obtain dimensions from the inputs");
        }
        realToInt64_inplace(in_dim);
        in_ndims = Rf_length(in_dim);
        in_dimptr = INTEGER64(in_dim);
        
        // slice length
        slice_sizes[ii] = 1;
        for(R_xlen_t jj = 0; jj <in_ndims - 1; jj++, in_dimptr++){
            slice_sizes[ii] *= *in_dimptr;
        }
        in_dimptr = INTEGER64(in_dim) + (in_ndims - 1);
        
        // length of input array
        input_lens[ii] = slice_sizes[ii] * (*in_dimptr);
    }
    
    
    // allocate buffers
    SEXP argbuffers = PROTECT(Rf_allocVector(VECSXP, narrays));
    for(int ii = 0; ii < narrays; ii++){
        SET_VECTOR_ELT(argbuffers, ii, PROTECT(Rf_allocVector(memory_buffer_types[ii], buffer_nelems[ii])));
    }
    
    int64_t current_pos = 0;
    int64_t current_pos_save = 0;
    SEXP tmp = R_NilValue;
    
    if( result_nelems <= 0 ){
        result_nelems = buffer_nelems[0];
    }
    R_xlen_t expected_res_nelem = result_nelems;
    
    int ncores = getThreads();
    if( ncores > narrays ){
        ncores = narrays;
    }
    
    // SEXP convert_as2(SEXP x, SEXP y, SEXPTYPE type)
    SEXP tmp_val = PROTECT(Rf_allocVector(out_file_type, result_nelems));
    
    
    
    /**
     * Repeat meself
     */
    
    FARRSequentialSubsetter seqsubsetter(
            input_filebases, slice_sizes, cumparts,
            arr_types, argbuffers, 0, buffer_nelems
    );
    
    
    try{
    
        for( ; current_pos < input_lens[0]; current_pos += buffer_nelems[0] ){
            
            seqsubsetter.current_pos = current_pos / buffer_nelems[0]; 
            TinyParallel::parallelFor(0, narrays, seqsubsetter);
        
        
            tmp = PROTECT(map(argbuffers));
            
            R_xlen_t tmplen = Rf_xlength(tmp);
            if( result_nelems <= 0 ){
                expected_res_nelem = tmplen;
            } else {
                expected_res_nelem = result_nelems;
                if(tmplen - expected_res_nelem != 0){
                    // Rcout << tmplen << " vs. " << result_nelems << "\n";
                    UNPROTECT(1);
                    stop(
                        "Function `map` return length is inconsistent with `result_nelems` (expected: " + 
                            std::to_string(expected_res_nelem) + ", actual: " + 
                            std::to_string(tmplen) + ")"
                    );
                }
            }
            convert_as2(tmp, tmp_val, out_array_type);
            
            UNPROTECT(1); // tmp
            
            FARR_subset_assign_sequential_bare(
                out_fbase, out_unit_partlen,
                out_cumpart, out_array_type,
                tmp_val, current_pos_save
            );
            current_pos_save += expected_res_nelem;
        
        }
    } catch(std::exception &ex){
        UNPROTECT(2 + narrays);
        forward_exception_to_r(ex);
    } catch(...){
        // UNPROTECT(2 + narrays);
        Rcpp::warning("C++ `FARR_buffer_map`: cannot finish map");
    }
    
    UNPROTECT(2 + narrays);
    
    return(R_NilValue);
}

// [[Rcpp::export]]
SEXP FARR_buffer_map2(
        std::vector<std::string>& input_filebases,
        const Function& map,
        std::vector<int>& buffer_nelems
){
    // prepare inputs
    int narrays = input_filebases.size();
    if(buffer_nelems.size() - narrays < 0) {
        Rcpp::stop("C++: `FARR_buffer_map2`: vector buffer_nelems is too short. The length must be consistent with number of input arrays.");
    }
    
    std::vector<List> metas(narrays);
    std::vector<SEXPTYPE> arr_types(narrays);
    std::vector<SEXPTYPE> file_buffer_types(narrays);
    std::vector<SEXPTYPE> memory_buffer_types(narrays);
    
    std::vector<SEXP> cumparts(narrays);
    std::vector<int64_t> slice_sizes(narrays);
    std::vector<int64_t> input_lens(narrays);
    
    // dimensions of the first input array
    SEXP in_dim = R_NilValue;
    R_xlen_t in_ndims;
    int64_t* in_dimptr;
    
    for(int ii = 0; ii < narrays; ii++){
        std::string fbase = correct_filebase(input_filebases[ii]);
        input_filebases[ii] = fbase;
        List meta = FARR_meta(fbase);
        metas[ii] = meta;
        arr_types[ii] = meta["sexp_type"];
        file_buffer_types[ii] = file_buffer_sxptype(arr_types[ii]);
        memory_buffer_types[ii] = array_memory_sxptype(arr_types[ii]);
        cumparts[ii] = realToInt64_inplace(meta["cumsum_part_sizes"]);
        
        in_dim = meta["dimension"];
        if( in_dim == R_NilValue ){
            stop("Cannot obtain dimensions from the inputs");
        }
        realToInt64_inplace(in_dim);
        in_ndims = Rf_length(in_dim);
        in_dimptr = INTEGER64(in_dim);
        
        // slice length
        slice_sizes[ii] = 1;
        for(R_xlen_t jj = 0; jj <in_ndims - 1; jj++, in_dimptr++){
            slice_sizes[ii] *= *in_dimptr;
        }
        in_dimptr = INTEGER64(in_dim) + (in_ndims - 1);
        
        // length of input array
        input_lens[ii] = slice_sizes[ii] * (*in_dimptr);
    }
    
    
    // allocate buffers
    SEXP argbuffers = PROTECT(Rf_allocVector(VECSXP, narrays));
    for(int ii = 0; ii < narrays; ii++){
        SET_VECTOR_ELT(argbuffers, ii, PROTECT(Rf_allocVector(memory_buffer_types[ii], buffer_nelems[ii])));
    }
    
    int64_t current_pos = 0;
    
    int ncores = getThreads();
    if( ncores > narrays ){
        ncores = narrays;
    }
    
    R_xlen_t niters = input_lens[0] / buffer_nelems[0];
    if( niters * buffer_nelems[0] < input_lens[0] ){
        niters++;
    }
    SEXP ret = PROTECT(Rf_allocVector(VECSXP, niters));
    R_xlen_t iter = 0;
    
    FARRSequentialSubsetter seqsubsetter(
            input_filebases, slice_sizes, cumparts,
            arr_types, argbuffers, 0, buffer_nelems
    );
    
    
    // current_pos < input_lens[0]; 
    for( iter = 0, current_pos = 0 ; 
         iter < niters;
         current_pos += buffer_nelems[0], iter++ ){
        
        seqsubsetter.current_pos = iter;
        TinyParallel::parallelFor(0, narrays, seqsubsetter);

        try{
            SET_VECTOR_ELT(ret, iter, Shield<SEXP>(map(argbuffers)));
        } catch(std::exception &ex){
            UNPROTECT(2 + narrays);
            forward_exception_to_r(ex);
        } catch(...){
            UNPROTECT(2 + narrays);
            stop("Unknown error.");
        }


    }
    
    UNPROTECT(2 + narrays);
    
    return(ret);
}

/*** R
devtools::load_all()
e1 <- as_filearray(1:240, dimension = c(2,3,4,10), type = "integer", partition_size = 3L)
e2 <- as_filearray(rnorm(240), dimension = c(2,3,4,10), type = "double", partition_size = 3L)

x <- e1+e2
invisible(x[])

tmp <- integer(240)
current_pos <- 0L
FARR_subset_sequential(
    x$.filebase,
    24L,
    bit64::as.integer64(c(3,6, 9, 10)),
    13L,
    tmp,
    current_pos, 24L
);

# # devtools::load_all()
# require(filearray)
# dim <- 3:5
# set.seed(1); 
# fbases <- sapply(1:4, function(i){
#     file <- tempfile(); unlink(file, recursive = TRUE)
#     x <- filearray_create(file, dim, type = 'double')
#     x[] <- seq_len(prod(dim))
#     x$.filebase
# })
# set.seed(2); file <- tempfile(); unlink(file, recursive = TRUE)
# y <- filearray_create(file, 4:5, type = 'complex')
# y$initialize_partition()
#     
# FARR_buffer_map(
#     fbases,
#     y$.filebase,
#     function(x){
#         print(c(x[[1]], sum(x[[1]])))
#         sum(x[[1]])
#     },
#     3L,
#     1L
# )
# res <- FARR_buffer_map2(
#     fbases,
#     function(x){
#         print(c(x[[1]], sum(x[[1]])))
#         sum(x[[1]])
#     },
#     3L,
#     1L
# )
# # y[] - simplify2array(res)

*/
