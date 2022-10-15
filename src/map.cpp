#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>
// [[Rcpp::depends(BH)]]

#include "core.h"
#include "serialize.h"
#include "conversion.h"
#include "utils.h"
#include "load.h"
#include "save.h"
#include "openmp.h"
using namespace Rcpp;

// [[Rcpp::export]]
SEXP FARR_buffer_map(
        std::vector<std::string>& input_filebases,
        const std::string& output_filebase,
        const Function& map,
        const int& buffer_nelems,
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
    std::vector<List> metas(narrays);
    std::vector<SEXPTYPE> arr_types(narrays);
    std::vector<SEXPTYPE> file_buffer_types(narrays);
    std::vector<SEXPTYPE> memory_buffer_types(narrays);
    
    std::vector<SEXP> cumparts(narrays);
    std::vector<int64_t> part_lengths(narrays);
    
    SEXP in_dim = R_NilValue;
    
    for(int ii = 0; ii < narrays; ii++){
        std::string fbase = correct_filebase(input_filebases[ii]);
        input_filebases[ii] = fbase;
        List meta = FARR_meta(fbase);
        metas[ii] = meta;
        arr_types[ii] = meta["sexp_type"];
        file_buffer_types[ii] = file_buffer_sxptype(arr_types[ii]);
        memory_buffer_types[ii] = array_memory_sxptype(arr_types[ii]);
        cumparts[ii] = realToInt64_inplace(meta["cumsum_part_sizes"]);
        if( in_dim == R_NilValue ){
            in_dim = meta["dimension"];
            realToInt64_inplace(in_dim);
        }
    }
    
    if( in_dim == R_NilValue ){
        stop("Cannot obtain input dimensions");
    }
    
    R_xlen_t in_ndims = Rf_length(in_dim);
    int64_t* in_dimptr = INTEGER64(in_dim);
    int64_t in_unit_partlen = 1;
    for(R_xlen_t jj = 0; jj <in_ndims - 1; jj++, in_dimptr++){
        in_unit_partlen *= *in_dimptr;
    }
    int64_t in_array_length = in_unit_partlen * *(INTEGER64(in_dim) + (in_ndims - 1));
    
    
    // allocate buffers
    SEXP argbuffers = PROTECT(Rf_allocVector(VECSXP, narrays));
    for(int ii = 0; ii < narrays; ii++){
        SET_VECTOR_ELT(argbuffers, ii, PROTECT(Rf_allocVector(memory_buffer_types[ii], buffer_nelems)));
    }
    
    int64_t current_pos = 0;
    int64_t current_pos_save = 0;
    SEXP tmp = R_NilValue;
    
    if( result_nelems <= 0 ){
        result_nelems = buffer_nelems;
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
    int nprot = 0;
    
    
    for( ; current_pos < in_array_length; current_pos += buffer_nelems ){
        
#pragma omp parallel num_threads(ncores)
{
#pragma omp for schedule(static, 1) nowait
        for(int ii = 0; ii < narrays; ii++){
            FARR_subset_sequential(
                input_filebases[ii],
                in_unit_partlen,
                cumparts[ii],
                arr_types[ii],
                VECTOR_ELT(argbuffers, ii),
                current_pos, buffer_nelems
            );
        }
}   
        
        nprot = 0;
        try{
            tmp = PROTECT(map(argbuffers));
            nprot++;
            
            R_xlen_t tmplen = Rf_xlength(tmp);
            if( result_nelems <= 0 ){
                expected_res_nelem = tmplen;
            } else {
                expected_res_nelem = result_nelems;
                if(tmplen != result_nelems){
                    UNPROTECT(1);
                    stop("Function `map` return length is inconsistent with `result_nelems`");
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
            
        } catch(std::exception &ex){
            UNPROTECT(2 + narrays);
            forward_exception_to_r(ex);
        } catch(...){
            UNPROTECT(2 + narrays);
            stop("Unknown error.");
        }
        
        
    }
    
    UNPROTECT(2 + narrays);
    
    return(R_NilValue);
}

// [[Rcpp::export]]
SEXP FARR_buffer_map2(
        std::vector<std::string>& input_filebases,
        const Function& map,
        const int& buffer_nelems
){
    // prepare inputs
    int narrays = input_filebases.size();
    std::vector<List> metas(narrays);
    std::vector<SEXPTYPE> arr_types(narrays);
    std::vector<SEXPTYPE> file_buffer_types(narrays);
    std::vector<SEXPTYPE> memory_buffer_types(narrays);
    
    std::vector<SEXP> cumparts(narrays);
    std::vector<int64_t> part_lengths(narrays);
    
    SEXP in_dim = R_NilValue;
    
    for(int ii = 0; ii < narrays; ii++){
        std::string fbase = correct_filebase(input_filebases[ii]);
        input_filebases[ii] = fbase;
        List meta = FARR_meta(fbase);
        metas[ii] = meta;
        arr_types[ii] = meta["sexp_type"];
        file_buffer_types[ii] = file_buffer_sxptype(arr_types[ii]);
        memory_buffer_types[ii] = array_memory_sxptype(arr_types[ii]);
        cumparts[ii] = realToInt64_inplace(meta["cumsum_part_sizes"]);
        if( in_dim == R_NilValue ){
            in_dim = meta["dimension"];
            realToInt64_inplace(in_dim);
        }
    }
    
    if( in_dim == R_NilValue ){
        stop("Cannot obtain input dimensions");
    }
    
    R_xlen_t in_ndims = Rf_length(in_dim);
    int64_t* in_dimptr = INTEGER64(in_dim);
    int64_t in_unit_partlen = 1;
    for(R_xlen_t jj = 0; jj <in_ndims - 1; jj++, in_dimptr++){
        in_unit_partlen *= *in_dimptr;
    }
    int64_t in_array_length = in_unit_partlen * *(INTEGER64(in_dim) + (in_ndims - 1));
    
    
    // allocate buffers
    SEXP argbuffers = PROTECT(Rf_allocVector(VECSXP, narrays));
    for(int ii = 0; ii < narrays; ii++){
        SET_VECTOR_ELT(argbuffers, ii, PROTECT(Rf_allocVector(memory_buffer_types[ii], buffer_nelems)));
    }
    
    int64_t current_pos = 0;
    
    int ncores = getThreads();
    if( ncores > narrays ){
        ncores = narrays;
    }
    
    R_xlen_t niters = in_array_length / buffer_nelems;
    if( niters * buffer_nelems < in_array_length ){
        niters++;
    }
    SEXP ret = PROTECT(Rf_allocVector(VECSXP, niters));
    R_xlen_t iter = 0;
    
    for( ; current_pos < in_array_length; current_pos += buffer_nelems, iter++ ){
        
#pragma omp parallel num_threads(ncores)
{
#pragma omp for schedule(static, 1) nowait
        for(int ii = 0; ii < narrays; ii++){
            FARR_subset_sequential(
                input_filebases[ii],
                in_unit_partlen,
                cumparts[ii],
                arr_types[ii],
                VECTOR_ELT(argbuffers, ii),
                current_pos, buffer_nelems
            );
        }
}   

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
# devtools::load_all()
require(filearray)
dim <- 3:5
set.seed(1); 
fbases <- sapply(1:4, function(i){
    file <- tempfile(); unlink(file, recursive = TRUE)
    x <- filearray_create(file, dim, type = 'double')
    x[] <- seq_len(prod(dim))
    x$.filebase
})
set.seed(2); file <- tempfile(); unlink(file, recursive = TRUE)
y <- filearray_create(file, 4:5, type = 'complex')
y$initialize_partition()
    
FARR_buffer_map(
    fbases,
    y$.filebase,
    function(x){
        print(c(x[[1]], sum(x[[1]])))
        sum(x[[1]])
    },
    3L,
    1L
)
res <- FARR_buffer_map2(
    fbases,
    function(x){
        print(c(x[[1]], sum(x[[1]])))
        sum(x[[1]])
    },
    3L,
    1L
)
# y[] - simplify2array(res)

*/
