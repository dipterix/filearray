#include "common.h"
#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>
// [[Rcpp::depends(BH)]]

#include "openmp.h"
#include "serialize.h"
#include "core.h"
#include "utils.h"
#include "conversion.h"
#include "save.h"
using namespace Rcpp;

SEXP FARR_subset_assign_sequential_bare(
        const std::string& filebase, 
        const int64_t& unit_partlen, 
        SEXP cum_partsizes, 
        SEXPTYPE array_type,
        SEXP value_, 
        const int64_t from
) {
    R_xlen_t len = Rf_xlength(value_);
    
    // print(value_);
    
    int file_buffer_elemsize = file_element_size(array_type);
    std::string fbase = correct_filebase(filebase);
    R_len_t nparts = Rf_length(cum_partsizes);
    
    // calculate the first partition
    int64_t slice_idx1 = 0;
    int64_t slice_idx2 = 0;
    int64_t tmp = 0;
    for(; tmp <= from; tmp+= unit_partlen, slice_idx1++){}
    for(slice_idx2 = slice_idx1; tmp < from + len; tmp+= unit_partlen, slice_idx2++){}
    
    int part_start = 0;
    int part_end = 0;
    int64_t skip_start = 0;
    int64_t skip_end = 0;
    
    int64_t* cum_part = INTEGER64(cum_partsizes);
    for(; slice_idx1 > *cum_part; cum_part++, part_start++){}
    if( part_start == 0 ){
        skip_start = from;
    } else {
        skip_start = from - (*(cum_part - 1)) * unit_partlen;
    }
    for(part_end = part_start; slice_idx2 > *cum_part; cum_part++, part_end++){}
    skip_end = (*cum_part) * unit_partlen - (from + len);
    
    int64_t read_start = 0;
    int64_t write_len = 0;
    int64_t part_nelem = 0;
    int64_t last_part_nelem = 0;
    cum_part = INTEGER64(cum_partsizes);
    
    int64_t nwrite = 0;
    
    const boost::interprocess::mode_t mode = boost::interprocess::read_write;
    
    for(int part = part_start; part <= part_end; part++, cum_part++, nwrite += write_len){
        // Rcout << part << "\n";
        if( part >= nparts ){
            continue;
        }
        // get partition n_elems
        part_nelem = (*cum_part) * unit_partlen - last_part_nelem;
        last_part_nelem = (*cum_part) * unit_partlen;
        
        // skip read_start elements
        read_start = 0;
        if( part == part_start ) {
            read_start = skip_start;
        }
        // Rcout << part_nelem << "--\n";
        // then read read_len elements
        write_len = part_nelem - read_start;
        if( part == part_end ){
            write_len -= skip_end;
        }
        
        std::string part_file = fbase + std::to_string(part) + ".farr";
        
        try{
            boost::interprocess::file_mapping fm(part_file.c_str(), mode);
            boost::interprocess::mapped_region region(
                    fm, mode, 
                    FARR_HEADER_LENGTH + file_buffer_elemsize * read_start, 
                    file_buffer_elemsize * write_len);
            region.advise(boost::interprocess::mapped_region::advice_sequential);
            unsigned char* begin = static_cast<unsigned char*>(region.get_address());
            switch(array_type) {
            case REALSXP: {
                lendian_assign(begin, REAL(value_) + nwrite, file_buffer_elemsize, write_len);
                // lendian_fwrite(REAL(value_) + nwrite, file_buffer_elemsize, write_len, conn);
                break;
            }
            case INTSXP: {
                lendian_assign(begin, INTEGER(value_) + nwrite, file_buffer_elemsize, write_len);
                // lendian_fwrite(INTEGER(value_) + nwrite, file_buffer_elemsize, write_len, conn);
                break;
            }
            case RAWSXP: {
                lendian_assign(begin, RAW(value_) + nwrite, file_buffer_elemsize, write_len);
                // lendian_fwrite(RAW(value_) + nwrite, file_buffer_elemsize, write_len, conn);
                break;
            }
            case FLTSXP: {
                lendian_assign(begin, FLOAT(value_) + nwrite, file_buffer_elemsize, write_len);
                // lendian_fwrite(FLOAT(value_) + nwrite, file_buffer_elemsize, write_len, conn);
                break;
            }
            case LGLSXP: {
                lendian_assign(begin, RAW(value_) + nwrite, file_buffer_elemsize, write_len);
                // lendian_fwrite(RAW(value_) + nwrite, file_buffer_elemsize, write_len, conn);
                break;
            }
            case CPLXSXP: {
                lendian_assign(begin, REAL(value_) + nwrite, file_buffer_elemsize, write_len);
                // lendian_fwrite(REAL(value_) + nwrite, file_buffer_elemsize, write_len, conn);
                break;
            }
            default: {
                stop("Unsupported SEXP type");
            }
            }
            // region.flush();
        } catch(std::exception &ex){
            stop("Error while writing sequential to partition " +
                std::to_string(part + 1) + ". Reason: " + ex.what());
        } catch(...){
            stop("Error while writing sequential to partition " +
                std::to_string(part + 1) + ". (Unknown error)");
        }
        
    }
    
    return(R_NilValue);
}

// [[Rcpp::export]]
SEXP FARR_subset_assign_sequential(
        const std::string& filebase, 
        const int64_t& unit_partlen, 
        SEXP cum_partsizes, 
        SEXPTYPE array_type,
        SEXP value, 
        const int64_t from
) {
    SEXP value_ = PROTECT(convert_as(value, array_type));
    FARR_subset_assign_sequential_bare(
        filebase, unit_partlen,
        cum_partsizes, array_type,
        value_, from
    );
    UNPROTECT(1);
    return(R_NilValue);
}


template <typename T>
void subset_assign_partition(
        char* conn0, T* value, const R_xlen_t block_size, 
        int64_t* idx1ptr0, R_xlen_t idx1len, 
        int64_t idx1_start, int64_t idx2_start, 
        int64_t* idx2ptr0, R_xlen_t idx2len ) {
    // TODO: swap_endian
    int elem_size = sizeof(T);
    
    // fseek(conn, FARR_HEADER_LENGTH, SEEK_SET);
    
    int64_t* idx1ptr = idx1ptr0;
    int64_t* idx2ptr = idx2ptr0;
    
    T* valptr2 = value;
    T* buf = NULL;
    
    R_xlen_t idx2ii = 0;
    R_xlen_t idx1ii = 0;
    int64_t start_loc = 0;
    
    for(idx2ii = 0; idx2ii < idx2len; idx2ii++, idx2ptr++){
        
        if(*idx2ptr == NA_INTEGER64){
            continue;
        }
        
        // idx1ptr = (int64_t*) REAL(idx1);
        idx1ptr = idx1ptr0;
        start_loc = (*idx2ptr - idx2_start) * block_size;
        
        // load current block
        buf = (T*)(conn0 + start_loc * elem_size);
        
        for(idx1ii = 0; idx1ii < idx1len; idx1ii++, idx1ptr++, valptr2++){
            // calculate pointer location in the file
            // no check here, but tmp_loc should be >=0
            if(*idx1ptr != NA_INTEGER64){
                // *(buf + (*idx1ptr - idx1_start)) = (*valptr2);
                
                lendian_assign(buf + (*idx1ptr - idx1_start),
                               valptr2, elem_size);
            }
        }
        
    }
    
}

template <typename T>
SEXP FARR_subset_assign_template(
        const std::string& filebase, 
        const List& sch, T* value_ptr){
    
    SEXP idx1 = sch["idx1"];
    SEXP idx1range = sch["idx1range"];
    List idx2s = sch["idx2s"];
    int64_t block_size = (int64_t) (sch["block_size"]);
    IntegerVector partitions = sch["partitions"];
    IntegerVector idx2lens = sch["idx2lens"];
    int elem_size = sizeof(T);
    
    int has_error = -1;
    std::string error_msg = "";
    
    R_xlen_t idx1len = Rf_xlength(idx1);
    
    // Check whether indices are valid
    int64_t* idx1rangeptr = (int64_t*) REAL(idx1range);
    int64_t idx1_start = *idx1rangeptr, idx1_end = *(idx1rangeptr + 1);
    
    if( idx1_start == NA_INTEGER64 || idx1_end == idx1_start ||
        idx1_end < 0 || idx1_start < 0 ){
        return( R_NilValue );
    }
    
    
    int ncores = getThreads();
    if(ncores > idx2s.length()){
        ncores = idx2s.length();
    }
    
    int64_t* idx1ptr0 = (int64_t*) REAL(idx1);
    const boost::interprocess::mode_t mode = boost::interprocess::read_write;
    
    std::map<int64_t, boost::interprocess::file_mapping*> conn_map;
    std::map<int64_t, bool> conn_exist;
    
    std::map<int64_t, bool>::iterator it2;
    
    // it = conn_map.find(part);
    for(IntegerVector::iterator partptr = partitions.begin();
        partptr != partitions.end(); partptr++){
        it2 = conn_exist.find(*partptr);
        if(it2 == conn_exist.end()){
            try {
                std::string file = filebase + std::to_string(*partptr) + ".farr";
                boost::interprocess::file_mapping *fm = new boost::interprocess::file_mapping(file.c_str(), mode);
                conn_map.insert( std::pair<int64_t, boost::interprocess::file_mapping*>(*partptr, fm) );
                conn_exist.insert( std::pair<int64_t, bool>(*partptr, true) );
            } catch (...){
                conn_exist.insert( std::pair<int64_t, bool>(*partptr, false) );
            }
        }
    }
    
#pragma omp parallel num_threads(ncores)
{
#pragma omp for schedule(static, 1) nowait
    for(R_xlen_t iter = 0; iter < idx2s.length(); iter++){
        
        if( has_error >= 0 ){ continue; }
        
        R_xlen_t part = partitions[iter];
        int64_t skips = 0;
        if(iter > 0){
            skips = idx2lens[iter - 1];
        }
        
        // obtain starting end ending indices of idx2
        SEXP idx2 = idx2s[iter];
        R_xlen_t idx2len = Rf_xlength(idx2);
        int64_t idx2_start = NA_INTEGER64, idx2_end = -1;
        for(int64_t* ptr2 = INTEGER64(idx2); idx2len > 0; idx2len--, ptr2++ ){
            if( *ptr2 == NA_INTEGER64 ){
                continue;
            }
            if( *ptr2 < idx2_start || idx2_start == NA_INTEGER64 ){
                idx2_start = *ptr2;
            }
            if( idx2_end < *ptr2 ){
                idx2_end = *ptr2;
            }
        }
        
        if( idx2_start == NA_INTEGER64 || 
            idx2_end < 0 || idx2_start < 0 ){
            // This is NA partition, no need to subset
            continue;
        }
        
        std::string file = filebase + std::to_string(part) + ".farr";
        
        try{
            std::map<int64_t, boost::interprocess::file_mapping*>::const_iterator it = conn_map.find(part);
            
            if(it == conn_map.end()){
                has_error = part;
                error_msg = "Cannot open partition " + std::to_string(part + 1);
                continue;
            }
            
            int64_t region_len = elem_size * (idx1_end - idx1_start + block_size * (idx2_end - idx2_start));
            int64_t region_offset = FARR_HEADER_LENGTH + elem_size * (block_size * idx2_start + idx1_start);
            boost::interprocess::mapped_region region(
                    *(it->second), mode, region_offset, region_len);
            region.advise(boost::interprocess::mapped_region::advice_sequential);
            
            char* begin = static_cast<char*>(region.get_address());
        
            int64_t* idx2_ptr = INTEGER64(idx2);
            R_xlen_t idx2_len = Rf_xlength(idx2);
            T* value_ptr2 = value_ptr + (idx1len * skips);
            int64_t* idx1ptr = idx1ptr0;
            subset_assign_partition(
                begin, value_ptr2,
                block_size, idx1ptr, idx1len, 
                idx1_start, idx2_start, 
                idx2_ptr, idx2_len );
            
            // region.flush();
        } catch(std::exception &ex){
            has_error = part;
            error_msg =  ex.what();
            error_msg += " while trying to open file.";
        } catch(...){
            has_error = part;
            error_msg = "Unknown error.";
        }
    }
}

    std::map<int64_t, boost::interprocess::file_mapping*>::iterator it1 = conn_map.begin();
    for(; it1 != conn_map.end(); it1++){
        delete it1->second;
    }

    // UNPROTECT(ncores);
    if( has_error >= 0 ){
        stop("Cannot write to partition " + 
            std::to_string(has_error + 1) + 
            ". Reason: " + error_msg);
    }
    
    return( R_NilValue );
}

// [[Rcpp::export]]
SEXP FARR_subset_assign2(
        const std::string& filebase,
        SEXP value,
        const SEXP listOrEnv,
        const size_t thread_buffer = 2097152,
        int split_dim = 0
) {
    // Get meta information
    const std::string fbase = correct_filebase(filebase);
    List meta = FARR_meta(fbase);
    const int elem_size = meta["elem_size"];
    const SEXPTYPE sexp_type = meta["sexp_type"];
    SEXP dim = meta["dimension"]; // double
    SEXP cum_part_size = meta["cumsum_part_sizes"];
    
    // calculate split_dim
    R_len_t ndims = Rf_length(dim);
    if( split_dim == NA_INTEGER || split_dim == 0 ){
        split_dim = guess_splitdim(dim, elem_size, thread_buffer);
    } else if (split_dim < 1 || split_dim > ndims-1 ){
        stop("Incorrect `split_dim`: must be an integer from 1 to ndims-1 ");
    }
    set_buffer(dim, elem_size, thread_buffer, split_dim);
    
    // schedule indices
    List sch = schedule(listOrEnv, dim, cum_part_size,
                        split_dim, 1);
    
    // Check whether indices are valid
    SEXP idx1range = sch["idx1range"];
    int64_t* idx1rangeptr = (int64_t*) REAL(idx1range);
    int64_t idx1_start = *idx1rangeptr, idx1_end = *(idx1rangeptr + 1);
    
    if( idx1_start == NA_INTEGER64 || idx1_end < 0 || idx1_start < 0 ){
        return( R_NilValue );
    }
    
    // coerce vector to desired SEXP type
    SEXP value_ = PROTECT(convert_as(value, sexp_type));
    SEXPTYPE valtype = TYPEOF(value_);
    
    // allocate buffers
    int ncores = getThreads();
    std::vector<SEXP> buff_pool(ncores);
    for(int i = 0; i < ncores; i++){
        buff_pool[i] = PROTECT(Rf_allocVector(
            valtype, idx1_end - idx1_start + 1));
    }
    
        
    switch(sexp_type) {
    case INTSXP: {
        FARR_subset_assign_template(fbase, sch, INTEGER(value_));
        break;
    }
    case CPLXSXP:
    case REALSXP: {
        FARR_subset_assign_template(fbase, sch, REAL(value_));
        break;
    }
    case FLTSXP: {
        FARR_subset_assign_template(fbase, sch, FLOAT(value_));
        break;
    }
    case LGLSXP:
    case RAWSXP: {
        FARR_subset_assign_template(fbase, sch, RAW(value_));
        break;
    }
    default: {
        UNPROTECT( 1 + ncores );
        stop("SEXP type not supported.");
    }
    }
    
    UNPROTECT( 1 + ncores );
    return(R_NilValue);
    
}





/*** R
devtools::load_all()
set.seed(1); file <- tempfile(); unlink(file, recursive = TRUE)
x <- filearray_create(file, 3:5, partition_size = 2, type = "complex")
x$initialize_partition()
FARR_subset_assign2(
    filebase = x$.filebase,
    1:60 + 1i,
    listOrEnv = list()
)
x[]

# dim <- c(100,100,100,100)
# x <- filearray_create(file, dim, type = 'double')
# tmp <- as.double(seq_len(1e8))
# setThreads(3)
# system.time({
#     x[] <- tmp
# }, gcFirst = TRUE)
# identical(as.vector(x[]), as.double(tmp))
# pryr::object_size(x)
*/
