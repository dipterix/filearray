#include "common.h"
#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>
// [[Rcpp::depends(BH)]]

#include "TinyParallel.h"
#include "threadSettings.h"
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
    
    // print(wrap(unit_partlen));
    // print(cum_partsizes);
    // print(value_);
    
    int file_buffer_elemsize = file_element_size(array_type);
    std::string fbase = correct_filebase(filebase);
    R_len_t nparts = Rf_length(cum_partsizes);
    int64_t* cum_part = INTEGER64(cum_partsizes);
    
    // We want to calculate which partitions are used, saved to part_start and part_end
    // However, each partition may contain multiple slices, hence we first decide
    // slices that's being used, stored in slice_idx1 and slice_idx2
    // in R's index format (startwith 1 and ends with to # of partitions)
    // calculate the first partition, should start from 1
    int64_t slice_idx1 = 0;
    // partition number cannot go beyond nparts + 1 (can equal)
    int64_t slice_idx2 = 0;
    int64_t tmp = 0;
    
    // printed message means get element from `from` (C index) and length of 
    // `len` across `nparts` partitions
    // Rcout << "From: " << from << " - len: " << len << " nparts: " << nparts << "\n";
    for(; tmp <= from; tmp+= unit_partlen, slice_idx1++){}
    
    cum_part = INTEGER64(cum_partsizes) + (nparts - 1);
    const int64_t max_slices = unit_partlen * (*cum_part);
    for(
        slice_idx2 = slice_idx1; 
        tmp < from + len && slice_idx2 < max_slices; 
        tmp+= unit_partlen, slice_idx2++
    ){}
    
    if( slice_idx2 > *cum_part ) {
        slice_idx2 = *cum_part;
    }
    
    // which slices to start and which to end
    // Rcout << "Starting from partition: " << slice_idx1 << " - ends before: " << slice_idx2 << 
    //     " (max: " << *cum_part << ")\n";
    
    // Which file partition to start: min = 0
    // unlike slice_idx1/2, part_start and part_end are index in C-style
    // That is: they starting from 0, and max is number of partitions-1
    int part_start = 0;
    int part_end = 0;
    int64_t skip_start = 0;

    for(cum_part = INTEGER64(cum_partsizes); *cum_part < slice_idx1; cum_part++, part_start++){}
    if( part_start == 0 ){
        skip_start = from;
    } else {
        skip_start = from - (*(cum_part - 1)) * unit_partlen;
    }
    for(part_end = part_start; *cum_part < slice_idx2; cum_part++, part_end++){
        // Rcout << *cum_part << std::endl; 
    }
    
    /*
    // skip_end = (*cum_part) * unit_partlen - (from + len);
    if(part_end == 0) {
        skip_end = unit_partlen - (from + len);
        // Rcout << part_start << " " << part_end << " " << *cum_part << std::endl; 
    } else {
        skip_end = (*(cum_part - 1)) * unit_partlen - (from + len);
        // Rcout << part_start << " " << part_end << " " << *(cum_part-1) << std::endl; 
    }
    // This happens when buffer size is longer than array length
    if(skip_end < 0) {
        skip_end = 0;
    }
     */
    
    int64_t read_start = 0;
    int64_t write_len = 0;
    int64_t part_nelem = 0;
    cum_part = INTEGER64(cum_partsizes) + part_start;
    
    int64_t nwrite = 0;
    
    const boost::interprocess::mode_t mode = boost::interprocess::read_write;
    
    for(int part = part_start; part <= part_end; part++, cum_part++, nwrite += write_len){
        // Rcout << part << "\n";
        if( part >= nparts ){
            continue;
        }
        // get partition n_elems
        // part_nelem = (*cum_part) * unit_partlen - last_part_nelem;
        // last_part_nelem = (*cum_part) * unit_partlen;
        
        // skip read_start elements
        if(part == 0) {
            part_nelem = (*cum_part) * unit_partlen;
        } else {
            part_nelem = (*cum_part - *(cum_part - 1)) * unit_partlen;
        }
        if( part == part_start ) {
            read_start = skip_start;
        } else {
            read_start = 0;
        }
        // Rcout << part_nelem << "--\n";
        // then read read_len elements
        write_len = part_nelem - read_start;
        
        if(nwrite + write_len > len) {
            write_len = len - nwrite;
        }
        if(write_len <= 0) {
            break;
        }
        // if( part == part_end ){
        //     write_len -= skip_end;
        // }
        
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
                // const unsigned char *buffer_src = (const unsigned char*)(INTEGER(value_) + nwrite);
                // const unsigned char *buffer_dst = (const unsigned char*)(begin);
                lendian_assign(begin, INTEGER(value_) + nwrite, file_buffer_elemsize, write_len);
                // for( size_t tt = 0 ; tt < file_buffer_elemsize * write_len ; tt++ ) {
                //     Rcout << (int)*(buffer_src + tt) << " - " << (int)*(buffer_dst + tt) << "\n";
                // }
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
        int64_t* idx2ptr0, R_xlen_t idx2len,
        const int &value_inc = 1) {
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
        
        // Rcout << "### idx2ii:" << idx2ii << ", start_loc: " << start_loc << 
        //     ", buf pos: " << start_loc * elem_size << ", idx1_start: " <<
        //         idx1_start << "\n";
        // block_size: 861584, idx1len: 9001, idx1_start: 216024, idx2_start: 0, idx2_len: 1
        // ### idx2ii:0, start_loc: 0, buf pos: 0, idx1_start: 216024
        
        for(idx1ii = 0; idx1ii < idx1len; idx1ii++, idx1ptr++, valptr2+=value_inc){
            // calculate pointer location in the file
            // no check here, but tmp_loc should be >=0
            
            // Rcout << "idx1ii: " << idx1ii << ", *idx1ptr: " << *idx1ptr << ", idx1_start: " << idx1_start << 
            //     ", *valptr2: " << *valptr2 << ", buf_new: " << *(buf + (*idx1ptr - idx1_start)) << "\n";
            // *idx1ptr: 225024, idx1_start: 216024, *valptr2: 147

            if(*idx1ptr != NA_INTEGER64){
                // *(buf + (*idx1ptr - idx1_start)) = (*valptr2);
                
                // lendian_assign(void* dst, const void* src, const size_t& elem_size, const size_t& nelems)
                lendian_assign(buf + (*idx1ptr - idx1_start), valptr2, elem_size, 1);
            }
        }
        
    }
    
}

template <typename T>
struct FARRAssigner : public TinyParallel::Worker {
    const std::string& filebase;
    const List& sch;
    T* value_ptr;
    
    // value_ptr increment size, can either be 0 (length(value) == 1)
    // or 1 (length(value) is the same as subset size)
    int value_ptr_inc;
    
    SEXP idx1;
    SEXP idx1range;
    List idx2s;
    int64_t block_size;
    IntegerVector partitions;
    IntegerVector idx2lens;
    // int elem_size;
    R_xlen_t idx1len;
    int64_t idx1_start;
    int64_t idx1_end;
    int64_t* idx1ptr0;
    
    int has_error;
    std::string error_msg;
    boost::interprocess::mode_t mode;
    
    
    FARRAssigner(
        const std::string& filebase,
        const List& sch, 
        const int64_t& value_len,
        T* value_ptr
    ): filebase(filebase), sch(sch) {
        this->value_ptr = value_ptr;
        if( value_len - 1 == 0 ) {
            this->value_ptr_inc = 0;
        } else {
            this->value_ptr_inc = 1;
        }
        this->idx1 = sch["idx1"];
        this->idx1range = sch["idx1range"];
        this->idx2s = sch["idx2s"];
        this->block_size = (int64_t) (sch["block_size"]);
        this->partitions = sch["partitions"];
        this->idx2lens = sch["idx2lens"];
        // int elem_size = sizeof(T);
        
        this->idx1len = Rf_xlength(idx1);
        
        // Check whether indices are valid
        int64_t* idx1rangeptr = (int64_t*) REAL(idx1range);
        this->idx1_start = *idx1rangeptr;
        this->idx1_end = *(idx1rangeptr + 1);
        
        if( idx1_start == NA_INTEGER64 || idx1_end < idx1_start ||
            idx1_end < 0 || idx1_start < 0 ){
            this->idx1ptr0 = NULL;
            // return( R_NilValue );
        } else {
            this->idx1ptr0 = (int64_t*) REAL(idx1);
        }
        
        // 
        // int ncores = getThreads();
        // if(ncores > idx2s.length()){
        //     ncores = idx2s.length();
        // }
        
        this->has_error = -1;
        this->error_msg = "";
        this->mode = boost::interprocess::read_write;
    }
    
    void operator()(std::size_t begin, std::size_t end) {
        if( idx1ptr0 == NULL ) { return; }
        if( has_error >= 0 ){ return; }
        
        int elem_size = sizeof(T);
        
        for(R_xlen_t iter = begin; iter < end; iter++){
            
            if( has_error >= 0 ){ continue; }
            
            R_xlen_t part = partitions[iter];
            int64_t skips = 0;
            if(iter > 0){
                skips = idx2lens[iter - 1];
            }
            
            // Rcout << part << ", skip=" << skips << "\n";
            
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
            
            // Rcout << "idx start-end: " << idx2_start << " - " << idx2_end << "\n";
            
            std::string file = filebase + std::to_string(part) + ".farr";
            // Rcpp::print(Rcpp::wrap(file));
            
            
            
            try{
                /*
                 * On most OS, there is a limit on how many descriptors that open
                 * simultaneously:
                 * (OSX) launchctl limit maxfiles
                 * 
                 * Besides, the closed file descriptor will be reused quickly on 
                 * many Unix systems
                 * 
                 * https://docs.fedoraproject.org/en-US/Fedora_Security_Team/1/html/Defensive_Coding/sect-Defensive_Coding-Tasks-Descriptors.html
                 * 
                 * Unlike process IDs, which are recycle only gradually, the kernel 
                 * always allocates the lowest unused file descriptor when a new 
                 * descriptor is created. This means that in a multi-threaded 
                 * program which constantly opens and closes file descriptors, 
                 * descriptors are reused very quickly. Unless descriptor closing 
                 * and other operations on the same file descriptor are synchronized 
                 * (typically, using a mutex), there will be race coniditons and 
                 * I/O operations will be applied to the wrong file descriptor. 
                 * 
                 * https://stackoverflow.com/questions/52087692/can-multiple-file-objects-share-the-same-file-descriptor
                 */
                boost::interprocess::file_mapping fm(file.c_str(), mode);
                // std::map<int64_t, boost::interprocess::file_mapping*>::const_iterator it = conn_map.find(part);
                // 
                // if(it == conn_map.end()){
                //     has_error = part;
                //     error_msg = "Cannot open partition " + std::to_string(part + 1);
                //     continue;
                // }
                
                int64_t region_len = elem_size * (idx1_end - idx1_start + 1 + block_size * (idx2_end - idx2_start));
                int64_t region_offset = FARR_HEADER_LENGTH + elem_size * (block_size * idx2_start + idx1_start);
                // boost::interprocess::mapped_region region(
                //         *(it->second), mode, region_offset, region_len);
                boost::interprocess::mapped_region region(
                        fm, mode, region_offset, region_len);
                region.advise(boost::interprocess::mapped_region::advice_sequential);
                
                char* begin = static_cast<char*>(region.get_address());
                
                int64_t* idx2_ptr = INTEGER64(idx2);
                R_xlen_t idx2_len = Rf_xlength(idx2);
                T* value_ptr2 = value_ptr + (idx1len * skips) * this->value_ptr_inc;
                int64_t* idx1ptr = idx1ptr0;
                
                // Rcout << "block_size: " << block_size << ", idx1len: " << idx1len << ", idx1_start: " << idx1_start << 
                //     ", idx2_start: " << idx2_start << ", idx2_len: " << idx2_len << "\n";
                
                subset_assign_partition(
                    begin, value_ptr2,
                    block_size, idx1ptr, idx1len, 
                    idx1_start, idx2_start, 
                    idx2_ptr, idx2_len,
                    this->value_ptr_inc );
                
                
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
    
    void save() {
        if( idx1ptr0 == NULL ) { return; }
        parallelFor(0, idx2s.length(), *this);
        if( has_error >= 0 ){
            stop("Cannot write to partition " + 
                std::to_string(has_error + 1) + 
                ". Reason: " + error_msg);
        }
    }
};

template <typename T>
SEXP FARR_subset_assign_template(
        const std::string& filebase, 
        const List& sch, 
        const R_xlen_t &value_len,
        T* value_ptr
){
    FARRAssigner<T> assigner(filebase, sch, value_len, value_ptr);
    assigner.save();
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
    
    // Rcpp::print(sch);
    
    // Check whether indices are valid
    SEXP idx1range = sch["idx1range"];
    int64_t* idx1rangeptr = (int64_t*) REAL(idx1range);
    int64_t idx1_start = *idx1rangeptr, idx1_end = *(idx1rangeptr + 1);
    
    if( idx1_start == NA_INTEGER64 || idx1_end < 0 || idx1_start < 0 ){
        return( R_NilValue );
    }
    
    // coerce vector to desired SEXP type
    SEXP value_ = PROTECT(convert_as(value, sexp_type));
    // SEXPTYPE valtype = TYPEOF(value_);
    
    // // allocate buffers
    // int ncores = getThreads();
    // std::vector<SEXP> buff_pool(ncores);
    // for(int i = 0; i < ncores; i++){
    //     buff_pool[i] = PROTECT(Rf_allocVector(
    //         valtype, idx1_end - idx1_start + 1));
    // }
    
        
    switch(sexp_type) {
    case INTSXP: {
        FARR_subset_assign_template(fbase, sch, XLENGTH(value_), INTEGER(value_));
        break;
    }
    case CPLXSXP:
    case REALSXP: {
        FARR_subset_assign_template(fbase, sch, XLENGTH(value_), REAL(value_));
        break;
    }
    case FLTSXP: {
        FARR_subset_assign_template(fbase, sch, XLENGTH(value_), FLOAT(value_));
        break;
    }
    case LGLSXP:
    case RAWSXP: {
        FARR_subset_assign_template(fbase, sch, XLENGTH(value_), RAW(value_));
        break;
    }
    default: {
        UNPROTECT( 1 );
        stop("SEXP type not supported.");
        return(R_NilValue); // wall
    }
    }
    
    UNPROTECT( 1 );
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
