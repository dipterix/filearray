#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>
// [[Rcpp::depends(BH)]]

#include "core.h"
#include "serialize.h"
#include "conversion.h"
#include "utils.h"
using namespace Rcpp;


template <typename T, typename B>
SEXP each_partition_template(
        T* mmap_ptr, const int64_t& mmap_size,
        const int64_t exp_len, 
        const Function fun, int64_t* count, List& ret,
        B* argbuf_ptr, SEXP argbuf,
        void (*transform) (const T*, B*, const int&, const bool&)
) {
    bool swap_endian = !isLittleEndian();
    int64_t read_len = 0, current_pos = 0;
    
    const int buffer_nelems = Rf_length(argbuf);
    
    int64_t rest_len = 0;
    
    SEXP readlen_sxp = PROTECT(Rf_allocVector(REALSXP, 1));
    double* readlen_ptr = REAL(readlen_sxp);
    
    SEXP count_sxp = PROTECT(Rf_allocVector(REALSXP, 1));
    double* count_ptr = REAL(count_sxp);
    
    while(current_pos < exp_len){
        read_len = exp_len - current_pos;
        if( read_len > buffer_nelems ){
            read_len = buffer_nelems;
        }
        if( read_len + current_pos > mmap_size ){
            read_len = mmap_size - current_pos;
        }
        // read_len = lendian_fread(filebuf_ptr, elem_size, buffer_nelems, conn);
        transform(mmap_ptr, argbuf_ptr, read_len, swap_endian);
        
        // transform(const *T, ...) does not alter mmap_ptr, need to increase
        mmap_ptr += read_len;
        
        if( read_len > 0 ){
            *readlen_ptr = (double) read_len;
            *count_ptr = (double) *count;
            if( read_len < buffer_nelems ){
                // if( tmp_arg == R_NilValue ){
                //     tmp_arg = PROTECT(sub_vec_range(argbuf, 0, read_len));
                // } else if( read_len - Rf_xlength(tmp_arg) != 0 ){
                //     UNPROTECT(1);
                //     tmp_arg = PROTECT(sub_vec_range(argbuf, 0, read_len));
                // }
                SEXP tmp_arg = PROTECT(sub_vec_range(argbuf, 0, read_len));
                SEXP item = PROTECT( fun(tmp_arg, readlen_sxp, count_sxp) );
                ret.push_back( item );
                UNPROTECT( 2 ); // item, tmp_arg
            } else {
                SEXP item = PROTECT( fun(argbuf, readlen_sxp, count_sxp) );
                ret.push_back( item );
                UNPROTECT( 1 ); // item
                // ret.push_back( Shield<SEXP>( fun(Shield<SEXP>(argbuf), Shield<SEXP>(wrap(read_len)), Shield<SEXP>(wrap(*count))) ) );
            }
        }
        
        rest_len = exp_len - current_pos;
        if( rest_len > buffer_nelems ){
            rest_len = buffer_nelems;
        }
        current_pos += rest_len;
        *count += rest_len;
    }
    // if( tmp_arg != R_NilValue ){
    //     UNPROTECT(1);
    // }
    
    UNPROTECT(2); // count_sxp, readlen_sxp
    
    return( ret );
}

// [[Rcpp::export]]
SEXP FARR_buffer_mapreduce(
        const std::string& filebase, 
        const Function map, 
        const Nullable<Function> reduce,
        const int& buffer_nelems
){
    std::string fbase = correct_filebase(filebase);
    const List meta = FARR_meta(fbase);
    const SEXPTYPE x_type = meta["sexp_type"];
    
    SEXP dim_ = meta["dimension"];
    realToInt64_inplace(dim_);
    int ndims = Rf_length(dim_);
    
    SEXP pcumlens = meta["cumsum_part_sizes"];
    realToInt64_inplace(pcumlens);
    R_xlen_t nparts = Rf_xlength(pcumlens);
    
    // int64_t bufferlen = get_buffer_size() / elem_size;
    SEXPTYPE argtype = x_type == FLTSXP ? REALSXP : x_type;
    
    SEXP argbuffer = PROTECT(Rf_allocVector(argtype, buffer_nelems)); 
    
    // estimate number of runs (TODO)
    List ret = List::create();
    
    int64_t* dimptr = INTEGER64(dim_);
    int64_t plen = 1;
    for(R_xlen_t ii = 0; ii <ndims - 1; ii++, dimptr++){
        plen *= *dimptr;
    }
    int64_t* pclptr = (int64_t*) REAL(pcumlens);
    int64_t psize = 0;
    
    std::string partition_path = "";
    int64_t count = 1;
    int64_t count2 = 1;
    const boost::interprocess::mode_t mode = boost::interprocess::read_only;
    for(R_xlen_t part = 0; part < nparts; part++){
        partition_path = fbase + std::to_string(part) + ".farr";
        
        if(part == 0){
            count = count2;
            psize = (*pclptr + part);
        } else {
            psize = (*pclptr + part) - (*pclptr + (part-1));
            count = count2 + plen * (*pclptr + (part-1));
        }
        
        try {
            boost::interprocess::file_mapping fm(partition_path.c_str(), mode);
            boost::interprocess::mapped_region region(fm, mode, FARR_HEADER_LENGTH);
            region.advise(boost::interprocess::mapped_region::advice_sequential);
            
            int64_t region_size = region.get_size();
            
            switch(x_type){
            case INTSXP: {
                int* begin = static_cast<int*>(region.get_address());
                each_partition_template(
                    begin, region_size / sizeof(int),
                    psize * plen, map, &(count), ret,
                    INTEGER(argbuffer), argbuffer,
                    transforms_asis);
                break;
            }
            case REALSXP: {
                
                double* begin = static_cast<double*>(region.get_address());
                each_partition_template(
                    begin, region_size / sizeof(double),
                    psize * plen, map, &(count), ret,
                    REAL(argbuffer), argbuffer,
                    transforms_asis);
                break;
            }
            case FLTSXP: {
                
                float* begin = static_cast<float*>(region.get_address());
                each_partition_template(
                    begin, region_size / sizeof(float),
                    psize * plen, map, &(count), ret,
                    REAL(argbuffer), argbuffer,
                    transforms_float);
                break;
            }
            case RAWSXP: {
                Rbyte* begin = static_cast<Rbyte*>(region.get_address());
                each_partition_template(
                    begin, region_size / sizeof(Rbyte),
                    psize * plen, map, &(count), ret,
                    RAW(argbuffer), argbuffer,
                    transforms_asis);
                break;
            }
            case LGLSXP: {
                
                Rbyte* begin = static_cast<Rbyte*>(region.get_address());
                each_partition_template(
                    begin, region_size / sizeof(Rbyte),
                    psize * plen, map, &(count), ret,
                    LOGICAL(argbuffer), argbuffer,
                    transforms_logical);
                break;
            }
            case CPLXSXP: {
                double* begin = static_cast<double*>(region.get_address());
                each_partition_template(
                    begin, region_size / sizeof(double),
                    psize * plen, map, &(count), ret,
                    COMPLEX(argbuffer), argbuffer,
                    transforms_complex);
                break;
            }
            }
        } catch (...) {}
        
    }
    
    if(reduce == R_NilValue){
        UNPROTECT( 1 );
        return ret;
    }
    
    Function reduce2 = (Function) reduce;
    SEXP re = PROTECT(reduce2(ret));
    UNPROTECT( 2 );
    return(re);
}


/*** R
# devtools::load_all()
dim <- 33:35
set.seed(1); file <- tempfile(); unlink(file, recursive = TRUE)
x <- filearray_create(file, dim)
tmp <- seq_len(prod(dim))
setThreads(8)
system.time({
    x[] <- tmp
}, gcFirst = TRUE)
rm(tmp); gc()

mapreduce(x, \(data){
    max(data, na.rm = TRUE)
}, \(x){
    do.call('max', x)
})

system.time=profvis::profvis
gc()
system.time({
    filebase <- paste0(x$.filebase, x$.sep)
    set_buffer_size(max_buffer_size())
    FARR_buffer_mapreduce(filebase, function(buffer, size, idx){
        if(size != length(buffer)){
            buffer <- buffer[seq_len(size)]
        }
        re <- which(buffer == 5000)
        if(length(re)){
            re <- re + (idx-1)
        }
        # re <- max(buffer, na.rm = TRUE)
        re
    }, function(ret){
        do.call(c, ret)
    }, x$dimension(), x$.partition_info[,3], 1000000, 14L)
})

setThreads(1)
gc()
system.time({
    max(sapply(1:100, function(i){
        a <- x[,,,i]
        max(a, na.rm = TRUE)
    }))
})

*/
