#include "core.h"
#include "serialize.h"
#include "conversion.h"
#include "utils.h"
using namespace Rcpp;

template <typename T>
void transform_asis(const T* x, T* y, const int& nelem){
    memcpy(y, x, nelem * sizeof(T));
}

void transform_floats(const float* x, double* y, const int& nelem){
    double* y2 = y;
    for(int i = 0; i < nelem; i++, y2++){
        if(ISNAN(*(x + i))){
            *y2 = NA_REAL;
        } else {
            *y2 = *(x + i);
        }
    }
}
void transform_logicals(const Rbyte* x, int* y, const int& nelem){
    int* y2 = y;
    for(int i = 0; i < nelem; i++, y2++){
        *y2 = *(x + i);
        if( *y2 == 2 ){
            *y2 = NA_LOGICAL;
        }
    }
}
void transform_cplxs(const double* x, Rcomplex* y, const int& nelem){
    realToCplx(x, y, nelem);
}

template <typename T, typename B>
SEXP each_partition_template(
        FILE* conn, const int64_t exp_len, 
        const Function fun, int64_t* count, List& ret,
        T* filebuf_ptr, B* argbuf_ptr, SEXP argbuf,
        void (*transform) (const T*, B*, const int&)
) {
    
    fseek(conn, FARR_HEADER_LENGTH, SEEK_SET);
    
    int64_t read_len = 0, current_pos = 0;
    
    const int buffer_nelems = Rf_length(argbuf);
    int elem_size = sizeof(T);
    
    int64_t rest_len = 0;
    
    while(current_pos < exp_len){
        read_len = lendian_fread(filebuf_ptr, elem_size, buffer_nelems, conn);
        transform(filebuf_ptr, argbuf_ptr, read_len);
        
        if( read_len > 0 ){
            if( read_len < buffer_nelems ){
                // if( tmp_arg == R_NilValue ){
                //     tmp_arg = PROTECT(sub_vec_range(argbuf, 0, read_len));
                // } else if( read_len - Rf_xlength(tmp_arg) != 0 ){
                //     UNPROTECT(1);
                //     tmp_arg = PROTECT(sub_vec_range(argbuf, 0, read_len));
                // }
                SEXP tmp_arg = Shield<SEXP>(sub_vec_range(argbuf, 0, read_len));
                ret.push_back( Shield<SEXP>( fun(Shield<SEXP>(tmp_arg), Shield<SEXP>(wrap(read_len)), Shield<SEXP>(wrap(*count))) ) );
            } else {
                ret.push_back( Shield<SEXP>( fun(Shield<SEXP>(argbuf), Shield<SEXP>(wrap(read_len)), Shield<SEXP>(wrap(*count))) ) );
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
    SEXPTYPE fbtype = file_buffer_sxptype(x_type);
    SEXPTYPE argtype = x_type == FLTSXP ? REALSXP : x_type;
    
    SEXP filebuffer = PROTECT(Rf_allocVector(fbtype, buffer_nelems)); 
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
    FILE* conn = NULL;
    int64_t count = 1;
    int64_t count2 = 1;
    for(R_xlen_t part = 0; part < nparts; part++){
        partition_path = fbase + std::to_string(part) + ".farr";
        
        if(part == 0){
            count = count2;
            psize = (*pclptr + part);
        } else {
            psize = (*pclptr + part) - (*pclptr + (part-1));
            count = count2 + plen * (*pclptr + (part-1));
        }
        
        conn = fopen(partition_path.c_str(), "rb");
        
        if( conn ){
            try{
                switch(x_type){
                case INTSXP:
                    each_partition_template(
                        conn, psize * plen, map, &(count), ret,
                        INTEGER(filebuffer), INTEGER(argbuffer), argbuffer,
                        transform_asis);
                    break;
                case REALSXP:
                    each_partition_template(
                        conn, psize * plen, map, &(count), ret,
                        REAL(filebuffer), REAL(argbuffer), argbuffer,
                        transform_asis);
                    break;
                case FLTSXP:
                    each_partition_template(
                        conn, psize * plen, map, &(count), ret,
                        FLOAT(filebuffer), REAL(argbuffer), argbuffer,
                        transform_floats);
                    break;
                case RAWSXP:
                    each_partition_template(
                        conn, psize * plen, map, &(count), ret,
                        RAW(filebuffer), RAW(argbuffer), argbuffer,
                        transform_asis);
                    break;
                case LGLSXP:
                    each_partition_template(
                        conn, psize * plen, map, &(count), ret,
                        RAW(filebuffer), LOGICAL(argbuffer), argbuffer,
                        transform_logicals);
                    break;
                case CPLXSXP:
                    each_partition_template(
                        conn, psize * plen, map, &(count), ret,
                        REAL(filebuffer), COMPLEX(argbuffer), argbuffer,
                        transform_cplxs);
                    break;
                default: 
                    fclose(conn);
                conn = NULL;
                }
            } catch(...){}
            if(conn != NULL){
                fclose(conn);
                conn = NULL;
            }
        }
    }
    
    if(reduce == R_NilValue){
        UNPROTECT( 2 );
        return ret;
    }
    
    Function reduce2 = (Function) reduce;
    SEXP re = PROTECT(reduce2(ret));
    UNPROTECT( 3 );
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
