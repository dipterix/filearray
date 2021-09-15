// partition_path_private = rootPath + std::to_string(lidx) + ".bmat";

// [[Rcpp::export]]
SEXP p_subset(
        const std::string& file, 
        const R_xlen_t block_size, 
        SEXP idx1, SEXP idx2
) {
    R_xlen_t idx1len = Rf_xlength(idx1),
        idx2len = Rf_xlength(idx2);
    SEXP ret = Rf_allocVector(REALSXP, idx1len * idx2len);
    double* retptr = REAL(ret);
    const double na = NA_REAL;
    const int elem_size = sizeof(double);
    
    R_xlen_t ii = 0;
    for(ii = 0; ii < Rf_xlength(ret); ii++, retptr++ ){
        *retptr = na;
    }
    retptr = REAL(ret);
    int64_t idx1_start = NA_INTEGER64, idx1_end = 0, 
        idx2_start = NA_INTEGER64, idx2_end = 0;
    int64_t* ptr; 
    
    for(ptr = (int64_t*) REAL(idx1), ii = 0; ii < Rf_xlength(idx1); ii++, ptr++ ){
        if( *ptr == NA_INTEGER64 ){
            continue;
        }
        if( *ptr < idx1_start || idx1_start == NA_INTEGER64 ){
            idx1_start = *ptr;
        }
        if( idx1_end < *ptr ){
            idx1_end = *ptr;
        }
    }
    
    for(ptr = (int64_t*) REAL(idx2), ii = 0; ii < Rf_xlength(idx2); ii++, ptr++ ){
        if( *ptr == NA_INTEGER64 ){
            continue;
        }
        if( *ptr < idx2_start || idx2_start == NA_INTEGER64 ){
            idx2_start = *ptr;
        }
        if( idx2_end < *ptr ){
            idx2_end = *ptr;
        }
    }
    
    const int idx1_sorted = kinda_sorted(idx1, idx1_start, nbuffers / elem_size);
    const int idx2_sorted = kinda_sorted(idx2, idx2_start, 1);
    // Rcout << idx2_sorted << "\n";
    
    FILE* conn = fopen( file.c_str(), "rb" );
    
    char* buffer[nbuffers];
    
    try{
        // subset(conn, block_size, idx1, idx2, REAL(ret), 8);
        subset_partition(conn, buffer, nbuffers, retptr, block_size, 
                         idx1, idx1_start, idx1_end,
                         idx2, idx2_start, idx2_end,
                         idx1_sorted, idx2_sorted);
    } catch(...){
        fclose(conn);
        conn = NULL;
        stop("Failed to read from file.");
    }
    // input.close();
    if( conn != NULL ){
        fclose(conn);
    }
    
    return( ret );
}


void coerce_real(SEXP& x, SEXP& y){
    // T must be valid SEXPTYPE in native R
    // R must be consistent with array file storage type
    
    const SEXPTYPE xtype = TYPEOF(x);
    R_xlen_t xlen = Rf_xlength(x);
    R_xlen_t ylen = Rf_xlength(y);
    if( xlen > ylen ){ xlen = ylen; }
    
    switch(xtype) {
    case REALSXP:
        memcmp(REAL(y), x, xlen * sizeof(double));
        break;
    case CPLXSXP: {
        Rcomplex* xptr = COMPLEX(x);
        double* yptr = REAL(y);
        for(R_xlen_t ii = 0; ii < xlen; ii++, yptr++, xptr++){
            if(xptr->i == NA_REAL || xptr->r == NA_REAL){
                *yptr = NA_REAL;
            } else {
                *yptr = xptr->r;
            }
        }
        break;
    }
    case INTSXP: {
        int* xptr = INTEGER(x);
        double* yptr = REAL(y);
        for(R_xlen_t ii = 0; ii < xlen; ii++, yptr++, xptr++){
            if(*xptr == NA_INTEGER){
                *yptr = NA_REAL;
            } else {
                *yptr = *xptr;
            }
        }
        break;
    }
    case RAWSXP: {
        Rbyte* xptr = RAW(x);
        double* yptr = REAL(y);
        for(R_xlen_t ii = 0; ii < xlen; ii++, yptr++, xptr++){
            *yptr = *xptr;
        }
        break;
    }
    case LGLSXP: {
        int* xptr = LOGICAL(x);
        double* yptr = REAL(y);
        for(R_xlen_t ii = 0; ii < xlen; ii++, yptr++, xptr++){
            if(*xptr == NA_LOGICAL){
                *yptr = NA_REAL;
            } else {
                *yptr = *xptr;
            }
        }
        break;
    }
    }
    
}

void coerce_float(SEXP& x, SEXP& y){
    // T must be valid SEXPTYPE in native R
    // R must be consistent with array file storage type
    
    const SEXPTYPE xtype = TYPEOF(x);
    R_xlen_t xlen = Rf_xlength(x);
    R_xlen_t ylen = Rf_xlength(y);
    if( xlen > ylen ){ xlen = ylen; }
    
    switch(xtype) {
    case REALSXP: {
        double* xptr = REAL(x);
        float* yptr = FLOAT(y);
        for(R_xlen_t ii = 0; ii < xlen; ii++, yptr++, xptr++){
            if(*xptr == NA_REAL){
                *yptr = NA_FLOAT;
            } else {
                *yptr = (float) (*xptr);
            }
        }
        break;
    }
    case CPLXSXP: {
        Rcomplex* xptr = COMPLEX(x);
        float* yptr = FLOAT(y);
        for(R_xlen_t ii = 0; ii < xlen; ii++, yptr++, xptr++){
            if(xptr->i == NA_REAL || xptr->r == NA_REAL){
                *yptr = NA_FLOAT;
            } else {
                *yptr = (float) (xptr->r);
            }
        }
        break;
    }
    case INTSXP: {
        int* xptr = INTEGER(x);
        float* yptr = FLOAT(y);
        for(R_xlen_t ii = 0; ii < xlen; ii++, yptr++, xptr++){
            if(*xptr == NA_INTEGER){
                *yptr = NA_FLOAT;
            } else {
                *yptr = (float) (*xptr);
            }
        }
        break;
    }
    case RAWSXP: {
        Rbyte* xptr = RAW(x);
        float* yptr = FLOAT(y);
        for(R_xlen_t ii = 0; ii < xlen; ii++, yptr++, xptr++){
            *yptr = *xptr;
        }
        break;
    }
    case LGLSXP: {
        int* xptr = LOGICAL(x);
        float* yptr = FLOAT(y);
        for(R_xlen_t ii = 0; ii < xlen; ii++, yptr++, xptr++){
            if(*xptr == NA_LOGICAL){
                *yptr = NA_FLOAT;
            } else {
                *yptr = (float) (*xptr);
            }
        }
        break;
    }
    }
    
}

void coerce_complex(SEXP& x, SEXP& y){
    // T must be valid SEXPTYPE in native R
    // R must be consistent with array file storage type
    
    const SEXPTYPE xtype = TYPEOF(x);
    R_xlen_t xlen = Rf_xlength(x);
    R_xlen_t ylen = Rf_xlength(y);
    if( xlen > ylen ){ xlen = ylen; }
    
    switch(xtype) {
    case REALSXP: {
        double* xptr = REAL(x);
        Rcomplex* yptr = COMPLEX(y);
        for(R_xlen_t ii = 0; ii < xlen; ii++, yptr++, xptr++){
            if(*xptr == NA_REAL){
                yptr->r = NA_REAL;
                yptr->i = NA_REAL;
            } else {
                yptr->r = *xptr;
                yptr->i = 0.0;
            }
        }
        break;
    }
    case CPLXSXP: {
        memcpy(COMPLEX(y), COMPLEX(x), xlen * sizeof(Rcomplex));
        break;
    }
    case INTSXP: {
        int* xptr = INTEGER(x);
        Rcomplex* yptr = COMPLEX(y);
        for(R_xlen_t ii = 0; ii < xlen; ii++, yptr++, xptr++){
            if(*xptr == NA_INTEGER){
                yptr->r = NA_REAL;
                yptr->i = NA_REAL;
            } else {
                yptr->r = *xptr;
                yptr->i = 0.0;
            }
        }
        break;
    }
    case RAWSXP: {
        Rbyte* xptr = RAW(x);
        Rcomplex* yptr = COMPLEX(y);
        for(R_xlen_t ii = 0; ii < xlen; ii++, yptr++, xptr++){
            yptr->r = *xptr;
            yptr->i = 0.0;
        }
        break;
    }
    case LGLSXP: {
        int* xptr = LOGICAL(x);
        Rcomplex* yptr = COMPLEX(y);
        for(R_xlen_t ii = 0; ii < xlen; ii++, yptr++, xptr++){
            if(*xptr == NA_LOGICAL){
                yptr->r = NA_REAL;
                yptr->i = NA_REAL;
            } else {
                yptr->r = *xptr;
                yptr->i = 0.0;
            }
        }
        break;
    }
    }
    
}

void corece_buffer(SEXP& x, SEXP& y, SEXPTYPE array_type){
    // please make sure y consistent with array_type
    switch(array_type){
    case REALSXP:
        coerce_real(x, y);
        break;
    case FLTSXP:
        coerce_float(x, y);
        break;
    case CPLXSXP:
        coerce_complex(x, y);
        break;
    }
}

// [[Rcpp::export]]
SEXP FARR_buffer_map2(
        std::vector<std::string>& input_filebases,
        const std::string& output_filebase,
        const int& method,
        const int& buffer_nelems
){
    // Prepare outputs
    std::string out_fbase = correct_filebase(output_filebase);
    List out_meta = FARR_meta(out_fbase);
    SEXP out_cumpart = realToInt64_inplace(out_meta["cumsum_part_sizes"]);
    SEXPTYPE out_array_type = out_meta["sexp_type"];
    SEXPTYPE out_file_type = file_buffer_sxptype(out_array_type);
    
    switch(out_file_type) {
    case REALSXP:
    case FLTSXP:
    case CPLXSXP:
        break;
    default:
        stop("C++: `FARR_buffer_map2` only supports double, float, or complex arrays");
    }
    
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
    
    int64_t nloops = in_array_length / buffer_nelems;
    if( nloops * buffer_nelems < in_array_length ){
        nloops++;
    }
    
    // allocate buffers
    int ncores = getThreads();
    if( ncores - nloops > 0 ){
        ncores = (int) nloops;
    }
    std::vector<std::vector<SEXP>> filebufferss(ncores);
    std::vector<SEXP> argbufferss(ncores);
    std::vector<SEXP> arg_converts(ncores);
    std::vector<SEXP> argbuffers(ncores);
    for( int core = 0; core < ncores; core++ ){
        std::vector<SEXP> filebuffers = std::vector<SEXP>(narrays);
        SEXP argbuffers = PROTECT(Rf_allocVector(VECSXP, narrays));
        SEXP arg_convert = PROTECT(Rf_allocVector(out_file_type, buffer_nelems));
        for(int ii = 0; ii < narrays; ii++){
            filebuffers[ii] = PROTECT(Rf_allocVector(file_buffer_types[ii], buffer_nelems));
            SET_VECTOR_ELT(argbuffers, ii, PROTECT(Rf_allocVector(memory_buffer_types[ii], buffer_nelems)));
        }
        filebufferss[core] = filebuffers;
        argbufferss[core] = argbuffers;
        arg_converts[core] = arg_convert;
    }
    
    
#pragma omp parallel num_threads(ncores)
{
#pragma omp for schedule(static, 1) nowait
    for(int64_t iter = 0 ; iter < nloops; iter++ ){
        int64_t current_pos = iter * buffer_nelems;
        int thread = iter % ncores;
        std::vector<SEXP> filebuffers = filebufferss[thread];
        SEXP argbuffers = argbufferss[thread];
        SEXP arg_convert = arg_converts[thread];
        // int64_t current_pos_save = 0;
        
        for(int ii = 0; ii < narrays; ii++){
            FARR_subset_sequential(
                input_filebases[ii],
                               in_unit_partlen,
                               cumparts[ii],
                                       arr_types[ii],
                                                filebuffers[ii],
                                                           VECTOR_ELT(argbuffers, ii),
                                                           current_pos, buffer_nelems
            );
        }
        
        // TODO: generate arg_convert
        
        
        // argbuffers
        FARR_subset_assign_sequential_bare(
            out_fbase, out_unit_partlen, 
            out_cumpart, out_array_type,
            arg_convert, current_pos
        );
        
    }
}
UNPROTECT(2 * ncores * (1 + narrays));

return(R_NilValue);
}

