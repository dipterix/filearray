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
