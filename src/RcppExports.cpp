// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// FARR_collapse
SEXP FARR_collapse(const std::string& filebase, const NumericVector& dim, const IntegerVector& keep, const NumericVector& cum_part, SEXPTYPE array_type, int method, bool remove_na, double scale);
RcppExport SEXP _filearray_FARR_collapse(SEXP filebaseSEXP, SEXP dimSEXP, SEXP keepSEXP, SEXP cum_partSEXP, SEXP array_typeSEXP, SEXP methodSEXP, SEXP remove_naSEXP, SEXP scaleSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::string& >::type filebase(filebaseSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type dim(dimSEXP);
    Rcpp::traits::input_parameter< const IntegerVector& >::type keep(keepSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type cum_part(cum_partSEXP);
    Rcpp::traits::input_parameter< SEXPTYPE >::type array_type(array_typeSEXP);
    Rcpp::traits::input_parameter< int >::type method(methodSEXP);
    Rcpp::traits::input_parameter< bool >::type remove_na(remove_naSEXP);
    Rcpp::traits::input_parameter< double >::type scale(scaleSEXP);
    rcpp_result_gen = Rcpp::wrap(FARR_collapse(filebase, dim, keep, cum_part, array_type, method, remove_na, scale));
    return rcpp_result_gen;
END_RCPP
}
// FARR_collapse_complex
SEXP FARR_collapse_complex(const std::string& filebase, const NumericVector& dim, const IntegerVector& keep, const NumericVector& cum_part, int method, bool remove_na, double scale);
RcppExport SEXP _filearray_FARR_collapse_complex(SEXP filebaseSEXP, SEXP dimSEXP, SEXP keepSEXP, SEXP cum_partSEXP, SEXP methodSEXP, SEXP remove_naSEXP, SEXP scaleSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::string& >::type filebase(filebaseSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type dim(dimSEXP);
    Rcpp::traits::input_parameter< const IntegerVector& >::type keep(keepSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type cum_part(cum_partSEXP);
    Rcpp::traits::input_parameter< int >::type method(methodSEXP);
    Rcpp::traits::input_parameter< bool >::type remove_na(remove_naSEXP);
    Rcpp::traits::input_parameter< double >::type scale(scaleSEXP);
    rcpp_result_gen = Rcpp::wrap(FARR_collapse_complex(filebase, dim, keep, cum_part, method, remove_na, scale));
    return rcpp_result_gen;
END_RCPP
}
// check_missing_dots
SEXP check_missing_dots(const SEXP env);
RcppExport SEXP _filearray_check_missing_dots(SEXP envSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const SEXP >::type env(envSEXP);
    rcpp_result_gen = Rcpp::wrap(check_missing_dots(env));
    return rcpp_result_gen;
END_RCPP
}
// set_buffer_size
int set_buffer_size(int size);
RcppExport SEXP _filearray_set_buffer_size(SEXP sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(set_buffer_size(size));
    return rcpp_result_gen;
END_RCPP
}
// get_buffer_size
int get_buffer_size();
RcppExport SEXP _filearray_get_buffer_size() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(get_buffer_size());
    return rcpp_result_gen;
END_RCPP
}
// kinda_sorted
int kinda_sorted(SEXP idx, int64_t min_, int64_t buffer_count);
RcppExport SEXP _filearray_kinda_sorted(SEXP idxSEXP, SEXP min_SEXP, SEXP buffer_countSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type idx(idxSEXP);
    Rcpp::traits::input_parameter< int64_t >::type min_(min_SEXP);
    Rcpp::traits::input_parameter< int64_t >::type buffer_count(buffer_countSEXP);
    rcpp_result_gen = Rcpp::wrap(kinda_sorted(idx, min_, buffer_count));
    return rcpp_result_gen;
END_RCPP
}
// realToUint64
SEXP realToUint64(NumericVector x, const double min_, const double max_, const int strict);
RcppExport SEXP _filearray_realToUint64(SEXP xSEXP, SEXP min_SEXP, SEXP max_SEXP, SEXP strictSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const double >::type min_(min_SEXP);
    Rcpp::traits::input_parameter< const double >::type max_(max_SEXP);
    Rcpp::traits::input_parameter< const int >::type strict(strictSEXP);
    rcpp_result_gen = Rcpp::wrap(realToUint64(x, min_, max_, strict));
    return rcpp_result_gen;
END_RCPP
}
// locationList
SEXP locationList(const SEXP listOrEnv, const NumericVector& dim, const int strict);
RcppExport SEXP _filearray_locationList(SEXP listOrEnvSEXP, SEXP dimSEXP, SEXP strictSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const SEXP >::type listOrEnv(listOrEnvSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type dim(dimSEXP);
    Rcpp::traits::input_parameter< const int >::type strict(strictSEXP);
    rcpp_result_gen = Rcpp::wrap(locationList(listOrEnv, dim, strict));
    return rcpp_result_gen;
END_RCPP
}
// reshape_or_drop
SEXP reshape_or_drop(SEXP x, SEXP reshape, bool drop);
RcppExport SEXP _filearray_reshape_or_drop(SEXP xSEXP, SEXP reshapeSEXP, SEXP dropSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type reshape(reshapeSEXP);
    Rcpp::traits::input_parameter< bool >::type drop(dropSEXP);
    rcpp_result_gen = Rcpp::wrap(reshape_or_drop(x, reshape, drop));
    return rcpp_result_gen;
END_RCPP
}
// loc2idx
SEXP loc2idx(const List sliceIdx, const NumericVector& dim);
RcppExport SEXP _filearray_loc2idx(SEXP sliceIdxSEXP, SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type sliceIdx(sliceIdxSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type dim(dimSEXP);
    rcpp_result_gen = Rcpp::wrap(loc2idx(sliceIdx, dim));
    return rcpp_result_gen;
END_RCPP
}
// schedule
List schedule(const SEXP listOrEnv, const NumericVector& dim, const NumericVector& cum_part_sizes, const int split_dim, const int strict);
RcppExport SEXP _filearray_schedule(SEXP listOrEnvSEXP, SEXP dimSEXP, SEXP cum_part_sizesSEXP, SEXP split_dimSEXP, SEXP strictSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const SEXP >::type listOrEnv(listOrEnvSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type dim(dimSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type cum_part_sizes(cum_part_sizesSEXP);
    Rcpp::traits::input_parameter< const int >::type split_dim(split_dimSEXP);
    Rcpp::traits::input_parameter< const int >::type strict(strictSEXP);
    rcpp_result_gen = Rcpp::wrap(schedule(listOrEnv, dim, cum_part_sizes, split_dim, strict));
    return rcpp_result_gen;
END_RCPP
}
// cplxToReal2
SEXP cplxToReal2(SEXP x);
RcppExport SEXP _filearray_cplxToReal2(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cplxToReal2(x));
    return rcpp_result_gen;
END_RCPP
}
// realToCplx2
SEXP realToCplx2(SEXP x);
RcppExport SEXP _filearray_realToCplx2(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(realToCplx2(x));
    return rcpp_result_gen;
END_RCPP
}
// realToFloat2
SEXP realToFloat2(SEXP x);
RcppExport SEXP _filearray_realToFloat2(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(realToFloat2(x));
    return rcpp_result_gen;
END_RCPP
}
// floatToReal2
SEXP floatToReal2(SEXP x);
RcppExport SEXP _filearray_floatToReal2(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(floatToReal2(x));
    return rcpp_result_gen;
END_RCPP
}
// get_float_na
SEXP get_float_na();
RcppExport SEXP _filearray_get_float_na() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(get_float_na());
    return rcpp_result_gen;
END_RCPP
}
// FARR_subset
SEXP FARR_subset(const std::string& filebase, const SEXPTYPE type, const SEXP listOrEnv, const NumericVector& dim, const NumericVector& cum_part_sizes, const int split_dim, const SEXP reshape, const bool drop, const int strict, const SEXP dimnames, const bool half_size);
RcppExport SEXP _filearray_FARR_subset(SEXP filebaseSEXP, SEXP typeSEXP, SEXP listOrEnvSEXP, SEXP dimSEXP, SEXP cum_part_sizesSEXP, SEXP split_dimSEXP, SEXP reshapeSEXP, SEXP dropSEXP, SEXP strictSEXP, SEXP dimnamesSEXP, SEXP half_sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::string& >::type filebase(filebaseSEXP);
    Rcpp::traits::input_parameter< const SEXPTYPE >::type type(typeSEXP);
    Rcpp::traits::input_parameter< const SEXP >::type listOrEnv(listOrEnvSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type dim(dimSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type cum_part_sizes(cum_part_sizesSEXP);
    Rcpp::traits::input_parameter< const int >::type split_dim(split_dimSEXP);
    Rcpp::traits::input_parameter< const SEXP >::type reshape(reshapeSEXP);
    Rcpp::traits::input_parameter< const bool >::type drop(dropSEXP);
    Rcpp::traits::input_parameter< const int >::type strict(strictSEXP);
    Rcpp::traits::input_parameter< const SEXP >::type dimnames(dimnamesSEXP);
    Rcpp::traits::input_parameter< const bool >::type half_size(half_sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(FARR_subset(filebase, type, listOrEnv, dim, cum_part_sizes, split_dim, reshape, drop, strict, dimnames, half_size));
    return rcpp_result_gen;
END_RCPP
}
// FARR_buffer_mapreduce
SEXP FARR_buffer_mapreduce(const std::string& filebase, const Function map, const Nullable<Function> reduce, const NumericVector& dim, const NumericVector& partition_cumlens, const int bufferlen, const SEXPTYPE x_type);
RcppExport SEXP _filearray_FARR_buffer_mapreduce(SEXP filebaseSEXP, SEXP mapSEXP, SEXP reduceSEXP, SEXP dimSEXP, SEXP partition_cumlensSEXP, SEXP bufferlenSEXP, SEXP x_typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::string& >::type filebase(filebaseSEXP);
    Rcpp::traits::input_parameter< const Function >::type map(mapSEXP);
    Rcpp::traits::input_parameter< const Nullable<Function> >::type reduce(reduceSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type dim(dimSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type partition_cumlens(partition_cumlensSEXP);
    Rcpp::traits::input_parameter< const int >::type bufferlen(bufferlenSEXP);
    Rcpp::traits::input_parameter< const SEXPTYPE >::type x_type(x_typeSEXP);
    rcpp_result_gen = Rcpp::wrap(FARR_buffer_mapreduce(filebase, map, reduce, dim, partition_cumlens, bufferlen, x_type));
    return rcpp_result_gen;
END_RCPP
}
// getThreads
int getThreads(bool max);
RcppExport SEXP _filearray_getThreads(SEXP maxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< bool >::type max(maxSEXP);
    rcpp_result_gen = Rcpp::wrap(getThreads(max));
    return rcpp_result_gen;
END_RCPP
}
// setThreads
int setThreads(int n, int reset_after_fork);
RcppExport SEXP _filearray_setThreads(SEXP nSEXP, SEXP reset_after_forkSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type reset_after_fork(reset_after_forkSEXP);
    rcpp_result_gen = Rcpp::wrap(setThreads(n, reset_after_fork));
    return rcpp_result_gen;
END_RCPP
}
// hasOpenMP
bool hasOpenMP();
RcppExport SEXP _filearray_hasOpenMP() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(hasOpenMP());
    return rcpp_result_gen;
END_RCPP
}
// FARR_subset_assign
SEXP FARR_subset_assign(const std::string& filebase, const SEXP listOrEnv, const NumericVector& dim, const NumericVector& cum_part_sizes, const int split_dim, const SEXPTYPE type, SEXP value_);
RcppExport SEXP _filearray_FARR_subset_assign(SEXP filebaseSEXP, SEXP listOrEnvSEXP, SEXP dimSEXP, SEXP cum_part_sizesSEXP, SEXP split_dimSEXP, SEXP typeSEXP, SEXP value_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::string& >::type filebase(filebaseSEXP);
    Rcpp::traits::input_parameter< const SEXP >::type listOrEnv(listOrEnvSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type dim(dimSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type cum_part_sizes(cum_part_sizesSEXP);
    Rcpp::traits::input_parameter< const int >::type split_dim(split_dimSEXP);
    Rcpp::traits::input_parameter< const SEXPTYPE >::type type(typeSEXP);
    Rcpp::traits::input_parameter< SEXP >::type value_(value_SEXP);
    rcpp_result_gen = Rcpp::wrap(FARR_subset_assign(filebase, listOrEnv, dim, cum_part_sizes, split_dim, type, value_));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_filearray_FARR_collapse", (DL_FUNC) &_filearray_FARR_collapse, 8},
    {"_filearray_FARR_collapse_complex", (DL_FUNC) &_filearray_FARR_collapse_complex, 7},
    {"_filearray_check_missing_dots", (DL_FUNC) &_filearray_check_missing_dots, 1},
    {"_filearray_set_buffer_size", (DL_FUNC) &_filearray_set_buffer_size, 1},
    {"_filearray_get_buffer_size", (DL_FUNC) &_filearray_get_buffer_size, 0},
    {"_filearray_kinda_sorted", (DL_FUNC) &_filearray_kinda_sorted, 3},
    {"_filearray_realToUint64", (DL_FUNC) &_filearray_realToUint64, 4},
    {"_filearray_locationList", (DL_FUNC) &_filearray_locationList, 3},
    {"_filearray_reshape_or_drop", (DL_FUNC) &_filearray_reshape_or_drop, 3},
    {"_filearray_loc2idx", (DL_FUNC) &_filearray_loc2idx, 2},
    {"_filearray_schedule", (DL_FUNC) &_filearray_schedule, 5},
    {"_filearray_cplxToReal2", (DL_FUNC) &_filearray_cplxToReal2, 1},
    {"_filearray_realToCplx2", (DL_FUNC) &_filearray_realToCplx2, 1},
    {"_filearray_realToFloat2", (DL_FUNC) &_filearray_realToFloat2, 1},
    {"_filearray_floatToReal2", (DL_FUNC) &_filearray_floatToReal2, 1},
    {"_filearray_get_float_na", (DL_FUNC) &_filearray_get_float_na, 0},
    {"_filearray_FARR_subset", (DL_FUNC) &_filearray_FARR_subset, 11},
    {"_filearray_FARR_buffer_mapreduce", (DL_FUNC) &_filearray_FARR_buffer_mapreduce, 7},
    {"_filearray_getThreads", (DL_FUNC) &_filearray_getThreads, 1},
    {"_filearray_setThreads", (DL_FUNC) &_filearray_setThreads, 2},
    {"_filearray_hasOpenMP", (DL_FUNC) &_filearray_hasOpenMP, 0},
    {"_filearray_FARR_subset_assign", (DL_FUNC) &_filearray_FARR_subset_assign, 7},
    {NULL, NULL, 0}
};

int detectForked(DllInfo *dll);
RcppExport void R_init_filearray(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    detectForked(dll);
}