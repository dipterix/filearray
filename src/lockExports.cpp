// R interface of the partition locks (lock.h); used by R/lock.R

#include "common.h"
#include "lock.h"

#include <cmath>

using namespace Rcpp;

// Partition numbers to lock bytes, validated before anything is locked
static std::vector<int64_t> lock_bytes(const NumericVector& parts) {
    std::vector<int64_t> bytes;
    bytes.reserve(parts.size());
    double previous = 0;
    for (R_xlen_t i = 0; i < parts.size(); i++) {
        const double v = parts[i];
        if (!R_FINITE(v) || v < 1 || v != std::floor(v) ||
            v > 9007199254740992.0 || v <= previous) {
            Rcpp::stop("Partition numbers must be strictly increasing positive integers");
        }
        previous = v;
        bytes.push_back((int64_t) v);
    }
    return bytes;
}

static int64_t lock_scope_id(const double scope_id) {
    if (!R_FINITE(scope_id) || scope_id < 1 || scope_id != std::floor(scope_id)) {
        Rcpp::stop("Invalid partition lock scope");
    }
    return (int64_t) scope_id;
}

// [[Rcpp::export]]
List FARR_lock_acquire(const std::string& lock_path, const NumericVector& parts,
                       const double scope_id) {
    const std::vector<int64_t> bytes = lock_bytes(parts);
    const int64_t scope = lock_scope_id(scope_id);
    const farr_lock::AcquireResult res = farr_lock::acquire(lock_path, bytes, scope);
    return List::create(
        _["status"] = res.status,
        _["acquired"] = (double) res.acquired,
        _["sys_error"] = res.sys_error,
        _["message"] = res.message
    );
}

// [[Rcpp::export]]
double FARR_lock_release_scope(const double scope_id) {
    return (double) farr_lock::release_scope(lock_scope_id(scope_id));
}

// [[Rcpp::export]]
double FARR_lock_commit_scope(const double scope_id) {
    return (double) farr_lock::commit_scope(lock_scope_id(scope_id));
}

// [[Rcpp::export]]
double FARR_lock_release_explicit(const std::string& lock_path,
                                  const NumericVector& parts,
                                  const bool all = false) {
    std::vector<int64_t> bytes;
    if (!all) {
        bytes = lock_bytes(parts);
    }
    return (double) farr_lock::release_explicit(lock_path, bytes, all);
}

// [[Rcpp::export]]
double FARR_lock_forget(const std::string& lock_path) {
    return (double) farr_lock::forget(lock_path);
}

// [[Rcpp::export]]
double FARR_lock_release_all() {
    return (double) farr_lock::release_all();
}

// [[Rcpp::export]]
IntegerVector FARR_link_noreplace(const std::string& from, const std::string& to) {
    int sys_error = 0;
    std::string message;
    const int status = farr_lock::link_noreplace(from, to, sys_error, message);
    IntegerVector re = IntegerVector::create(status);
    if (!message.empty()) {
        re.attr("message") = message;
    }
    return re;
}

// [[Rcpp::export]]
List FARR_lock_status(const std::string& lock_path) {
    std::vector<farr_lock::HeldByte> held;
    const bool open = farr_lock::status(lock_path, held);
    const R_xlen_t n = (R_xlen_t) held.size();
    NumericVector partition(n);
    IntegerVector automatic(n);
    IntegerVector explicit_count(n);
    for (R_xlen_t i = 0; i < n; i++) {
        partition[i] = (double) held[i].byte;
        automatic[i] = (int) held[i].automatic;
        explicit_count[i] = (int) held[i].explicit_count;
    }
    return List::create(
        _["open"] = open,
        _["partition"] = partition,
        _["automatic"] = automatic,
        _["explicit"] = explicit_count
    );
}

// [[Rcpp::export]]
List FARR_lock_registry() {
    const farr_lock::RegistryInfo info = farr_lock::info();
    return List::create(
        _["entries"] = (double) info.entries,
        _["scopes"] = (double) info.scopes,
        _["paths"] = (double) info.paths,
        _["pid"] = (double) info.pid
    );
}
