#ifndef FARR_LOCK_H
#define FARR_LOCK_H

/*
 * Partition locks, protocol v1. This is a contract between filearray
 * versions; R/lock.R documents the same:
 *
 * - "<filebase>/.filearray.lock" is an empty file, created by the first
 *   process that locks. Nothing writes to it; it is never renamed or removed
 *   except together with the array, and no other code opens it.
 * - Byte k >= 1 is the exclusive write lock of partition k (file
 *   "<k-1>.farr"). Byte 0 is reserved for a future array-level lock.
 * - POSIX: classic fcntl record lock F_WRLCK on [k, k + 1), taken with
 *   non-blocking F_SETLK only. Windows: LockFileEx at offset k, length 1.
 * - Only writers lock. The locks are advisory, and the operating system
 *   releases them when a process exits.
 *
 * Locks belong to the process. A registry counts the holds per byte, so
 * nested writes and several objects for one array share a lock. Only the R
 * main thread may call these functions.
 *
 * No R headers here: lock.cpp includes <windows.h>.
 */

#include <string>
#include <vector>
#include <cstdint>

namespace farr_lock {

// Status of acquire(); keep in sync with R/lock.R
const int LOCK_OK = 0;
const int LOCK_BUSY = 1;
const int LOCK_UNSUPPORTED = 2;
const int LOCK_FAILED = 3;

// Status of link_noreplace(); keep in sync with R/lock.R
const int LINK_CREATED = 0;
const int LINK_EXISTS = 1;
const int LINK_UNSUPPORTED = 2;
const int LINK_FAILED = 3;

struct AcquireResult {
    int status;
    // number of leading bytes of the request that are held after the call
    int64_t acquired;
    int sys_error;
    std::string message;
};

struct HeldByte {
    int64_t byte;
    int64_t automatic;
    int64_t explicit_count;
};

struct RegistryInfo {
    int64_t entries;
    int64_t scopes;
    int64_t paths;
    int64_t pid;
};

// Locks `bytes` (strictly increasing, >= 1) in order and stops at the first
// byte that another process holds. Every hold taken is recorded under
// `scope_id`; bytes this process already holds are re-entered without a
// system call
AcquireResult acquire(const std::string& lock_path,
                      const std::vector<int64_t>& bytes, int64_t scope_id);

// Releases the holds recorded under a scope; returns how many
int64_t release_scope(int64_t scope_id);

// Turns the holds of a scope into explicit holds; returns how many
int64_t commit_scope(int64_t scope_id);

// Releases one explicit hold per byte, or every explicit hold when `all`;
// returns how many. Holds of scopes are never released here
int64_t release_explicit(const std::string& lock_path,
                         const std::vector<int64_t>& bytes, bool all);

// Drops every hold on a lock file and closes it; returns how many bytes
int64_t forget(const std::string& lock_path);

// Drops every hold of this process; returns how many bytes
int64_t release_all();

// Creates `to` as a hard link to `from`, unless `to` exists
int link_noreplace(const std::string& from, const std::string& to,
                   int& sys_error, std::string& message);

// Holds on a lock file, sorted by byte; false if this process holds none
bool status(const std::string& lock_path, std::vector<HeldByte>& out);

RegistryInfo info();

} // namespace farr_lock

#endif // FARR_LOCK_H
