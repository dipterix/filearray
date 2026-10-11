// Partition locks: registry and operating-system primitives. See lock.h for
// the protocol. No R headers in this file: it includes <windows.h>.

#include "lock.h"

#include <algorithm>
#include <cerrno>
#include <cstring>
#include <functional>
#include <map>
#include <mutex>
#include <set>

#ifdef _WIN32
#  ifndef NOMINMAX
#    define NOMINMAX
#  endif
#  ifndef WIN32_LEAN_AND_MEAN
#    define WIN32_LEAN_AND_MEAN
#  endif
#  include <windows.h>
#else
#  include <fcntl.h>
#  include <sys/stat.h>
#  include <sys/types.h>
#  include <unistd.h>
#endif

namespace farr_lock {

namespace {

#ifdef _WIN32
typedef HANDLE os_handle;
const char* const LOCK_CALL = "LockFileEx";
#else
typedef int os_handle;
const char* const LOCK_CALL = "fcntl(F_SETLK)";
#endif

// Identity of a lock file: device and inode (volume serial number and file
// index on Windows), so that every path of one file shares a registry entry
struct Key {
    uint64_t dev;
    uint64_t ino;
    bool operator<(const Key& other) const {
        return dev < other.dev || (dev == other.dev && ino < other.ino);
    }
    bool operator==(const Key& other) const {
        return dev == other.dev && ino == other.ino;
    }
};

struct Counts {
    int64_t automatic;
    int64_t explicit_count;
    Counts() : automatic(0), explicit_count(0) {}
    int64_t total() const { return automatic + explicit_count; }
};

struct Entry {
    os_handle handle;
    // Descriptors of the same file opened through another path. On POSIX,
    // closing any of them would release the locks held through `handle`
    std::vector<os_handle> aux;
    // bytes this process holds
    std::map<int64_t, Counts> bytes;
};

struct Scope {
    Key key;
    // one element per hold
    std::vector<int64_t> bytes;
};

struct Registry {
    std::mutex mutex;
    long pid;
    std::map<Key, Entry> entries;
    std::map<int64_t, Scope> scopes;
    std::map<std::string, std::set<Key> > paths;
    // POSIX descriptors whose file could not be identified; closing one might
    // release locks, so they stay open until release_all()
    std::vector<os_handle> orphans;
};

// Key for files without a usable identity (some network or FUSE file systems)
Key path_key(const std::string& path) {
    Key key;
    key.dev = UINT64_MAX;
    key.ino = (uint64_t) std::hash<std::string>()(path);
    return key;
}

#ifdef _WIN32

long current_pid() {
    return (long) GetCurrentProcessId();
}

std::string system_message(const char* call, const std::string& path, int err) {
    return std::string(call) + " '" + path + "' failed (Windows error " +
        std::to_string(err) + ")";
}

std::wstring utf8_to_wide(const std::string& s) {
    int n = MultiByteToWideChar(CP_UTF8, 0, s.c_str(), -1, NULL, 0);
    if (n <= 0) {
        return std::wstring();
    }
    std::wstring w((size_t) n, L'\0');
    MultiByteToWideChar(CP_UTF8, 0, s.c_str(), -1, &w[0], n);
    w.resize((size_t) n - 1);
    return w;
}

void close_handle(os_handle h) {
    if (h != INVALID_HANDLE_VALUE && h != NULL) {
        CloseHandle(h);
    }
}

bool handle_key(os_handle h, const std::string& path, Key& key) {
    BY_HANDLE_FILE_INFORMATION info;
    if (!GetFileInformationByHandle(h, &info)) {
        return false;
    }
    const uint64_t index = ((uint64_t) info.nFileIndexHigh << 32) |
        (uint64_t) info.nFileIndexLow;
    if (index == 0) {
        key = path_key(path);
    } else {
        key.dev = (uint64_t) info.dwVolumeSerialNumber;
        key.ino = index;
    }
    return true;
}

// Identity without creating the file; another handle is safe on Windows,
// where locks belong to handles
bool stat_key(const std::string& path, Key& key) {
    const std::wstring wpath = utf8_to_wide(path);
    if (wpath.empty()) {
        return false;
    }
    HANDLE h = CreateFileW(
        wpath.c_str(), FILE_READ_ATTRIBUTES,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
        NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (h == INVALID_HANDLE_VALUE) {
        return false;
    }
    const bool ok = handle_key(h, path, key);
    CloseHandle(h);
    return ok;
}

int open_lock_file(const std::string& path, os_handle& h, int& err,
                   std::string& message) {
    const std::wstring wpath = utf8_to_wide(path);
    if (wpath.empty()) {
        message = "cannot convert '" + path + "' to UTF-16";
        return LOCK_FAILED;
    }
    DWORD e = 0;
    for (int i = 0; i < 4; i++) {
        // GENERIC_READ is enough for LockFileEx, and does not conflict with
        // programs that open the file sharing only FILE_SHARE_READ
        h = CreateFileW(
            wpath.c_str(), GENERIC_READ,
            FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
            NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
        if (h != INVALID_HANDLE_VALUE) {
            return LOCK_OK;
        }
        e = GetLastError();
        if (e != ERROR_SHARING_VIOLATION && e != ERROR_LOCK_VIOLATION) {
            break;
        }
        // antivirus or synchronization software holding the file briefly
        if (i < 3) {
            Sleep(5);
        }
    }
    err = (int) e;
    message = system_message("CreateFileW", path, err);
    if (e == ERROR_SHARING_VIOLATION || e == ERROR_LOCK_VIOLATION) {
        return LOCK_BUSY;
    }
    if (e == ERROR_ACCESS_DENIED || e == ERROR_WRITE_PROTECT ||
        e == ERROR_NOT_SUPPORTED) {
        return LOCK_UNSUPPORTED;
    }
    return LOCK_FAILED;
}

void set_offset(OVERLAPPED& ov, int64_t byte) {
    ZeroMemory(&ov, sizeof(ov));
    ov.Offset = (DWORD) ((uint64_t) byte & 0xFFFFFFFFULL);
    ov.OffsetHigh = (DWORD) ((uint64_t) byte >> 32);
}

int os_trylock(os_handle h, int64_t byte, int& err) {
    // one OVERLAPPED per call; the handle is synchronous
    OVERLAPPED ov;
    set_offset(ov, byte);
    if (LockFileEx(h, LOCKFILE_EXCLUSIVE_LOCK | LOCKFILE_FAIL_IMMEDIATELY,
                   0, 1, 0, &ov)) {
        return LOCK_OK;
    }
    const DWORD e = GetLastError();
    err = (int) e;
    if (e == ERROR_LOCK_VIOLATION) {
        return LOCK_BUSY;
    }
    if (e == ERROR_NOT_SUPPORTED || e == ERROR_INVALID_FUNCTION) {
        return LOCK_UNSUPPORTED;
    }
    return LOCK_FAILED;
}

void os_unlock(os_handle h, int64_t byte) {
    // must match the locked region exactly
    OVERLAPPED ov;
    set_offset(ov, byte);
    UnlockFileEx(h, 0, 1, 0, &ov);
}

int os_link(const std::string& from, const std::string& to, int& err,
            std::string& message) {
    const std::wstring wfrom = utf8_to_wide(from);
    const std::wstring wto = utf8_to_wide(to);
    if (wfrom.empty() || wto.empty()) {
        message = "cannot convert '" + to + "' to UTF-16";
        return LINK_FAILED;
    }
    if (CreateHardLinkW(wto.c_str(), wfrom.c_str(), NULL)) {
        return LINK_CREATED;
    }
    const DWORD e = GetLastError();
    err = (int) e;
    if (e == ERROR_ALREADY_EXISTS || e == ERROR_FILE_EXISTS) {
        return LINK_EXISTS;
    }
    message = system_message("CreateHardLinkW", to, err);
    // no hard links on this volume (or no permission to make one): the
    // caller falls back to a rename, which reports its own errors
    if (e == ERROR_INVALID_FUNCTION || e == ERROR_NOT_SUPPORTED ||
        e == ERROR_NOT_SAME_DEVICE || e == ERROR_ACCESS_DENIED) {
        return LINK_UNSUPPORTED;
    }
#ifdef ERROR_TOO_MANY_LINKS
    if (e == ERROR_TOO_MANY_LINKS) {
        return LINK_UNSUPPORTED;
    }
#endif
    return LINK_FAILED;
}

#else // POSIX

long current_pid() {
    return (long) getpid();
}

std::string system_message(const char* call, const std::string& path, int err) {
    return std::string(call) + " '" + path + "' failed: " + std::strerror(err) +
        " (errno " + std::to_string(err) + ")";
}

void close_handle(os_handle fd) {
    if (fd >= 0) {
        ::close(fd);
    }
}

Key make_key(const struct stat& st, const std::string& path) {
    if (st.st_ino == 0) {
        return path_key(path);
    }
    Key key;
    key.dev = (uint64_t) st.st_dev;
    key.ino = (uint64_t) st.st_ino;
    return key;
}

// Identity without opening the file
bool stat_key(const std::string& path, Key& key) {
    struct stat st;
    if (::stat(path.c_str(), &st) != 0) {
        return false;
    }
    key = make_key(st, path);
    return true;
}

bool handle_key(os_handle fd, const std::string& path, Key& key) {
    struct stat st;
    if (::fstat(fd, &st) != 0) {
        return false;
    }
    key = make_key(st, path);
    return true;
}

// Errors meaning that this file system or directory cannot hold locks
bool errno_unsupported(int err) {
    if (err == ENOLCK || err == ENOSYS || err == EINVAL || err == EIO ||
        err == EACCES || err == EPERM || err == EROFS) {
        return true;
    }
#ifdef ENOTSUP
    if (err == ENOTSUP) {
        return true;
    }
#endif
#ifdef EOPNOTSUPP
    if (err == EOPNOTSUPP) {
        return true;
    }
#endif
    return false;
}

int open_lock_file(const std::string& path, os_handle& fd, int& err,
                   std::string& message) {
    int flags = O_RDWR | O_CREAT;
#ifdef O_CLOEXEC
    flags |= O_CLOEXEC;
#endif
    for (int i = 0; i < 3; i++) {
        // F_WRLCK needs a descriptor open for writing; 0666 & ~umask, like
        // the partition files
        fd = ::open(path.c_str(), flags, 0666);
        if (fd >= 0 || errno != EINTR) {
            break;
        }
    }
    if (fd < 0) {
        err = errno;
        message = system_message("open", path, err);
        return errno_unsupported(err) ? LOCK_UNSUPPORTED : LOCK_FAILED;
    }
#ifndef O_CLOEXEC
    ::fcntl(fd, F_SETFD, FD_CLOEXEC);
#endif
    return LOCK_OK;
}

void set_range(struct flock& fl, short type, int64_t byte) {
    // zeroed, so that platform-specific fields (l_pid, l_sysid) are 0
    std::memset(&fl, 0, sizeof(fl));
    fl.l_type = type;
    fl.l_whence = SEEK_SET;
    fl.l_start = (off_t) byte;
    fl.l_len = 1;
}

int os_trylock(os_handle fd, int64_t byte, int& err) {
    struct flock fl;
    set_range(fl, F_WRLCK, byte);
    // never F_SETLKW: a blocking wait could not be interrupted
    for (int i = 0; i < 3; i++) {
        if (::fcntl(fd, F_SETLK, &fl) == 0) {
            return LOCK_OK;
        }
        err = errno;
        if (err == EINTR) {
            continue;
        }
        if (err == EAGAIN || err == EACCES) {
            return LOCK_BUSY;
        }
        return errno_unsupported(err) ? LOCK_UNSUPPORTED : LOCK_FAILED;
    }
    return LOCK_BUSY;
}

void os_unlock(os_handle fd, int64_t byte) {
    struct flock fl;
    set_range(fl, F_UNLCK, byte);
    // if this fails, closing the descriptor releases the lock
    for (int i = 0; i < 3; i++) {
        if (::fcntl(fd, F_SETLK, &fl) == 0 || errno != EINTR) {
            break;
        }
    }
}

int os_link(const std::string& from, const std::string& to, int& err,
            std::string& message) {
    if (::link(from.c_str(), to.c_str()) == 0) {
        return LINK_CREATED;
    }
    err = errno;
    if (err == EEXIST) {
        return LINK_EXISTS;
    }
    message = system_message("link", to, err);
    // no hard links on this file system (or no permission to make one): the
    // caller falls back to a rename, which reports its own errors
    if (err == EPERM || err == EACCES || err == ENOSYS || err == EXDEV ||
        err == EMLINK) {
        return LINK_UNSUPPORTED;
    }
#ifdef ENOTSUP
    if (err == ENOTSUP) {
        return LINK_UNSUPPORTED;
    }
#endif
#ifdef EOPNOTSUPP
    if (err == EOPNOTSUPP) {
        return LINK_UNSUPPORTED;
    }
#endif
    return LINK_FAILED;
}

// In a fork() child: close a descriptor inherited from the parent, but only
// if it still refers to the lock file
void close_if_same_file(os_handle fd, const Key& key) {
    struct stat st;
    if (key.dev != UINT64_MAX && ::fstat(fd, &st) == 0 &&
        (uint64_t) st.st_dev == key.dev && (uint64_t) st.st_ino == key.ino) {
        ::close(fd);
    }
}

#endif

Registry& registry() {
    // never destroyed: the operating system releases the locks at exit
    static Registry* r = []() {
        Registry* reg = new Registry();
        reg->pid = current_pid();
        return reg;
    }();
    return *r;
}

// A fork() child owns none of its parent's locks; closing its copies of the
// parent's descriptors cannot release the parent's classic fcntl locks
void check_fork(Registry& r) {
    const long pid = current_pid();
    if (pid == r.pid) {
        return;
    }
#ifndef _WIN32
    for (std::map<Key, Entry>::iterator it = r.entries.begin();
         it != r.entries.end(); ++it) {
        close_if_same_file(it->second.handle, it->first);
        for (size_t i = 0; i < it->second.aux.size(); i++) {
            close_if_same_file(it->second.aux[i], it->first);
        }
    }
#endif
    r.entries.clear();
    r.scopes.clear();
    r.paths.clear();
    r.orphans.clear();
    r.pid = pid;
}

void forget_key_paths(Registry& r, const Key& key) {
    for (std::map<std::string, std::set<Key> >::iterator it = r.paths.begin();
         it != r.paths.end(); ) {
        it->second.erase(key);
        if (it->second.empty()) {
            it = r.paths.erase(it);
        } else {
            ++it;
        }
    }
}

void close_entry(Registry& r, std::map<Key, Entry>::iterator it) {
    const Key key = it->first;
    close_handle(it->second.handle);
    for (size_t i = 0; i < it->second.aux.size(); i++) {
        close_handle(it->second.aux[i]);
    }
    r.entries.erase(it);
    forget_key_paths(r, key);
}

// Closes a lock file once this process holds nothing in it, so descriptors are
// only open for arrays with locks, and Windows can delete idle arrays
void close_if_idle(Registry& r, const Key& key) {
    std::map<Key, Entry>::iterator it = r.entries.find(key);
    if (it != r.entries.end() && it->second.bytes.empty()) {
        close_entry(r, it);
    }
}

// Opens (or reuses) the registry entry of a lock file
int open_entry(Registry& r, const std::string& path, Key& key, int& err,
               std::string& message) {
#ifndef _WIN32
    // Never open a second descriptor for a file this process holds locks
    // in: closing it later would release them
    Key known;
    if (stat_key(path, known) && r.entries.count(known)) {
        key = known;
        r.paths[path].insert(key);
        return LOCK_OK;
    }
#endif
    os_handle h;
    const int status = open_lock_file(path, h, err, message);
    if (status != LOCK_OK) {
        return status;
    }
    Key opened;
    if (!handle_key(h, path, opened)) {
#ifdef _WIN32
        err = (int) GetLastError();
        message = system_message("GetFileInformationByHandle", path, err);
        CloseHandle(h);
#else
        err = errno;
        message = system_message("fstat", path, err);
        r.orphans.push_back(h);
#endif
        return LOCK_FAILED;
    }
    std::map<Key, Entry>::iterator it = r.entries.find(opened);
    if (it == r.entries.end()) {
        Entry entry;
        entry.handle = h;
        r.entries[opened] = entry;
    } else {
#ifdef _WIN32
        // Windows locks belong to handles: closing another one is safe
        CloseHandle(h);
#else
        // the path changed to a file this process already has open
        it->second.aux.push_back(h);
#endif
    }
    r.paths[path].insert(opened);
    key = opened;
    return LOCK_OK;
}

// Drops one hold of `byte`, and unlocks the byte when none is left
bool release_hold(Entry& entry, int64_t byte, bool explicit_hold) {
    std::map<int64_t, Counts>::iterator it = entry.bytes.find(byte);
    if (it == entry.bytes.end()) {
        return false;
    }
    Counts& counts = it->second;
    if (explicit_hold) {
        if (counts.explicit_count <= 0) {
            return false;
        }
        counts.explicit_count--;
    } else {
        if (counts.automatic <= 0) {
            return false;
        }
        counts.automatic--;
    }
    if (counts.total() == 0) {
        os_unlock(entry.handle, byte);
        entry.bytes.erase(it);
    }
    return true;
}

// Registry keys of a lock path: the identities it had when locked, and the one
// it has now
std::set<Key> resolve(Registry& r, const std::string& path) {
    std::set<Key> keys;
    std::map<std::string, std::set<Key> >::iterator it = r.paths.find(path);
    if (it != r.paths.end()) {
        keys = it->second;
    }
    Key key;
    if (stat_key(path, key)) {
        keys.insert(key);
    }
    return keys;
}

} // namespace

AcquireResult acquire(const std::string& lock_path,
                      const std::vector<int64_t>& bytes, int64_t scope_id) {
    AcquireResult result;
    result.status = LOCK_OK;
    result.acquired = 0;
    result.sys_error = 0;

    Registry& r = registry();
    std::lock_guard<std::mutex> guard(r.mutex);
    check_fork(r);
    if (bytes.empty()) {
        return result;
    }

    Key key;
    const int status = open_entry(r, lock_path, key, result.sys_error,
                                  result.message);
    if (status != LOCK_OK) {
        result.status = status;
        return result;
    }

    std::map<int64_t, Scope>::iterator sit = r.scopes.find(scope_id);
    if (sit != r.scopes.end() && !(sit->second.key == key)) {
        close_if_idle(r, key);
        result.status = LOCK_FAILED;
        result.message = "a lock scope cannot hold partitions of two arrays";
        return result;
    }
    Scope& scope = r.scopes[scope_id];
    scope.key = key;
    Entry& entry = r.entries[key];
    const size_t first_new = scope.bytes.size();

    for (size_t i = 0; i < bytes.size(); i++) {
        const int64_t byte = bytes[i];
        std::map<int64_t, Counts>::iterator held = entry.bytes.find(byte);
        if (held == entry.bytes.end()) {
            int err = 0;
            const int s = os_trylock(entry.handle, byte, err);
            if (s == LOCK_BUSY) {
                // leading bytes stay held while the caller waits for this one
                result.status = LOCK_BUSY;
                break;
            }
            if (s != LOCK_OK) {
                // undo this call; earlier calls of the scope keep their holds
                for (size_t j = scope.bytes.size(); j > first_new; j--) {
                    release_hold(entry, scope.bytes[j - 1], false);
                }
                scope.bytes.resize(first_new);
                result.status = s;
                result.acquired = 0;
                result.sys_error = err;
                result.message = system_message(LOCK_CALL, lock_path, err);
                break;
            }
            held = entry.bytes.insert(std::make_pair(byte, Counts())).first;
        }
        held->second.automatic++;
        scope.bytes.push_back(byte);
        result.acquired++;
    }

    if (scope.bytes.empty()) {
        r.scopes.erase(scope_id);
    }
    close_if_idle(r, key);
    return result;
}

int64_t release_scope(int64_t scope_id) {
    Registry& r = registry();
    std::lock_guard<std::mutex> guard(r.mutex);
    check_fork(r);

    std::map<int64_t, Scope>::iterator sit = r.scopes.find(scope_id);
    if (sit == r.scopes.end()) {
        return 0;
    }
    const Scope scope = sit->second;
    r.scopes.erase(sit);

    int64_t released = 0;
    std::map<Key, Entry>::iterator it = r.entries.find(scope.key);
    if (it != r.entries.end()) {
        for (size_t j = scope.bytes.size(); j > 0; j--) {
            if (release_hold(it->second, scope.bytes[j - 1], false)) {
                released++;
            }
        }
        close_if_idle(r, scope.key);
    }
    return released;
}

int64_t commit_scope(int64_t scope_id) {
    Registry& r = registry();
    std::lock_guard<std::mutex> guard(r.mutex);
    check_fork(r);

    std::map<int64_t, Scope>::iterator sit = r.scopes.find(scope_id);
    if (sit == r.scopes.end()) {
        return 0;
    }
    const Scope scope = sit->second;
    r.scopes.erase(sit);

    int64_t committed = 0;
    std::map<Key, Entry>::iterator it = r.entries.find(scope.key);
    if (it == r.entries.end()) {
        return 0;
    }
    for (size_t j = 0; j < scope.bytes.size(); j++) {
        std::map<int64_t, Counts>::iterator held =
            it->second.bytes.find(scope.bytes[j]);
        if (held != it->second.bytes.end() && held->second.automatic > 0) {
            held->second.automatic--;
            held->second.explicit_count++;
            committed++;
        }
    }
    return committed;
}

int64_t release_explicit(const std::string& lock_path,
                         const std::vector<int64_t>& bytes, bool all) {
    Registry& r = registry();
    std::lock_guard<std::mutex> guard(r.mutex);
    check_fork(r);

    int64_t released = 0;
    const std::set<Key> keys = resolve(r, lock_path);
    for (std::set<Key>::const_iterator k = keys.begin(); k != keys.end(); ++k) {
        std::map<Key, Entry>::iterator it = r.entries.find(*k);
        if (it == r.entries.end()) {
            continue;
        }
        Entry& entry = it->second;
        if (all) {
            std::vector<int64_t> held;
            for (std::map<int64_t, Counts>::iterator b = entry.bytes.begin();
                 b != entry.bytes.end(); ++b) {
                if (b->second.explicit_count > 0) {
                    held.push_back(b->first);
                }
            }
            for (size_t i = 0; i < held.size(); i++) {
                while (release_hold(entry, held[i], true)) {
                    released++;
                }
            }
        } else {
            for (size_t i = 0; i < bytes.size(); i++) {
                if (release_hold(entry, bytes[i], true)) {
                    released++;
                }
            }
        }
        close_if_idle(r, *k);
    }
    return released;
}

int64_t forget(const std::string& lock_path) {
    Registry& r = registry();
    std::lock_guard<std::mutex> guard(r.mutex);
    check_fork(r);

    int64_t dropped = 0;
    const std::set<Key> keys = resolve(r, lock_path);
    for (std::set<Key>::const_iterator k = keys.begin(); k != keys.end(); ++k) {
        std::map<Key, Entry>::iterator it = r.entries.find(*k);
        if (it == r.entries.end()) {
            continue;
        }
        for (std::map<int64_t, Counts>::iterator b = it->second.bytes.begin();
             b != it->second.bytes.end(); ++b) {
            os_unlock(it->second.handle, b->first);
            dropped++;
        }
        it->second.bytes.clear();
        close_entry(r, it);
        for (std::map<int64_t, Scope>::iterator s = r.scopes.begin();
             s != r.scopes.end(); ) {
            if (s->second.key == *k) {
                s = r.scopes.erase(s);
            } else {
                ++s;
            }
        }
    }
    return dropped;
}

int64_t release_all() {
    Registry& r = registry();
    std::lock_guard<std::mutex> guard(r.mutex);
    check_fork(r);

    int64_t dropped = 0;
    for (std::map<Key, Entry>::iterator it = r.entries.begin();
         it != r.entries.end(); ++it) {
        for (std::map<int64_t, Counts>::iterator b = it->second.bytes.begin();
             b != it->second.bytes.end(); ++b) {
            os_unlock(it->second.handle, b->first);
            dropped++;
        }
        close_handle(it->second.handle);
        for (size_t i = 0; i < it->second.aux.size(); i++) {
            close_handle(it->second.aux[i]);
        }
    }
    for (size_t i = 0; i < r.orphans.size(); i++) {
        close_handle(r.orphans[i]);
    }
    r.entries.clear();
    r.scopes.clear();
    r.paths.clear();
    r.orphans.clear();
    return dropped;
}

int link_noreplace(const std::string& from, const std::string& to,
                   int& sys_error, std::string& message) {
    sys_error = 0;
    return os_link(from, to, sys_error, message);
}

bool status(const std::string& lock_path, std::vector<HeldByte>& out) {
    Registry& r = registry();
    std::lock_guard<std::mutex> guard(r.mutex);
    check_fork(r);

    bool open = false;
    const std::set<Key> keys = resolve(r, lock_path);
    for (std::set<Key>::const_iterator k = keys.begin(); k != keys.end(); ++k) {
        std::map<Key, Entry>::iterator it = r.entries.find(*k);
        if (it == r.entries.end()) {
            continue;
        }
        open = true;
        for (std::map<int64_t, Counts>::iterator b = it->second.bytes.begin();
             b != it->second.bytes.end(); ++b) {
            HeldByte held;
            held.byte = b->first;
            held.automatic = b->second.automatic;
            held.explicit_count = b->second.explicit_count;
            out.push_back(held);
        }
    }
    std::sort(out.begin(), out.end(),
              [](const HeldByte& a, const HeldByte& b) { return a.byte < b.byte; });
    return open;
}

RegistryInfo info() {
    Registry& r = registry();
    std::lock_guard<std::mutex> guard(r.mutex);
    check_fork(r);

    RegistryInfo result;
    result.entries = (int64_t) r.entries.size();
    result.scopes = (int64_t) r.scopes.size();
    result.paths = (int64_t) r.paths.size();
    result.pid = (int64_t) r.pid;
    return result;
}

} // namespace farr_lock
