# Definition of file array

`S4` class definition of `FileArray`. Please use
[`filearray_create`](https://dipterix.org/filearray/reference/filearray.md)
and
[`filearray_load`](https://dipterix.org/filearray/reference/filearray.md)
to create instances.

## Public Methods

- `get_header(key, default = NULL)`:

  Get header information; returns `default` if `key` is missing

- `set_header(key, value)`:

  Set header information; the extra headers will be stored in meta file.
  Please do not store large headers as they will be loaded into memory
  frequently.

- `can_write()`:

  Whether the array data can be altered

- `create(filebase, dimension, type = "double", partition_size = 1)`:

  Create a file array instance

- `delete(force = FALSE)`:

  Remove array from local file system and reset

- `dimension()`:

  Get dimension vector

- `dimnames(v)`:

  Set/get dimension names

- `element_size()`:

  Internal storage: bytes per element

- `fill_partition(part, value)`:

  Fill a partition with given scalar

- `get_partition(part, reshape = NULL)`:

  Get partition data, and reshape (if not null) to desired dimension

- `expand(n)`:

  Expand array along the last margin; returns true if expanded; if the
  `dimnames` have been assigned prior to expansion, the last dimension
  names will be filled with `NA`

- `initialize_partition()`:

  Make sure a partition file exists; if not, create one and fill with
  `NA`s or 0 (`type='raw'`)

- `load(filebase, mode = c("readwrite", "readonly"))`:

  Load file array from existing directory

- `lock_partition(parts, timeout = NULL)`:

  Lock partitions `parts` (default: all) for this R session, so that
  other processes cannot write to them; waits and retries until all of
  them are locked or `timeout` seconds have passed (`NULL`: option
  `filearray.lock.timeout`, which defaults to waiting without limit;
  `0`: try once). Returns `TRUE` once locked, or `FALSE` after the
  time-out, with nothing locked; see section 'Partition locks'

- `partition_path(part)`:

  Get partition file path

- `partition_size()`:

  Get partition size; see
  [`filearray`](https://dipterix.org/filearray/reference/filearray.md)

- `set_partition(part, value, ..., strict = TRUE)`:

  Set partition value

- `sexp_type()`:

  Get data `SEXP` type; see R internal manuals

- `show()`:

  Print information

- `type()`:

  Get data type

- `unlock_partition(parts)`:

  Release one `lock_partition` lock on each of `parts`, or every
  `lock_partition` lock this R session holds on the array if `parts` is
  missing; returns `TRUE` if a lock was released

- `valid()`:

  Check if the array is valid.

## Partition locks

Several processes (for example parallel, callr or mirai workers) can
write to one array. Writes lock the partitions they change:
sub-assignment (`x[...] <- value`), `fill_partition`,
`initialize_partition`, `set_partition`,
[`fmap`](https://dipterix.org/filearray/reference/fmap.md),
[`fmap_element_wise`](https://dipterix.org/filearray/reference/fmap.md)
and
[`filearray_bind`](https://dipterix.org/filearray/reference/filearray_bind.md)
wait while another process holds one of their partitions, so concurrent
writers never lose each other's data. Reading takes no locks, so a
reader may see a write that is still in progress. Functions that read
but must first create missing partitions (`max`, `min`, `range` and
`sum` with `na.rm = FALSE`, inputs of `fmap`, `filearray_bind`,
evaluating array operations) lock the partitions they create, so they
can wait too.

To keep partitions to yourself over several operations, for example to
check whether a partition has been computed and write it only if not,
lock them explicitly:


    if (!x$lock_partition(k, timeout = 60)) stop("partition ", k, " is busy")
    on.exit(x$unlock_partition(k), add = TRUE)
    if (is.na(x[1, 1, k])) x[, , k] <- compute(k)

Locks belong to the R session, not to the object: every object of the
same array shares them, writes of this session to partitions it holds do
not wait, and each `lock_partition` call needs one `unlock_partition`
call. Lock everything you need in one call: two sessions that each hold
one partition and wait for the other's wait until their time-outs.
Likewise, do not hold partitions that forked workers (`mclapply`) of the
same session write. Non-interactive workers should set a time-out.

The locks are byte-range locks on an empty file `.filearray.lock` in the
array directory (byte `k` locks partition `k`). They are advisory: older
versions of this package and other programs ignore them. The operating
system releases them when a process exits, so a crashed or killed
process never leaves a stale lock. The locks guard writes but never make
them fail: when a lock cannot be taken for a reason other than another
process holding it (a file system without lock support such as some
network mounts, no permission to create the lock file, a network error),
writes warn once and proceed unlocked, and `lock_partition` warns and
returns `TRUE` with attribute `locked = FALSE`. Cloud-synchronized
folders lock on one computer only. Copying or archiving the array
directory in the session that holds its locks releases them on `POSIX`
systems. To lock anything else the same way, see
[`with_filelock`](https://dipterix.org/filearray/reference/with_filelock.md).

Options (each falls back to the environment variable, which worker
processes inherit):

- `filearray.lock` (`FILEARRAY_LOCK`):

  set to `FALSE` to stop writes from locking automatically;
  `lock_partition` still locks

- `filearray.lock.timeout` (`FILEARRAY_LOCK_TIMEOUT`):

  seconds a write waits for a locked partition before it fails with an
  error of class `filearray_lock_timeout`; default `Inf`, wait until the
  partition is free. A value that is not a non-negative number warns
  once and is ignored

## See also

[`filearray`](https://dipterix.org/filearray/reference/filearray.md),
[`with_filelock`](https://dipterix.org/filearray/reference/with_filelock.md)
