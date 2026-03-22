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

- `valid()`:

  Check if the array is valid.

## See also

[`filearray`](https://dipterix.org/filearray/reference/filearray.md)
