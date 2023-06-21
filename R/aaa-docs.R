## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib filearray, .registration = TRUE
## usethis namespace: end
NULL

#' @title 'S3' methods for 'FileArray'
#' @name S3-filearray
#' @description These are 'S3' methods for 'FileArray'
#' @param x a file array
#' @param drop whether to drop dimensions; see topic \code{\link[base]{Extract}}
#' @param reshape a new dimension to set before returning subset results; default is \code{NULL} (use default dimensions)
#' @param strict whether to allow indices to exceed bound; currently only accept \code{TRUE}
#' @param dimnames whether to preserve \code{\link[base]{dimnames}}
#' @param value value to substitute or set
#' @param na.rm whether to remove \code{NA} values during the calculation
#' @param split_dim internally used; split dimension and calculate indices to
#' manually speed up the subset; value ranged from 0 to size of dimension minus
#' one.
#' @param lazy whether to lazy-evaluate the method, only works when assigning
#' arrays with logical array index
#' @param i,... index set, or passed to other methods
#' @param .env environment to evaluate formula when evaluating subset margin indices.
NULL

#' @name S4-filearray
#' @title 'S4' methods for \code{FileArray}
#' @param x,z,e1,e2 \code{FileArray} or compatible data
#' @param base,digits,... passed to other methods
#' @returns See \code{\link[methods]{S4groupGeneric}}
NULL
