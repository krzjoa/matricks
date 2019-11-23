# nrm <- function(x, z){
#   t(t(x)*z)
# }

#' @name antidiag
#' @title Matrix antidiagonals
#' @description
#' Extract or replace the antidiagonal of a matrix,
#' or construct a antidiagonal matrix.
#' @param a matrix, vector or 1D array, or missing.
#' @param ncol, nrow optional dimensions for the result when x is not a matrix.
#' @details
#' diag has four distinct usages:
#' # 1. x is a matrix, when it extracts the antidiagonal.
#' # 2. x is missing and nrow is specified, it returns an antidiagonal identity matrix.
#' # 3. x is a scalar (length-one vector) and the only argument, it returns a square antidiagonal identity matrix of size given by the scalar.
#' # 4. x is a ‘numeric’ (complex, numeric, integer, logical, or raw) vector, either of length at least 2 or there were further arguments. This returns a matrix with the given antidiagonal and zero off-antidiagonal entries.
# # It is an error to specify nrow or ncol in the first case.
antidiag <- function(x = 1, nrow, ncol){

}

`diag<-` <- antidiag
