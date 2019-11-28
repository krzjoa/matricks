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


#' @name %x%
#' @title multiply matrix by vector (or matrix) rowwise/columnwise
#' @description
#' This operator allows to do elementwise mutliplication of
#' two algebraic object i.e. matrices/vectors. There is one required condition
#' to perform such operation: at least one domension values from both objects
#' must be the same
#' @param a,b matrices/vectors
#' @return Matrix/vector
`%x%` <- function(a, b){
  # Chech types
  if(!is.matrix(a) | !is.matrix(b))
    stop("Error! you cannot apply matrix elemntwise multipliation on non-matrix objects!")

  matching.dim <- dim(a) == dim(b)

  # Match dimensions
  if(!any(matching.dim))
    stop("Matrices dimensions don't match. ",
         deparse(substitute(a)),": ", paste(dim(a), " "), " ,",
         deparse(substitute(b)),": ", paste(dim(b), " "))

  non.matching.dim <- !matching.dim

  # Check dimensions
  if(dim(a)[non.matching.dim] < dim(b)[non.matching.dim])
    stop("")
}


m(1,2,3 | 4,5,6 | 7:9) -> X
q <- v(2,3,4,5)

10 %% 3


X %x% q




