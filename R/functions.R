#' @name antidiag
#' @title Matrix antidiagonals
#' @description
#' Extract or replace the antidiagonal of a matrix,
#' or construct a antidiagonal matrix.
#' @param a matrix, vector or 1D array, or missing.
#' @param ncol, nrow optional dimensions for the result when x is not a matrix.
#' @details
#' diag has four distinct usages:
#' 1. x is a matrix, when it extracts the antidiagonal.
#' 2. x is missing and nrow is specified, it returns an antidiagonal identity matrix.
#' 3. x is a scalar (length-one vector) and the only argument, it returns a square antidiagonal identity matrix of size given by the scalar.
#' 4. x is a ‘numeric’ (complex, numeric, integer, logical, or raw) vector, either of length at least 2 or there were further arguments. This returns a matrix with the given antidiagonal and zero off-antidiagonal entries.
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

#' @name matrix_set
#' @title Set multiple values useing one function call
#' @description
#' This functions allows to set multiple elements of a matrix
#' instead of using annoying step-by-step assignment by
#' mat[1,2] <- 2
#' mat[2,3] <- 0.5
#' etc.
#' @examples
#' mat <- matrix(0, 4, 5)
#' matrix_set(mat, .(1,1) ~ 5, .(3, 4) ~ 0.3)
#' @export
set_matix<- function(matrix, ...){
  exprs <- list(...)

  is.formula <- sapply(exprs, function(x) inherits(x, 'formula'))

  if(!all(is.formula))
    stop(paste0("Following arguments are not formulae: ", exprs[!is.formula]))

  for(expr in exprs){
    args <- strsplit(as.character(expr), "~", fixed = TRUE)
    args <- args[args != ""]
    lh <- eval(parse(text = args[[1]]))
    rh <- as.numeric(args[[2]])
    matrix[lh[1], lh[2]] <- rh
  }
  matrix
}

#' @importFrom Rcpp sourceCpp
#' @useDynLib matricks, .registration = TRUE
NULL
