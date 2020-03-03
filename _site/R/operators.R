#' @name operators
#' @title Binary operations on matrices/vectors
#' @description
#' This operator allows to do elementwise operation of
#' two algebraic object i.e. matrices/vectors. There is one required condition
#' to perform such operation: at least one domension values from both objects
#' must be the same
#' @param a matrix/vector
#' @param b matrix/vector
#' @return Matrix/vector
#' @examples
#' # Multiply
#' m(1, 2, 3 | 4, 5, 6 | 7, 8, 9) %m% v(5,4,3)
#' # Divide
#' m(1, 2, 3 | 4, 5, 6 | 7, 8, 9) %d% v(5,4,3)
#' # Add
#' m(1, 2, 3 | 4, 5, 6 | 7, 8, 9) %+% v(5,4,3)
#' # Subtract
#' m(1, 2, 3 | 4, 5, 6 | 7, 8, 9) %-% v(5,4,3)
NULL

.abstract_operator <- function(a, b, ops){
   # Check types
  if(!is.matrix(a) | !is.matrix(b))
    stop("Error! you cannot apply matrix elemntwise multipliation on non-matrix objects!")

  matching.dim <- dim(a) == dim(b)

  # Match dimensions
  if(!any(matching.dim))
    stop("Matrices dimensions don't match. ",
         deparse(substitute(a)),": ", paste(dim(a), " "), " ,",
         deparse(substitute(b)),": ", paste(dim(b), " "))

  non.matching.dim <- !matching.dim
  res <- dim(a)[non.matching.dim] %% dim(b)[non.matching.dim]

  # Check dimensions
  if(!(res == 0))
    stop("Matrices dimensions don't match. ",
         deparse(substitute(a)),": ", paste(dim(a), " "), " ,",
         deparse(substitute(b)),": ", paste(dim(b), " "))

   n.times <- dim(a)[non.matching.dim] / dim(b)[non.matching.dim]

   if(matching.dim[1]){
    ops(a, crep(b, n.times))
  } else {
    ops(a, rrep(b, n.times))
  }
}

#' @rdname operators
#' @export
`%m%` <- function(a, b){
  .abstract_operator(a, b, `*`)
}

#' @rdname operators
#' @export
`%d%` <- function(a, b){
  .abstract_operator(a, b, `/`)
}

#' @rdname operators
#' @export
`%-%` <- function(a, b){
  .abstract_operator(a, b, `-`)
}

#' @rdname operators
#' @export
`%+%` <- function(a, b){
  .abstract_operator(a, b, `+`)
}
