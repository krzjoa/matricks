#' @name %x%
#' @title multiply matrix by vector rowwise/columnwise
#' @description
#' This operator allows to do elementwise mutliplication of
#' two algebraic object i.e. matrices/vectors. There is one required condition
#' to perform such operation: at least one domension values from both objects
#' must be the same
#' @param a,b matrices/vectors
#' @return Matrix/vector
#' @example
#' mat <- m(1, 2, 3 | 4, 5, 6 | 7, 8, 9)
#' vec <- v(5,4,3)
#' mat %x% vec
#' @export
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
  res <- dim(a)[non.matching.dim] %% dim(b)[non.matching.dim]

  # Check dimensions
  if(!(res == 0))
    stop("Matrices dimensions don't match. ",
         deparse(substitute(a)),": ", paste(dim(a), " "), " ,",
         deparse(substitute(b)),": ", paste(dim(b), " "))

  n.times <- dim(a)[non.matching.dim] / dim(b)[non.matching.dim]

  if(matching.dim[1]){
    a * crep(b, n.times)
  } else {
    a * rrep(b, n.times)
  }
}

#' @name %/%
#' @title divide matrix by vector rowwise/columnwise
#' @description
#' This operator allows to do elementwise division of
#' two algebraic object i.e. matrices/vectors. There is one required condition
#' to perform such operation: at least one dimension values from both objects
#' must be the same
#' @param a,b matrices/vectors
#' @return Matrix/vector
#' @example
#' m(1:3 | 4:6 | 7:9) %x% v(1,2,3)
#' @export
`%/%` <- function(a, b){
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
  res <- dim(a)[non.matching.dim] %% dim(b)[non.matching.dim]

  # Check dimensions
  if(!(res == 0))
    stop("Matrices dimensions don't match. ",
         deparse(substitute(a)),": ", paste(dim(a), " "), " ,",
         deparse(substitute(b)),": ", paste(dim(b), " "))

  n.times <- dim(a)[non.matching.dim] / dim(b)[non.matching.dim]

  if(matching.dim[1]){
    a / crep(b, n.times)
  } else {
    a / rrep(b, n.times)
  }
}

#' @name %-%
#' @title subtract vector from matrix rowwise/columnwise
#' @export
`%-%` <- function(a, b){
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
  res <- dim(a)[non.matching.dim] %% dim(b)[non.matching.dim]

  # Check dimensions
  if(!(res == 0))
    stop("Matrices dimensions don't match. ",
         deparse(substitute(a)),": ", paste(dim(a), " "), " ,",
         deparse(substitute(b)),": ", paste(dim(b), " "))

  n.times <- dim(a)[non.matching.dim] / dim(b)[non.matching.dim]

  if(matching.dim[1]){
    a - crep(b, n.times)
  } else {
    a - rrep(b, n.times)
  }
}


#' @name %+%
#' @title add matrix to vector rowwise/columnwise
#' @export
`%+%` <- function(a, b){
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
  res <- dim(a)[non.matching.dim] %% dim(b)[non.matching.dim]

  # Check dimensions
  if(!(res == 0))
    stop("Matrices dimensions don't match. ",
         deparse(substitute(a)),": ", paste(dim(a), " "), " ,",
         deparse(substitute(b)),": ", paste(dim(b), " "))

  n.times <- dim(a)[non.matching.dim] / dim(b)[non.matching.dim]

  if(matching.dim[1]){
    a + crep(b, n.times)
  } else {
    a + rrep(b, n.times)
  }
}


