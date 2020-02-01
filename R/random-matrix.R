#' @name runifm
#' @title Create matrix of random values drawn from uniform distribution
#' @param nrow number of rows
#' @param ncol numer of columns
#' @param min lower limit of the distribution. Must be finite.
#' @param max upper limit of the distribution. Must be finite.
#' @importFrom stats runif
#' @return a matrix
#' @examples
#' runifm(3, 3)
#' runifm(4, 5, min = -1, max = 3)
#' @export
runifm <- function(nrow, ncol, min = 0, max = 1){
  n <- nrow * ncol
  matrix(runif(n = n, min = min, max = max), nrow = nrow, ncol = ncol)
}

#' @name rboolm
#' @title Create matrix of random choosen boolean values
#' @param nrow number of rows
#' @param ncol numer of columns
#' @param true.proba probability of true values; default: 0.5
#' @importFrom stats runif
#' @return a matrix
#' @examples
#' rboolm(3, 3)
#' rboolm(4, 5, true.proba = 0.3)
#' @export
rboolm <- function(nrow, ncol, true.proba = 0.5){
  n <- nrow * ncol
  matrix(runif(n = n), nrow = nrow, ncol = ncol) <= true.proba
}

#' @name runif_same_dims
#' @title Create matrix of random values with dimensions copied from an existing matrix
#' @param mat matrix
#' @param min lower limit of the distribution. Must be finite.
#' @param max upper limit of the distribution. Must be finite.
#' @importFrom stats runif
#' @return a matrix
#' @examples
#' mat <- matrix(0, 3, 3)
#' runif_same_dims(mat)
#' @export
runif_same_dims <- function(mat, min = 0, max = 1){
  data <- runif(length(mat), min = min, max = max)
  matrix(data = data, nrow = nrow(mat), ncol = ncol(mat))
}
