#' @name v
#' @title A shortcut to create a vertical vector
#' @description
#' This function provides convenient shortcut
#' to create a vertical (column) vector.
#' @param ... arbitrary number of values
#' @return matrix with dims n_elements x 1
#' @examples
#' # Enumerating all the values with commas
#' v(1, 2, 3)
#' # Passing whole sequence as an argument
#' v(1:5)
#' @export
v <- function(...){
  matrix(c(...), ncol = 1)
}

#' @name m
#' @title A shortcut to create matrix defining rows
#' @param ... Single values, vectors, matrices
#' and `|` as special symbol which breaks input on
#' the rows.
#' @return matrix with defines elements
#' @description
#' One of the main functionalities of the package.
#' It is an alternative to standard way we define
#' matrices in R.
#' @examples
#' # Typically, we define in like this:
#' x <- matrix(c(1, 2, 3,
#'               4, 5, 6,
#'               7, 8, 9), nrow=3, byrow=TRUE)
#' x
#' # However, this way of ceating matices seems
#' # a little bit clunky. Using `matricks`, we can do
#' # it in more staightforward way dividing our input
#' # into rows by using special symbol `|`
#' x <- m(1, 2, 3|
#'        4, 5, 6|
#'        7, 8, 9)
#' x
#' # Moreover, we can pass to the `m` function
#' # whole sequences or even matrices.
#' x <- m(1:5 | 6:10 | 11:15 )
#' x
#' # We can combine multiple matrices into one
#' m(diag(3),     diag(3) * 3|
#'   diag(3) * 3, diag(3)    )
#' @import rlang
#' @export
m <- function(...){
  raw.matrix <- rlang::exprs(...)
  as.chars <-  as.character(raw.matrix)
  as.chars <- gsub('\\|(?![^()]*\\))', '), col_bind(', as.chars, perl = TRUE)
  transformed <- paste0('rbind(col_bind(', paste(as.chars, collapse = ',') ,'))')
  eval(parse(text = transformed), parent.frame())
}

#' @name with_same_dims
#' @title Create new matrix copying dimensions from the existing one
#' @param mat a matrix with desired dimensions
#' @param data sigle numeric value or numeric vector
#' @return a matrix
#' @examples
#' x <- matrix(7, 3, 6)
#' x
#' with_same_dims(x, 0)
#' with_same_dims(x, c(1, 2))
#' @export
with_same_dims <- function(mat, data) {
  matrix(data = data, nrow = nrow(mat), ncol = ncol(mat))
}

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

