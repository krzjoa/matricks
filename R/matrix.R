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
#' # Typically, we define matrices like this:
#' x <- matrix(c(1, 2, 3,
#'               4, 5, 6,
#'               7, 8, 9), nrow=3, byrow=TRUE)
#' x
#' # However, this way of ceating matices seems to be
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
  as.chars <- gsub('\\|(?![^()]*\\))', '), matricks::col_bind(', as.chars, perl = TRUE)
  transformed <- paste0('rbind(matricks::col_bind(', paste(as.chars, collapse = ',') ,'))')
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


