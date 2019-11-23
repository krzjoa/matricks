#' @name v
#' @title A shortcut to create a vector
#' @section Creating matrices and vectors
#' @description
#' This function provides convenient shortcut
#' to create a vertical (column) vector.
#' @param ... arbitrary number of values
#' @return matrix with dims n_elements x 1
#' @examples
#' # Enumerating all the values with commas
#' v(1, 2, 3)
#' #      [,1]
#' # [1,]    1
#' # [2,]    2
#' # [3,]    3
#'
#' # Passing whole sequence as an argument
#' v(1:5)
#' #      [,1]
#' # [1,]    1
#' # [2,]    2
#' # [3,]    3
#' # [4,]    4
#' # [5,]    5
#'
#' @export
v <- function(...){
  matrix(c(...), ncol = 1)
}

#' @name m
#' @title A shortcut to create matrix defining rows
#' @section Creating matrices and vectors
#' @param ... Single values, vectors, matrices
#' and `|` as special symbol which breaks input on
#' the rows.
#' @description
#' One of the main functionalities of the package.
#' It is an alternative to standard way we define
#' matrices in R.
#' @examples
#' # Typically, we define in like this:
#' x <- matrix(c(1, 2, 3,
#'               4, 5, 6,
#'               7, 8, 9, nrow=3, byrow=TRUE))
#' # However, this way of ceating matices seems
#' # a little bit clunky. Using `matricks`, we can do
#' it in more staightforward way dividing our input
#' into rows by using special symbol `|`
#'
#' x <- m(1, 2, 3 |
#'        4, 5, 6 |
#'        7, 8, 9 )
#'
#' # Moreover, we can pass to the `m` function
#' # whole sequences or even matrices.
#' x <- m(1:5 | 6:10 | 11:15 )
#'
#' i <- diag(3)
#' a <- i * 3
#' x <- m(i, a |
#'        a, i)
#' #      [,1] [,2] [,3] [,4] [,5] [,6]
#' # [1,]    1    0    0    3    0    0
#' # [2,]    0    1    0    0    3    0
#' # [3,]    0    0    1    0    0    3
#' # [4,]    3    0    0    1    0    0
#' # [5,]    0    3    0    0    1    0
#' # [6,]    0    0    3    0    0    1
#'
#' @export
m <- function(...){
  raw.matrix <- rlang::exprs(...)
  chars <-  as.character(raw.matrix)
  chars <- gsub('\\|(?![^()]*\\))', '), col_bind(', chars, perl = TRUE)
  chars2 <- paste0('rbind(col_bind(', paste(chars, collapse = ',') ,'))')
  eval(parse(text = chars2))
}

#' @name col_bind
#' @title Bind vector, single values and matrices
#' @section Creating matrices and vectors
#' @description
#' This function works very similar to well-known base
#' `cbind` function. However, there is one big difference
#' between these functions. If you pass a vector, each value
#' will be get individually.
#' @examples
#' cbind(1:5)
#'      [,1]
#' [1,]    1
#' [2,]    2
#' [3,]    3
#' [4,]    4
#' [5,]    5
#' col_bind(1:5)
#'      [,1] [,2] [,3] [,4] [,5]
#' [1,]    1    2    3    4    5
#'
#'
#'
col_bind <- function(...){
  fun <- function(x, y){
    y <- if(!is.matrix(y)) as.list(y) else list(y)
    c(x, y)
  }
  input <- Reduce(fun, list(...), list())
  do.call('cbind', input)
}
