#' @name v
#' @title A shortcut to create a vector
#' @section Creating matrices and vectors
#' @description
#' This function provides convenient shortcut
#' to create a vertical (column) vector.
#' @param ... arbitrary number of values
#' @return matrix with dims n_elements x 1
#' @example
#' v(1, 2, 3)
#'      [,1]
#' [1,]    1
#' [2,]    2
#' [3,]    3
#' @export
v <- function(...){
  matrix(c(...), ncol = 1)
}

#' @name m
#' @title A shortcut to create matrix defining rows
#' @section Creating matrices and vectors
#' @description
#' @example
#' a <- m(1, 2, 3 |
#'        4, 5, 6 |
#'        7, 8, 9 )
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
col_bind <- function(...){
  fun <- function(x, y){
    y <- if(!is.matrix(y)) as.list(y) else list(y)
    c(x, y)
  }
  input <- Reduce(fun, list(...), list())
  do.call('cbind', input)
}
