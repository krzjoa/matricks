#' @name v
#' @title A shortcut to create a vector
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

#' @name binding
#' @title Bind vector, single values and matrices
#' @description
#' This function works very similar to well-known base
#' `cbind` function. However, there is one big difference
#' between these functions. If you pass a vector, each value
#' will be get individually.
#' @param ... single values, vectors, matrices or data.frames
#' @examples
#' # `col_bind` vs `cbind`
#' cbind(1,2,3,4,5)
#' col_bind(1,2,3,4,5)
#' cbind(1:5)
#' col_bind(1:5)
#' cbind(matrix(3, 3, 3), 0.33, 4:7)
#' col_bind(matrix(3, 3, 3), 0.33, 4:7)
#' # `row_bind` vs `rbind`
#' rbind(1,2,3,4,5)
#' row_bind(1,2,3,4,5)
#' rbind(1:5)
#' row_bind(1:5)
#' rbind(matrix(3, 3, 3), 0.33, 4:7)
#' row_bind(matrix(3, 3, 3), 0.33, 4:7)
NULL

.abstract_bind <- function(..., fun.name){
  fun <- function(x, y){
    y <- if (!is.matrix(y)) as.list(y) else list(y)
    c(x, y)
  }
  input <- Reduce(fun, list(...), list())
  do.call(fun.name, input)
}

#' @rdname binding
#' @export
col_bind <- function(...){
  .abstract_bind(..., fun.name = 'cbind')
}

#' @rdname binding
#' @export
row_bind <- function(...){
  .abstract_bind(..., fun.name = 'rbind')
}
