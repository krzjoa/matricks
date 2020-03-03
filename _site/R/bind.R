#' @name binding
#' @title Bind vector, single values and matrices
#' @description
#' This functions works very similar to well-known base
#' `cbind` or `rbind`  function. However, there is one big difference
#' between these functions. If you pass a vector, each value
#' will be get individually.
#' @param ... single values, vectors, matrices or data.frames
#' @return a matrix being a product of matrix/vector/values binding
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
