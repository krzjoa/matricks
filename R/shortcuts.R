#' @name zeros
#' @title Create matrix with zeros values only
#' @param nrow number of rows
#' @param ncol number of columns
#' @example
#' zeros(4, 5)
#' @export
zeros <- function(nrow, ncol){
  matrix(0, nrow = nrow, ncol = ncol)
}

#' @name ones
#' @title Create matrix with ones values only
#' @inheritParams zeros
#' @example
#' ones(4, 5)
#' @export
ones <- function(nrow, ncol){
  matrix(1, nrow = nrow, ncol = ncol)
}
