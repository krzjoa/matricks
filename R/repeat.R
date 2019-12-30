#' @name repetitions
#' @title Repeat columns or rows
#' @description Repeat matrix object respectively to its shape and orientation
#' @details
#' crep = columnwise repetition
#'
#' rrep = rowwise repetition
#' @param x matrix
#' @param times number of repetitions
#' @return matrix
#' @examples
#' # Columnwise repetition
#' crep(v(1:3), 4)
#' crep(t(v(1:5)), 4)
#' # Rowwise repetition
#' rrep(v(1:3), 4)
#' rrep(t(v(1:5)), 4)
NULL

#' @rdname repetitions
#' @export
crep <- function(x, times){
  Reduce(cbind, rep(list(x), times), NULL)
}

#' @rdname repetitions
#' @export
rrep <- function(x, times){
  Reduce(rbind, rep(list(x), times), NULL)
}


