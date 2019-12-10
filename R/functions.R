#' @name set_values
#' @title Set multiple values useing one function call
#' @param matrix a matrix object
#' @param ... formulae; left hand values should be two-element interger vectors and right-hand: a single-value numeric
#' @return matrix
#' @description
#' This functions allows to set multiple elements of a matrix
#' instead of using annoying step-by-step assignment by
#' mat[1,2] <- 2
#' mat[2,3] <- 0.5
#' etc.
#' @examples
#' mat <- matrix(0, 4, 5)
#' set_values(mat, c(1,1) ~ 5, c(3, 4) ~ 0.3)
#' @export
set_values<- function(matrix, ...){
  exprs <- list(...)

  is.formula <- sapply(exprs, function(x) inherits(x, 'formula'))

  if(!all(is.formula))
    stop(paste0("Following arguments are not formulae: ", exprs[!is.formula]))

  for(expr in exprs){
    args <- strsplit(as.character(expr), "~", fixed = TRUE)
    args <- args[args != ""]
    lh <- eval(parse(text = args[[1]]))
    rh <- as.numeric(args[[2]])
    matrix[lh[1], lh[2]] <- rh
  }
  matrix
}

#' @name repetitions
#' @title Repeat columns or rows
#' @description Repeat matrix object respectively to its shape and orientation
#' @details
#' crep = columnwise repeat
#' rrep = rowwise repeat
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
