#' @name set_values
#' @title Set multiple values useing one function call
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

#' @name crep
#' @title Repeat columns
#' @param x matrix
#' @param times number of repetitions
#' @return matrix
#' @examples
#' crep(v(1:3), 4)
#' @export
crep <- function(x, times){
  Reduce(cbind, rep(list(x), times), NULL)
}
#' @name rrep
#' @title Repeat columns
#' @param x matrix
#' @param times number of repetitions
#' @return matrix
#' @examples
#' rrep(v(1:3), 4)
#' rrep(t(v(1:5)), 4)
#' @export
rrep <- function(x, times){
  Reduce(rbind, rep(list(x), times), NULL)
}


#' @importFrom Rcpp sourceCpp
#' @useDynLib matricks, .registration = TRUE
NULL
