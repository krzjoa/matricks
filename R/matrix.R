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
#' @example
#' a <- m(1, 2, 3 |
#'        4, 5, 6 |
#'        7, 8, 9 )
#' @export
m <- function(...){
  # Capture user input
  raw.matrix <- rlang::exprs(...)

  # Expressions
  exprs <- unlist(sapply(raw.matrix, function(x) as.list(x)))

  # Determine row limits
  delimiter.indices <- which(sapply(exprs, deparse) == '|')
  values.only <- exprs[-delimiter.indices]

  browser()

  end.idx <- (delimiter.indices + 1) -
    cumsum(rep(1, times = length(delimiter.indices)))
  start.idx <- c(1, end.idx + 1)
  end.idx <- c(end.idx, length(values.only))
  rng <- as.data.frame(rbind(start.idx, end.idx))

  # Binding values
  out <- sapply(rng, function(x) rbind(values.only[seq.int(x[1], x[2])]))
  dimnames(out) <- NULL
  t(out)
}

# m(1, 2, 3 | 4, 5, 6 | 7, 8, (9 + 1))

# a <- m(1, 2, 3 |
#        4, 5, 6 |
#        7, 8, 9 |
#       10, 11, 12)
#
# a <- m(1, 2, 3, 4 |
#          4, 5, 6 |
#          7, 8, 9 )
