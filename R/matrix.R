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
  chars <-  as.character(raw.matrix)
  # Transform input 
  chars <- gsub('\\|(?![^()]*\\))', '), cbind(', chars, perl = TRUE)
  chars2 <- paste0('rbind(cbind(', paste(chars, collapse = ',') ,'))')
  eval(parse(text = chars2))
 
  # Cases for unit tests
  # working: m(1, 2, NA | 4, NA, 6 | (1 | 2), 8, 9)
  # not working: m(1, 2, NA | 4, NA, (6 | 9) | 7, 8, 9)                                    
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
