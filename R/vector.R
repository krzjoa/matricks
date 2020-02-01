#' @name v
#' @title A shortcut to create a vertical vector
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
