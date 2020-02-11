#' @name is_idx_possible
#' @title Is idx possible in given matrix?
#' @param mat matrix
#' @param idx two-element vector
#' @examples
#' is_idx_possible(matrix(0, 3, 3), c(4, 5))
#' is_idx_possible(matrix(0, 3, 3), c(3, 2))
#' @export
is_idx_possible <- function(mat, idx){
  return(idx[1] >= 1         &
           idx[1] <= nrow(mat) &
           idx[2] >= 1         &
           idx[2] <= ncol(mat))
}

#' @name matrix_idx
#' @title Get available marix indices
#' @param mat matrix
#' @param n.row number of rows; default: NULL
#' @param n.col number of columns; default: NULL
#' @param mask logical matrix; default: NULL
#' @examples
#' T <- TRUE; F <- FALSE
#' mat <- matrix(0, 3, 3)
#' mask <- m(T, T, F | T, F, T | F, F, T)
#' # All poss
#' matrix_idx(mat)
#' matrix_idx(mat, mask = mask)
#' matrix_idx(mask = mask)
#' @export
matrix_idx <- function(mat, n.row = NULL, n.col = NULL, mask = NULL){

  if (missing(mat) & !is.null(mask))
    mat <- mask

  n.col <- if (is.null(n.col)) ncol(mat) else n.col
  n.row <- if (is.null(n.row)) nrow(mat) else n.col
  out <- expand.grid(1:n.row, 1:n.col)

  if (is.null(mask))
    mask <- matrix(TRUE, nrow = n.row, ncol = n.col)

  out2 <- asplit(out, 1)
  out2 <- Map(as.vector, out2)
  out3 <- matrix(out2, nrow = n.row, ncol = n.col)

  out3[mask]
}

#' @name seq_matrix
#' @title Return a sequence of pairs (value, index vector)
#' @description Facilitates iterating over matrix, returning a sequence of pairs,
#'  where the first element is a value at index (x, y) and the second one is the index (x, y)
#' @param mat matrix
#' @return list of two-element list (single value, two-element vector)
#' @examples
#' mat <- matrix(1:9, 3, 3)
#' seq_matrix(mat)
#' @export
seq_matrix <- function(mat){
  output <- list()

  for(i in 1:nrow(mat)){
    for(j in 1:ncol(mat)){
      output <- c(output, list(list(c(i, j), mat[[i, j]])))
    }
  }
  output
}
