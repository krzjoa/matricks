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


#' @title Check, is all elements are matrices
#' @examples
#' are_matrix(matrix(0, 3, 3), m(1:3 | 4:6), "trolo")
are_matrices <- function(...){
  Reduce("&", Map(is.matrix, list(...)), TRUE)
}



#' @name neighbour_idx
#' @title Get all indices in neighbourhood
#' @param mat matrix or data.frame
#' @param idx two-element vector
#' @param mask logical matrix; optional
#' @param diagonal include diagonal neighbours
#' @param include.idx include current index
#' @examples
#' mat <- matrix(0, 3, 3)
#' neighbour_idx(mat, c(1, 2))
#' neighbour_idx(mat, c(1, 2), diagonal = FALSE)
#' neighbour_idx(mat, c(1, 2), diagonal = FALSE, include.idx = TRUE)
#' # With mask
#' mat <- matrix(0, 3, 4)
#' mask <- m(FALSE, FALSE, TRUE, TRUE |
#'           FALSE, FALSE, FALSE, FALSE |
#'           TRUE, TRUE, FALSE, TRUE)
#' neighbour_idx(mat, c(1, 2), mask = mask)
#' @export
neighbour_idx <- function(mat, idx, mask = NULL, diagonal = TRUE, include.idx = FALSE){
  n.row <- nrow(mat)
  n.col <- ncol(mat)
  nidx <- NULL
  min.row <- max(1, idx[1]-1)
  max.row <- min(n.row, idx[1] + 1)
  min.col <- max(1, idx[2] - 1)
  max.col <- min(n.col, idx[2] + 1)

  for (i in min.row:max.row) {
    for (j in min.col:max.col) {

      if (!include.idx & idx[1] == i & idx[2] == j)
        next

      if (!diagonal & (abs(idx[1]-i) + abs(idx[2] - j)) == 2)
        next

      if (!is.null(mask)) {
        if (mask[i, j])
          nidx <- c(nidx, list(c(i, j)))
      } else {
        nidx <- c(nidx, list(c(i, j)))
      }
    }
  }
  nidx
}


#' @name neighbour_idx_matrix
#' @title Create matrix of lists, where each one contains list of neighbour field coordinates
#' @param mat matrix
#' @param mask logical matrix. Its dimensions must be identical with dimensions of mat
#' @param diagonal logical. get diagonal neighbours
#' @example
#' mat <- matrix(0, 3, 3)
#' mask <- m(T, T, F | T, F, T | F, F, T)
#' nimat <- neighbour_idx_matrix(mat, mask, diagonal = TRUE)
#' neighbour_idx_matrix(mat, mask, diagonal = TRUE, random.select = 1) # TODO: > 2 error
#' @export
neighbour_idx_matrix <- function(mat, mask = NULL, diagonal = TRUE, random.select = NULL){
  n.row <- nrow(mat)
  n.col <- ncol(mat)

  idx.matrix <- matrix(list(), nrow = n.row, ncol = n.col)

  for (i in 1:n.row) {
    for (j in 1:n.col) {

      if (!is.null(mask))
        if (!mask[i, j])
          next

      neighbours <- neighbour_idx(mat,
                                  idx      = c(i, j),
                                  mask     = mask,
                                  diagonal = diagonal)

      if (is.numeric(random.select))
        neighbours <- sample(neighbours, random.select)[[1]]

      idx.matrix[[i, j]] <- neighbours
    }
  }
  idx.matrix
}


#' @name matrix_idx
#' @title Get available marix indices
#' @param x matrix
#' @param n.row number of rows; default: NULL
#' @param n.col number of columns; default: NULL
#' @param mask logical matrix; default: NULL
#' @examples
#' mat <- matrix(0, 3, 3)
#' mask <- m(T, T, F | T, F, T | F, F, T)
#' # All poss
#' matrix_idx(mat)
#' matrix_idx(mat, mask = mask)
#' matrix_idx(mask = mask)
#' @export
matrix_idx <- function(x, n.row = NULL, n.col = NULL, mask = NULL){

  if (missing(x) & !is.null(mask))
    x <- mask

  n.col <- if (is.null(n.col)) ncol(x) else n.col
  n.row <- if (is.null(n.row)) nrow(x) else n.col
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
#' @importFrom reshape2 melt
#' @importFrom purrr map2
#' @return list of two-element list (single value, two-element vector)
#' @examples
#' mat <- matrix(1:9, 3, 3)
#' iter_matrix(mat)
#' @export
seq_matrix <- function(mat){
  # browser()
  Var1 <- list()
  Var2 <- list()

  for(i in 1:nrow(mat)){
    for(j in 1:ncol(mat)){
      Var1 <- c(Var1, list(c(i, j)))
      Var2 <- c(Var2, list(mat[[i, j]]))
    }
  }

  # browser()

  # coord.val <-  reshape2::melt(mat)
  # coord.val$coord <- purrr::map2(Var1, Var2, ~ c(.x, .y))
  purrr::map2(Var1, Var2, ~ list(.x, .y))
}
