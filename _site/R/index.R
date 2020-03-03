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
#' @param random.select select one random neighbour
#' @examples
#' T <- TRUE; F <- FALSE
#' mat <- matrix(0, 3, 3)
#' mask <- m(T, T, F | T, F, T | F, F, T)
#' nimat <- neighbour_idx_matrix(mat, mask, diagonal = TRUE)
#' neighbour_idx_matrix(mat, mask, diagonal = TRUE, random.select = 1)
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
