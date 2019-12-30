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

#' @rdname set_values
#' @export
sv <- set_values

#' @name at
#' @title Set or get matrix value at index vector
#' @description This function allows to access matrix values by passing indices as vector
#' @param mat matrix
#' @param idx two-element integer vector
#' @param value a value to be assign at index
#' @examples
#' mat <- matrix(0, 3, 3)
#' idx <- c(1, 2)
#' # Typically, given matrix and row-column indices as two-element vector, we should do it like this:
#' mat[idx[1], idx[2]]
#' mat[idx[1], idx[2]] <- 8
#' # Using `at`, we can do it simplier!
#' at(mat, idx)
#' at(mat, idx) <- 7
#' mat
#' at(mat, idx)
NULL

#' @rdname at
#' @export
at <- function(mat, idx){
  if (length(idx) > 2)
    stop("Index vector should have lenght equal to 2")
  mat[idx[1], idx[2]]
}

#' @rdname at
#' @export
`at<-` <- function(mat, idx, value){
  if (length(idx) > 2)
    stop("Index vector should have lenght equal to 2")
  mat[idx[1], idx[2]] <- value
  mat
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
#' mask <- m(F, F, T, T | F, F, F, F | T, T, F, T)
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
