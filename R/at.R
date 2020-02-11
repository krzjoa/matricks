#' @name at
#' @title Set or get matrix value at index vector
#' @description This function allows to access matrix values by passing indices as vector
#' @param mat matrix
#' @param idx two-element integer vector
#' @param value a value to be assign at index
#' @return `at` function: value from matrix at index idx
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
