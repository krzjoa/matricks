#' @name plot_matrix
#' @title Plot a matrix
#' @description This function allows us to plot matrices easily
#' @param x a matrix
#' @param ... for S3 generic API consistency; does nothing
#' @return a ggplot object
#' @import ggplot2
#' @examples
#' T <- TRUE; F <- FALSE
#' x1 <- m(T, T, T, F, T |
#'         T, T, F, T, T |
#'         F, T, T, T, F |
#'         T, T, T, T, T |
#'         F, F, T, T, T |
#'         F, T, T, T, F)
#' plot_matrix(x1)
#' x2 <- m(T, T, T, F, T |
#'         T, T, F, T, T )
#' plot(x2)
#' x3 <- m(runif(3) | runif(3) | runif(3))
#' plot(x3)
#' @export
plot_matrix <- function(x, ...){
  # To pass CRAN check
  columns <- rows <- value <- NULL
  x2 <- reshape2::melt(x)
  colnames(x2) <- c("rows", "columns", "value")
  ggplot(data = x2, aes(x = columns, y = rows, fill = value)) +
    geom_tile() +
    scale_x_continuous(minor_breaks = 1:ncol(x) + 0.5, breaks = 0:ncol(x) + 1) +
    scale_y_reverse(minor_breaks = nrow(x):1 + 0.5, breaks = nrow(x):0, labels = nrow(x):0) +
    theme(
      panel.background = element_rect(fill = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.ontop = TRUE) +
    coord_equal()
}

#' @rdname plot_matrix
#' @export
plot.matrix <- plot_matrix
