#' @name plot_matrix
#' @title Plot a matrix
#' @description This function allows us to plot
#' @import ggplot2
#' @examples
#' x <- m(T, T, T, F, T | T, T, F, T, T | F, T, T, T, F | T, T, T, T, T | F, F, T, T, T |  F, T, T, T, F)
#' plot_matrix(x)
#' @export
plot_matrix <- function(x, ...){
  x2 <- reshape2::melt(x)
  colnames(x2) <- c("columns", "rows", "value")
  p <- ggplot(data = x2, aes(x = columns, y = rows, fill = value)) +
    geom_tile() +
    scale_x_continuous(minor_breaks = 1:ncol(x) + 0.5, breaks = 0:ncol(x) + 1) +
    theme(
      panel.background = element_rect(fill = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.ontop = TRUE) +
    coord_equal()
  return(p)
}

#' @rdname plot_matrix
#' @export
plot.matrix <- plot_matrix
