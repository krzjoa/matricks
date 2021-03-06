% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/index.R
\name{neighbour_idx_matrix}
\alias{neighbour_idx_matrix}
\title{Create matrix of lists, where each one contains list of neighbour field coordinates}
\usage{
neighbour_idx_matrix(mat, mask = NULL, diagonal = TRUE,
  random.select = NULL)
}
\arguments{
\item{mat}{matrix}

\item{mask}{logical matrix. Its dimensions must be identical with dimensions of mat}

\item{diagonal}{logical. get diagonal neighbours}

\item{random.select}{select one random neighbour}
}
\description{
Create matrix of lists, where each one contains list of neighbour field coordinates
}
\examples{
T <- TRUE; F <- FALSE
mat <- matrix(0, 3, 3)
mask <- m(T, T, F | T, F, T | F, F, T)
nimat <- neighbour_idx_matrix(mat, mask, diagonal = TRUE)
neighbour_idx_matrix(mat, mask, diagonal = TRUE, random.select = 1)
}
