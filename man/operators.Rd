% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/operators.R
\name{operators}
\alias{operators}
\alias{\%m\%}
\alias{\%d\%}
\alias{\%-\%}
\alias{\%+\%}
\title{Binary operations on matrices/vectors}
\usage{
a \%m\% b

a \%d\% b

a \%-\% b

a \%+\% b
}
\arguments{
\item{a}{matrix/vector}

\item{b}{matrix/vector}
}
\value{
Matrix/vector
}
\description{
This operator allows to do elementwise operation of
two algebraic object i.e. matrices/vectors. There is one required condition
to perform such operation: at least one domension values from both objects
must be the same
}
\examples{
# Multiply
m(1, 2, 3 | 4, 5, 6 | 7, 8, 9) \%m\% v(5,4,3)
# Divide
m(1, 2, 3 | 4, 5, 6 | 7, 8, 9) \%d\% v(5,4,3)
# Add
m(1, 2, 3 | 4, 5, 6 | 7, 8, 9) \%+\% v(5,4,3)
# Subtract
m(1, 2, 3 | 4, 5, 6 | 7, 8, 9) \%-\% v(5,4,3)
}
