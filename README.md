# matricks
> Useful tricks for algebraic computations in R.

### Installation
```r
devtools::install_github('krzjoa/matricks')
```
### Usage
Main `matricks` funcions are `m` and `v`, which provide convenient API to create matrices and vectors.  Why should we write:
```r
matrix(c(5, 6, 7,
         8, 0, 9,
         3, 7, 1), nrow = 3, bycol = TRUE)
```
if we can simply create such a matrix like that:
```r
library(matricks)

m(5, 6, 7 |
  8, 0, 9 |
  3, 7, 1 )

```


