
<!-- README.md is generated from README.Rmd. Please edit that file -->

# matricks <img src='man/figures/logo.png' align="right" height="139" />

> Useful tricks for matrix
manipulation

<!-- badges: start -->

[![Documentation](https://img.shields.io/badge/documentation-matricks-orange.svg?colorB=E91E63)](http://krzjoa.github.io/matricks)
[![Travis build
status](https://travis-ci.org/krzjoa/matricks.svg?branch=master)](https://travis-ci.org/krzjoa/matricks)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/krzjoa/matricks?branch=master&svg=true)](https://ci.appveyor.com/project/krzjoa/matricks)
[![Buy hex
stciker](https://img.shields.io/badge/buy%20hex-matricks-green)](https://www.redbubble.com/people/krzjoa/works/43111073-matricks-r-package-hex?asc=u&kind=sticker&p=sticker&size=small)

<!-- badges: end -->

### Installation

``` r
devtools::install_github('krzjoa/matricks')
```

### Usage

Main `matricks` functions are `m` and `v`, which provide convenient API
to create matrices and vectors.  
Why should we write:

``` r
matrix(c(5, 6, 7,
         8, 0, 9,
         3, 7, 1), nrow = 3, byrow = TRUE)
#>      [,1] [,2] [,3]
#> [1,]    5    6    7
#> [2,]    8    0    9
#> [3,]    3    7    1
```

if we can simply create such a matrix like that:

``` r
library(matricks)

m(5, 6, 7|
  8, 0, 9|
  3, 7, 1)
#>      [,1] [,2] [,3]
#> [1,]    5    6    7
#> [2,]    8    0    9
#> [3,]    3    7    1
```

`v` function is an useful shortcut for creating vertical vectors (single
columns)

``` r
v(1,2,3)
#>      [,1]
#> [1,]    1
#> [2,]    2
#> [3,]    3
v(1:5)
#>      [,1]
#> [1,]    1
#> [2,]    2
#> [3,]    3
#> [4,]    4
#> [5,]    5
```

Setting values in easier with `matricks`

``` r
mat <- matrix(0, 3, 3)
set_values(mat, c(1, 2) ~ 0.5, c(3, 1) ~ 7)
#>      [,1] [,2] [,3]
#> [1,]    0  0.5    0
#> [2,]    0  0.0    0
#> [3,]    7  0.0    0
```
