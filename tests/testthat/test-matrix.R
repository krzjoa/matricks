test_that(
  "Create vector",{
  vec1 <- v(1,2,3,4)
  vec2 <- matrix(data = 1:4, ncol = 1)
  expect_equal(vec1, vec2)
})

test_that(
  "Create vector from a sequence",{
    vec1 <- v(1:4)
    vec2 <- matrix(data = 1:4, ncol = 1)
    expect_equal(vec1, vec2)
})

test_that(
  "col_bind: enumerating args vs passing whole vector", {
   expect_equal(col_bind(1,2,3,4,5),
                col_bind(1:5))
})

test_that(
  "col_bind: bind vector with a matrix", {
  mat1 <- cbind(cbind(diag(3), 1), 2)
  mat2 <- col_bind(diag(3), 1:2)
  expect_equal(mat1, mat2)
})
# col_bind(2:5, 1, 2, 3, diag(3))

test_that("m: the simpliest case", {
  mat1 <- m(1, 2, 3 | 4, 5, 6 | 7, 8, 9)
  mat2 <- t(matrix(1:9, ncol = 3, nrow = 3))
  expect_equal(mat1, mat2)
})

test_that("m: nested operation", {
  mat1 <- m(2, 3, 4 | 5, 6, 7 | 8, 9, (9 + 1))
  mat2 <- t(matrix((1:9+1), ncol = 3, nrow = 3))
  expect_equal(mat1, mat2)
})

test_that("m: throw error when number of columns differ", {
  expect_error(m(1:10 | 2:9))
})

test_that("m: nested '|' symbol v1", {
   mat1 <- m(1, 2, NA | 4, NA, 6 | (1 | 2), 8, 9)
   mat2 <- rbind(cbind(1, 2, NA),
                 cbind(4, NA, 6),
                 cbind(1, 8, 9))
   expect_equal(mat1, mat2)
})

test_that("m: nested '|' symbol v2", {
  mat1 <- m(1, 2, NA | 4, NA, (6 | 9) | 7, 8, 9)
  mat2 <- rbind(cbind(1, 2, NA),
                cbind(4, NA, 1),
                cbind(7, 8, 9))
  expect_equal(mat1, mat2)
})

test_that("m: binding vectors", {
  mat1 <- m( 1:3 | 2:4 )
  mat2 <- rbind(1:3, 2:4)
  expect_equal(mat1, mat2)
})

test_that("m: binding matrices", {
  mat1 <- diag(3)
  mat2 <- mat1 * 3
  mat3 <- m(mat1, mat2|
            mat2, mat1)
  mat4 <- rbind(cbind(mat1, mat2),
                cbind(mat2, mat1))
  expect_equal(mat3, mat4)
})
