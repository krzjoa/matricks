test_that(
  "Extract antidiagonal",{
    mat <- diag(1, 3, 3)
    vec1 <- antidiag(mat)
    vec2 <- c(0, 1, 0)
    expect_equal(vec1, vec2)
})

test_that(
  "Create antidiagonal matrix",{
    mat1 <- antidiag(7, 3, 3)
    mat2 <- matrix(0, 3, 3)
    mat2[1, 3] <- 7
    mat2[2, 2] <- 7
    mat2[3, 1] <- 7
    expect_equal(mat1, mat2)
})

test_that(
  "Create antidiagonal matrix from vector v1",{
    mat1 <- antidiag(1:3, 3, 3)
    mat2 <- matrix(0, 3, 3)
    mat2[1, 3] <- 1
    mat2[2, 2] <- 2
    mat2[3, 1] <- 3
    expect_equal(mat1, mat2)
})

test_that(
  "Create antidiagonal matrix from vector v2",{
    mat1 <- antidiag(1:2, 3, 3)
    mat2 <- matrix(0, 3, 3)
    mat2[1, 3] <- 1
    mat2[2, 2] <- 2
    mat2[3, 1] <- 1
    expect_equal(mat1, mat2)
})

test_that(
  "Create antidiagonal matrix from vector v3",{
    mat1 <- antidiag(1:5, 3, 3)
    mat2 <- matrix(0, 3, 3)
    mat2[1, 3] <- 1
    mat2[2, 2] <- 2
    mat2[3, 1] <- 3
    expect_equal(mat1, mat2)
})



