X <- 0.12 * matrix(c(25, -60, 33), ncol = 3)
Q <- euler2rotmat(X[, 1], X[, 2], X[, 3])
sample <- matrix(c(0.165428476209661, 0.876920163943781, -0.323951300858265),
                 nrow = 1, ncol = 3)

test_that("function works", {
  expect_equal(rotate_vecs(c(0.7, -0.63, -0.12), Q), sample)
})

test_that("function does not works", {
  expect_error(rotate_vecs(c(0.7, -0.63, -0.12)), "inputs for all arguments are required")
})  
