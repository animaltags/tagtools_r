X <- 0.12 * matrix(c(25, -60, 33), ncol = 3)
euler2rotmat(X[, 1], X[, 2], X[, 3])
set <- matrix(c(0.6765458, 0.7227523, 0.1411200,
                0.3675912, -0.4975063, 0.7857252,
                0.6380928, -0.4797047, -0.6022632),
              nrow = 3, ncol = 3)

sample <- c(0.14159265, 2.22477795, 0.81840737)

test_that("function works", {
  expect_equal(rotmat2euler(set), sample)
})

sample2 <- matrix(c(1, 2, 3,
                   4, "5", 6,
                   7, 8, 9),
                 nrow = 3, ncol = 3)

test_that("function does not works", {
  expect_error(rotmat2euler(sample2), "non-numeric argument to mathematical function")
})  