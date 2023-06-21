s <- matrix(sin( 2 * pi * 0.1 * c(1:50)), ncol=1)
A <- s %*% c(0.8, -0.64, 0.16) + s^2 %*% c(0, 0.2, 0.1)
sample <- list(
  V = c(-0.472866244, -0.394055203, 0.788110406),
  q = 4.199379e-16
)

test_that("function works", {
  expect_equal(inv_axis(A), sample)
})


test_that("function does not works", {
  expect_error(inv_axis(matrix(sin( 2 * pi * 0.1 * c(1:50)), ncol=1)), "subscript out of bounds")
})