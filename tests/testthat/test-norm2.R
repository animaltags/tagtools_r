sample <- matrix(c(1, -0.9, 0.16, 0.85, -2, 0.26, 0.53, -0.79, -0.743),
                 byrow = TRUE, nrow = 3, ncol = 3
)
set <- c(1.354843164, 2.188629708, 1.207082847)

test_that("function works", {
  expect_equal(norm2(sample),set)
})

sample2 <- matrix(c("hea", 1.9, -0.31))
test_that("function does not works", {
  expect_error(norm2(sample2), "non-numeric argument to binary operator")
})