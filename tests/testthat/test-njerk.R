sample <- matrix(c(1, -3, 5, -4, -2, -1, 4, 2, 1),
                 byrow = TRUE, nrow = 3, ncol = 3
)
set <- c(31.496031, 36.660606, 0.00000)

test_that("function works", {
  expect_equal(njerk(A = sample, sampling_rate=4),set)
})

sample2 <- matrix(c("hea", 9, -1))
test_that("function does not works", {
  expect_error(njerk(A = sample2, sampling_rate=4), "non-numeric argument to binary operator")
})