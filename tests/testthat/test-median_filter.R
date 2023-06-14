sample <- matrix(c(1, -3, 16, -4, -2, -1, 40, 25, 11), ncol = 1)
set <- matrix(c(1,-3,-2,-2,-1,-1,11,25,11))

test_that("function works", {
  expect_equal(median_filter(sample, n = 5),set)
})

sample2 <- matrix(c(list(7,8), 9, -1), ncol =1)
test_that("function does not works", {
  expect_error(median_filter(sample2, n = 2), "'data' must be of a vector type")
})