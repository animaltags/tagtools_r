test_that("function works", {
  expect_equal(mean_absorption(c(48e5,28e9), 500, c(4, 94)), matrix(c(3216.7275)))
})


test_that("function does not works", {
  expect_error(mean_absorption(c(48e5,28e9), "head", c(4, 94)), "non-numeric argument to binary operator")
})
