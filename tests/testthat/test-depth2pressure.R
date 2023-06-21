test_output <- 3404031.91


test_that("function works", {
  expect_equal(depth2pressure(338.8, 29.269136), test_output)
})


test_that("function does not work", {
  expect_error(depth2pressure("338.8", 29.269136), "non-numeric argument to binary operator")
})