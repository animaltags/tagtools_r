test_data <- sin(100) + 30

test_output <- 9.7928547

test_that("function works", {
  expect_equal(acc_wgs84(test_data), test_output)
})

test_data_2 <- "20"

test_that("error reported", {
  expect_error(acc_wgs84(test_data_2), "non-numeric argument to binary operator")
})
