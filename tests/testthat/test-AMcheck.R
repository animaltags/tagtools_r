fstr <- matrix(c(1.00019998 ,34.11744422), nrow = 1)

incl <- matrix(0.20181096)

fs <- 1

A <- matrix(c(-0.3, 0.52, 0.8), nrow = 1)
M <- matrix(c(22, -22, 14), nrow = 1)

test_output <- list(fstr = fstr, incl = incl)


test_that("function works", {
  expect_equal(check_AM(
  A,M,fs), test_output)
})

A2 <- matrix(c(-0.3, "0.52", 0.8), nrow = 1)

test_that("error reported", {
  expect_error(check_AM(
    A2,M,fs), "non-numeric argument to binary operator")
})