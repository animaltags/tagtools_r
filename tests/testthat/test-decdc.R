s <- matrix(sin(2 * pi / 100 * c(0:50) - 1), ncol = 1)


test_output <- matrix(c(
  -0.841890199,
  -0.680934784,
  -0.477580266,
  -0.243309562,
  0.004876105,
  0.253268991,
  0.485498052,
  0.687144621,
  0.845851188,
  0.951305699,
  0.996925184,
  0.981696020
), nrow = 12, ncol = 1)


test_that("function does work", {
  expect_equal(decdc(x = s, df = 4), test_output)
})

s2 <- matrix(c("spring", "1", "Summer"), nrow = 3)

test_that("function does not work", {
  expect_error(decdc(x = s2, df = 1), "non-numeric argument to binary operator")
})
