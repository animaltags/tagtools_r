s <- matrix(sin(2 * pi / 100 * c(0:50) - 1), ncol = 1)


test_output <- matrix(c(
  -0.8414709848079,
  -0.6805957165555,
  -0.4773424576588,
  -0.2431884074331,
  0.0048736773327,
  0.2531428775163,
  0.4852563013765,
  0.6868024616145,
  0.8454300012925,
  0.9508320021875,
  0.9964287704232,
  0.9812071897936), nrow = 12, ncol = 1)


test_that("function does work", {
  expect_equal(decdc(x = s, df = 4), test_output)
})

s2 <- matrix(c("spring", "1", "Summer"), nrow = 3)

test_that("function does not work", {
  expect_error(decdc(x = s2, df = 1), "non-numeric argument to binary operator")
})
