vec <- sin(2 * pi * 0.033 * c(1:100))
vec2 <-cos(c(1:20))

set <-  list(
  K = c(15.142776, 30.285834, 45.429458, 60.628078, 75.771442, 90.914353),
  s = c(-1, 1, -1, 1, -1, 1),
  KK = matrix(c(13, 17,
                28, 32,
                43, 47,
                59, 63,
                74, 78,
                89, 93), nrow = 6, ncol = 2, byrow = TRUE)
)


test_that("function works", {
  expect_equal(zero_crossings(vec, 0.3), set)
})


test_that("function does not works", {
  expect_error(zero_crossings(vec, "stay"), "invalid argument to unary operator")
})
