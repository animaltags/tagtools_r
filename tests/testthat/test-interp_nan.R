Q <- matrix(c(NA, NA, 13, 42, 59, 6, 37, 81, 9, 14, NA, NA, 17, 104, 51, 167, NA, NA), ncol = 2)
sample <- matrix(c(13, 13, 13, 42, 59, 6, 37, 81, 9, 14, 15, 16, 17, 104, 51, 167, 167, 167), ncol=2)

test_that("function works", {
  expect_equal(interp_nan(Q)$data, sample)
})

P <- matrix(c(40,"h",20))
test_that("function does not works", {
  expect_warning(interp_nan(P), "NAs introduced by coercion")
})

