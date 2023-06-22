BW <- beaked_whale
set <- BW$A$data[1:8,]
#set <- array(set, dim = c(8, 3))


sample <- list(
  residual = "0.0 ", axial_balance ="0.0",
  poly = matrix(c(1, -40618630, 1, 131030375, 1, 53257608), nrow = 3, ncol=2, byrow = TRUE),
               cross = matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, ncol=3, byrow = TRUE))

test_that("function works", {
  expect_equal(spherical_cal(array(set, dim = c(8, 3)))$G$poly, sample$poly)
})


test_that("function does not works", {
  expect_error(spherical_cal(matrix(c(17,12,19))), "non-conformable arrays")
})

test_that("function does not works", {
  expect_error(spherical_cal(c(17,12,19)), "'x' must be an array of at least two dimensions")
})
