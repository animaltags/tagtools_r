sample <- data.frame(
  northing = c(-15270043.7, -9882479.4),
  easting = c(2015179.07, 2332425.22)
)

coordinates <- matrix(c(
  -122.4194, 37.7749,
  -73.9352,  40.7306
), nrow = 2, ncol = 2, byrow = TRUE)
  
test_that("function works", {
    expect_equal(lalo2llf(coordinates, c(15,19)), sample)
})


test_that("function does not works", {
  expect_error(lalo2llf(matrix(c(4, 3, 8, "dance"), ncol=2)), "non-numeric argument to binary operator")
})  
  
test_that("function does not works", {
  expect_error(lalo2llf(c(102.15,78.5), c(85.75, 125.8)), "non-numeric matrix extent")
})  
