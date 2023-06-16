
test_that("function works", {
  expect_equal(julian_day(y=2015, d=28, m=11), 332)
})

test_that("function works", {
  expect_equal(julian_day(y=2015, 332), as.Date("2015-11-28"))
})

test_that("function does not works", {
  expect_error(julian_day(y=2013, m="12th"), "non-numeric argument to binary operator")
})  

