
test_that("function works", { 
  expect_equal(sound_speed(18, 900, 25),1519.23415)
})

test_that("function works", {
  expect_equal(sound_speed(0),1449.06623)
})

test_that("function does not works", {
  expect_error(sound_speed(8, "1000", 34), "non-numeric argument to binary operator")
})