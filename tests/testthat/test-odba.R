BW <- beaked_whale
set <- c(0.256803442324,
         0.089949766943,
         0.376061931801,
         0.303486936398,
         0.133099975615,
         0.356933592808,
         0.258773358039,
         0.152065384981,
         0.111805805132,
         0.069844635346,
         0.091748620760,
         0.267821268753,
         0.474163752018,
         0.300248460797,
         0.106744116703,
         0.241009678723,
         0.199496547273,
         0.238993026852)

test_that("function works", {
  expect_equal(odba(A = BW$A$data, sampling_rate = BW$A$sampling_rate, fh = 0.075)[1:18], set)
})


test_that("function does not work", {
  expect_error(odba(A = BW$A$data, sampling_rate = BW$A$sampling_rate, fh = "0.1"), "non-numeric argument to binary operator")
})
