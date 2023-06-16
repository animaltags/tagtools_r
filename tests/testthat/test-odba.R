BW <- beaked_whale
set <- c(0.25683511, 0.08996086, 0.37610830, 0.30352436, 0.13311639, 0.35697760,
          0.25880527, 0.15208414, 0.11181959, 0.06985325, 0.09175993, 0.26785429,
          0.47422222, 0.30028548, 0.10675728, 0.24103940, 0.19952115, 0.23902250)

test_that("function works", {
  expect_equal(odba(A = BW$A$data, sampling_rate = BW$A$sampling_rate, fh = 0.075)[1:18], set)
})


test_that("function does not works", {
  expect_error(odba(A = BW$A$data, sampling_rate = BW$A$sampling_rate, fh = "0.1"), "non-numeric argument to binary operator")
})