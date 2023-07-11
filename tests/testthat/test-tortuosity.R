BW <- beaked_whale
track <- ptrack(
  A = BW$A$data, M = BW$M$data, s = 3,
  sampling_rate = BW$A$sampling_rate,
  fc = NULL
)

sample <-
  c(0.009081966995, 0.012106058461, 0.003142875711, 0.005290762764, 0.008281926790, 0.003518177425)

test_that("function works", {
  expect_equal(tortuosity(track, sampling_rate = BW$A$sampling_rate, intvl = 12)[1:6,], sample)
})

test_that("function does not works", {
  expect_error(tortuosity(track, sampling_rate = BW$A$sampling_rate, intvl = "12"), "non-numeric argument to binary operator")
})