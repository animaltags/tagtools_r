depth <- c(10, 12, 15, 18, 20, 19, 16, 14, 12, 11,20,10)
sampling_rate <- 1  # Sampling rate of 1 Hz


test_output <- c(
  -2.34438762, -2.59343038, -2.88358978, -2.25602916,
  -0.32324049, 1.90824775, 2.64175825, 1.28966787,
  -0.36294179, -0.02150894, 2.10022712, 3.34610571
)


test_that("function works", {
  expect_equal(depth_rate(p = depth, fs = sampling_rate), test_output)
})


depth2 <- c(10, "12", 15, 18, 20, 19, 16, 14, 12, 11,20,10)

test_that("function does work", {
  expect_error(depth_rate(p = depth2, fs = sampling_rate), "non-numeric argument to binary operator")
})

