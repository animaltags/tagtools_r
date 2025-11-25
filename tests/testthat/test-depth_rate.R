depth <- c(10, 12, 15, 18, 20, 19, 16, 14, 12, 11,20,10)
sampling_rate <- 1  # Sampling rate of 1 Hz


test_output <- c(-2.35236377286,
                 -2.60225382945,
                 -2.89340042192,
                 -2.26370468769,
                 -0.32434022587,
                 1.91474004526,
                 2.65074612309,
                 1.29405561964,
                 -0.36417659865,
                 -0.02158211574,
                 2.10737257402,
                 3.35748993596)


test_that("function works", {
  expect_equal(depth_rate(p = depth, fs = sampling_rate), test_output)
})


depth2 <- c(10, "12", 15, 18, 20, 19, 16, 14, 12, 11,20,10)

test_that("function does not work", {
  expect_error(depth_rate(p = depth2, fs = sampling_rate), "non-numeric argument to binary operator")
})

