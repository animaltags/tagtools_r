
set.seed(123)
sample <- data.frame(
  statistic = 58.856975,
  CI_low = 58.161215,
  CI_up = 61.804645,
  n_rot = 10000,
  conf_level = 0.95,
  p_value = 0.4119588
)

test_that("function works", {
  expect_equal(rotation_test(event_times = 110 * runif(590),
  exp_period = c(10, 298),return_rot_stats = TRUE, ts_fun = mean)$result, sample)
})

test_that("function does not works", {
  expect_error(rotation_test(event_times = 110 * runif(590),return_rot_stats = TRUE, ts_fun = mean), "event_times and exp_period are required inputs.")
})