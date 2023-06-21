set.seed(123)

sample <- c(1435.9334,
            1439.5637,
            1441.3847,
            1442.0992,
            1443.1716,
            1443.5215,
            1447.0746,
            1448.4953,
            1448.6363,
            1449.3570)

test_that("function works", {
  expect_equal(round(rotate_data(15*stats::runif(10), full_period = c(0, 1500)), digits=4), sample)
})


test_that("function does not works", {
  expect_error(rotate_data(15*stats::runif(25)), "event_times and full_period are required inputs.")
})