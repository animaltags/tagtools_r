test_matrix <- matrix(1,2)

test_that("function does not work", {
  expect_error(find_dives(p = test_matrix, 
                          sampling_rate = 2, 
                          mindepth = 5, surface = 2, 
                          findall = FALSE), "subscript out of bounds")
})

BW <- beaked_whale

test_output <- data.frame(
  start = c(178, 3987, 4243),
  end = c(3920, 4039, 5293),
  max = c(1086.99275, 6.21215, 238.84884),
  tmax = c(1400, 4012, 4692)
)

test_that("function works", {
  expect_equal(find_dives(p = BW$P$data, 
                          sampling_rate = BW$P$sampling_rate, 
                          mindepth = 5, surface = 2, 
                          findall = FALSE), test_output)
})
