BW <- beaked_whale
sample<- BW$A$data %>% "["(.,1:9,)

set <- data.frame(
  seconds = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  dist = c(0.94391354, 2.15042492, 1.76236287, 1.77263865, 0.45172264, 2.16855383, 1.28969396, 1.54615165, 1.80995989)
)

test_that("function works", {
  expect_equal(m_dist(sample, BW$A$sampling_rate), set)
})


test_that("function does not works", {
  expect_error(m_dist(sample, 16), "'x' must be an array of at least two dimensions")
})