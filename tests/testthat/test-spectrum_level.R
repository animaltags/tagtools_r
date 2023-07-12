BW <- beaked_whale

sample<- BW$P$data %>% "["(.,1:9,)
set <- list(
  SL = matrix(c(-24.56609273, -20.52578992), nrow = 2, ncol = 1),
  freq = c(0.00, 0.25)
)

test_that("function works", {
  expect_equal(spectrum_level(x = sample, nfft = 4, sampling_rate = BW$P$sampling_rate),set)
})

test_that("function does not works", {
  expect_error(spectrum_level(x = sample, nfft = "4", sampling_rate = BW$P$sampling_rate), "non-numeric argument to binary operator")
})