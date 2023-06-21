s <- matrix(1* c(1:100), ncol = 1) *
  matrix(1* c(1:100), ncol = 1)

test_output <- c(8320.31159, 8497.96999  , 8682.97083  , 8855.68243  , 
                 9044.67815  , 9219.08578  , 9412.07867  , 9615.56546  , 
                 9812.60610, 10379.48748 )


test_that("function works", {
  expect_equal(hilbert_env(s, N= 20), test_output)
})

test_data <- matrix(c(1,4,5,6,"summer",1,3,6), ncol = 1)

test_that("function does not work", {
  expect_error(hilbert_env(test_data, N= 2), "non-numeric argument")
})

s2 <-vector("numeric",100)
s2 <- 1:100

test_output2 <- c(200)

test_that("function works", {
  expect_equal(hilbert_env(s2, N= 2), test_output2)
})