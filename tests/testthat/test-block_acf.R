resids <- c(10,20,30,20,10,0)
blocks <- c(1,2,3,3,2,1)

test_output <- data.frame(ACF = c(1.00000000,0.43636364,-0.09090909))

num <-block_acf(resids = resids, blocks = blocks)

test_that("function works", {
  expect_equal(num, test_output)
})


resids_2 <- c(10,"20",30,"word",10,sin(3))
blocks_2 <- c(1,2,3,3,2,1)



test_that("error reported", {
  expect_error(block_acf(resids = resids_2, blocks = blocks_2), "'x' must be numeric")
})
