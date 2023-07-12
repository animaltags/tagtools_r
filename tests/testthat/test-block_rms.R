test_matrix <- matrix(c(1:20), 
                      byrow = TRUE, nrow = 4)


Y <- matrix(c(4.30116263  , 5.14781507  , 6.04152299  , 6.96419414  , 7.90569415  ,
              13.72953022, 14.71393897 , 15.70031847 , 16.68831927 , 17.67766953 ),
            nrow = 2, byrow = TRUE)
samples <- c(1,3)

test_output <- list(Y = Y, samples = samples)

test_that("function works", {
  expect_equal(block_rms(test_matrix, n = 2, nov = NULL), test_output)
})


test_matrix_2 <- matrix(c(1, 3, 5, 7, 9, 13, 15, 17))

test_that("function does not work", {
  expect_error(block_mean(test_matrix_2, n=3, nov = 1), "subscript out of bounds")
})

