test_matrix <- matrix(c(1, 3, 5, 7, 9, 11, 13, 15, 17), 
                    byrow = TRUE, ncol = 3)
n <- 3
nov <- 1

Y <- matrix(c(7,9,11),byrow = FALSE ,ncol = 3)
t <- matrix(c(2))

test_output <- list(Y = Y, t = t)

test_that("function works", {
  expect_equal(block_mean(test_matrix, n = n, nov = nov), test_output)
})


test_matrix_2 <- matrix("summer",2)



test_that("function warning", {
  expect_error(block_mean(test_matrix_2, n=3, nov = 1), "subscript out of bounds")
})

