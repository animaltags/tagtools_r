test_matrix1 <- matrix(c(7, 2, 3, 6, 4, 9,10,23,6), 
                       byrow = TRUE, ncol = 3)


test_output <- c(6,4,9)


test_that("function works", {
  expect_equal(extract(x = test_matrix1 , sampling_rate = 1, tst = 1, ted = 2), 
               test_output)
})

test_matrix2 <- matrix(c(7, 2, 3, 6, "winter", 9,"10","summer",6), 
                                               byrow = TRUE, ncol = 3)

test_output2 <- c("6","winter","9")

test_that("function works", {
  expect_equal(extract(x = test_matrix2 , sampling_rate = 1, tst = 1, ted = 2), 
               test_output2)
})


test_data3 <- list(7, 2, "Spring", 6, 4, 9,10,23,6)

test_output3 <- list(2)


test_that("function does not work", {
  expect_equal(extract(x = test_data3 , sampling_rate = 1, tst = 1, ted = 2), 
               test_output3)
  
})