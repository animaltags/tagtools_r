test_matrix1 <- matrix(c(7, 2, 3, 6, 4, 9,10,23,6), 
                       byrow = TRUE, ncol = 3)


test_output <- list(X = array(
  data = c(6, 4, 9), dim = c(1, 3, 1)),cues = c(1))

test_that("function works", {
  expect_equal(extract_cues(x = test_matrix1 , sampling_rate = 1, cues = c(1, 5), len = 1), 
               test_output)
})

test_matrix2 <- matrix(c(7, 2, 3, 6, "winter", 9,"10","summer",6), 
                       byrow = TRUE, ncol = 3)

test_output2 <- list(X = array(
  data = c("6", "winter", "9"), dim = c(1, 3, 1)),cues = c(1))


test_that("function works", {
  expect_equal(extract_cues(x = test_matrix2 , sampling_rate = 1, cues = c(1, 5), len = 1), 
               test_output2)
})

test_data3 <- c(7, 2, "Spring", 6, 4, 9,10,23,6)

test_that("function does not work", {
  expect_error(extract_cues(x = test_data3 , sampling_rate = 1, cues = c(1, 5), len = 1), 
               "argument is of length zero")
  
})