test_x <- sin(t(2 * pi * 0.05 * (1:10)) +
           t(cos(2 * pi * 0.25 * (1:10))))


test_output <- c(0.247064751 , 0.331019693 , 0.533117290 , 0.732628678 ,
                 0.815504980 , 0.745549177 , 0.593048336 , 0.459056015 , 0.389992934 , 0.371411653)

test_that("function works", {
  expect_equal(fir_nodelay(x = test_x, n = 10, fc = 0.2, qual = "low"), test_output)
})

test_matrix <- matrix(seq(0, 20, 
                          length.out = 5), nrow = 10, ncol = 1)

test_output2 <- c(7.1331338  ,8.2089982  ,10.4860429 ,12.0358811 ,11.8728145 ,10.8354775 ,
                  10.6724109 ,12.2222491 ,14.4992938 ,15.5751582 )

test_that("function works", {
  expect_equal(fir_nodelay(x = test_matrix, n = 10, fc = 0.2, qual = "low"), test_output2)
})

test_data <- matrix(2, 4)

test_that("function does not work", {
  expect_error(fir_nodelay(x = test_data, n = 10, fc = 0.2, qual = "low"), 
               "subscript out of bounds")
})
