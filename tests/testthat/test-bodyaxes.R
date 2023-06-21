test_matrix1 <- matrix(c(7, 2, 3, 6, 4, 9), 
                       byrow = TRUE, ncol = 3)
test_matrix2 <- matrix(c(6, 5, 3, 4, 8, 9), 
                       byrow = TRUE, ncol = 3)
x <- matrix(c(-0.2807511347 , -0.4890366998 ,-0.3617405420 , -0.7001188574 , 0.8890008890  ,0.5202659817),
            nrow = 3, byrow = TRUE)

y <- matrix(c(0.95965842  , 0.87014806  , -0.12058018 , -0.35005943 , 0.25400025  , 0.34684399),
            nrow = 3, byrow = TRUE)

z <- matrix(c(0.015313698  , -0.060708004 , 0.924448052  , 0.6223279, 0.381000381  , 0.780398973),
            nrow = 3, byrow = TRUE)

sampling_rate <- NULL

test_output <- list(x = x, y = y, z = z, sampling_rate = sampling_rate)


test_that("function works", {
  expect_equal(body_axes(test_matrix1,test_matrix2, fc = NULL), test_output)
})

test_matrix3 <- matrix(c(7, 2, 3, 6, 4, "spring"), 
                       byrow = TRUE, ncol = 3)
test_matrix4 <- matrix(c(6, 5, 3, 4, "2", 9), 
                       byrow = TRUE, ncol = 3)

test_that("function does not work", {
  expect_error(body_axes(test_matrix3,test_matrix4, fc = NULL), "non-numeric argument to binary operator")
})