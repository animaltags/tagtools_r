x <- c(1:10)
n <- 3
overlap <- 2
opt <- c(2, 1)



X <- matrix(c(2, 1, 1, 2, 3, 4, 5, 6, 7, 8,
              1, 1, 2, 3, 4, 5, 6, 7, 8, 9,
              1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
            nrow = 3, byrow = TRUE)

z <- NULL

opt2 <- c(9, 10)

test_output <- list(X = X, z = z, opt = opt2)


test_that("function works", {
  expect_equal(buffer(x, n, overlap, opt), test_output)
})


x2 <- c("1", "cat")

X2 <- matrix(c("2", "1", "1", "1", "1", "cat"),
            nrow = 3, byrow = TRUE)

opt3 <- c("1", "cat")


test_output2 <- list(X = X2, z = z, opt = opt3)


test_that("function does works", {
  expect_equal(buffer(x2, n, overlap, opt), test_output2)
})