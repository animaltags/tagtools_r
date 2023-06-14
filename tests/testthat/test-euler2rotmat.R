m1 <- matrix(c(1:2), nrow = 2)
m2 <- matrix(c(3:4), nrow = 2)
m3 <- matrix(c(5:6), nrow = 2)


# Create the first matrix
matrix1 <- array(c(0.1532633329, -0.5181089968, 0.8414709848,
                   -0.98301226516, -0.16695271936, 0.07624746576,
                   0.1009813712, -0.8388622396, -0.5348952287), dim = c(3, 3, 1))

# Create the second matrix
matrix2 <- array(c(-0.3995718273, 0.1162778757, 0.9092974268,
                   0.4781112454, -0.8198913501, 0.3149409643,
                   0.7821457612, 0.5605868618, 0.2720117251), dim = c(3, 3, 1))

# Create the 3-dimensional array
test_output <- array(c(matrix1, matrix2), dim = c(3, 3, 2))


test_that("function works", {
  expect_equal(euler2rotmat(p = m1, r = m2, h = m3), test_output)
})

m4 <- matrix(c("Spring",2), nrow = 2)

test_that("function does not work", {
  expect_error(euler2rotmat(p = m4, r = m2, h = m3), "non-numeric argument to mathematical function")
})
