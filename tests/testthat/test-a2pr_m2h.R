
test_matrix <- matrix(c(1, -0.2, 0.3, -0.3, 0.2,-0.1, 0.23, 0.17, 0.19), byrow = TRUE, nrow = 3)

test_matrix_2 <- matrix(c(0.7887, "scary", cos(-0.292), str(-0.395), 0.432,sin(-0.43),byrow = TRUE, nrow = 2))

p <- c(1.22474940, -0.93027401, 0.73399298)
r <- c(-0.588002604, 2.034443936, 0.729899658)

test <- data.frame(p,r)
test <- as.list(test)


test_that("function works", {
  expect_equal(a2pr(test_matrix), test)
})



test_that("function does not works", {
  expect_error(a2pr(test_matrix_2), "non-numeric argument to binary operator")
})


# m2h

test_matrix_3 <- matrix(c(18,29,39), nrow = 1)
test_matrix_4 <- matrix(c(-0.61,0.32,0.12), nrow = 1)
h <- 0.5350092
v<- 51.826634
incl <- -0.082327314
testr <- data.frame(h,v, incl)
testr <- as.list(testr)

test_that("function works", {
  expect_equal(m2h(test_matrix_3,test_matrix_4), testr)
})

test_matrix_5 <- matrix(c("stat", sin(32),39), nrow = 1)
test_matrix_6 <- matrix(c(-0.61,1,0.12), nrow = 1)

test_that("function works", {
  expect_error(m2h(test_matrix_5,test_matrix_6), "non-numeric argument to binary operator")
})






