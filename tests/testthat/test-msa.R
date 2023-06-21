sample <- matrix(c(1, -0.9, 0.16, 0.85, -2, 0.26, 0.53, -0.79, -0.743),
                       byrow = TRUE, nrow = 3, ncol = 3
)
 set <- c(0.354843164, 1.188629708, 0.207082847)

test_that("function works", {
  expect_equal(msa(A = sample, ref=1),set)
})

sample2 <- matrix(c("hea", 0.94, -1.34))
test_that("function does not works", {
  expect_error(msa(A = sample2, ref=1), "A must be an acceleration matrix")
})