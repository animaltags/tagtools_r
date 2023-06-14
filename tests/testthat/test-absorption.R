f<-60
T<-20
d<-20

test_output <- 0.00000036238703

test_that("function works", {
  expect_equal(absorption(f,T,d), test_output)
})

f2 <-"60"
T2 <-20
d2 <-c(20,30)

 test_that("error reported", {
   expect_error(absorption(f2,T2,d2), "non-numeric argument to binary operator")
 })