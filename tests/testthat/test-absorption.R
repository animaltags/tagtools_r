freq1<-60
temp1<-20
d<-20

test_output <- 0.00000036238703

test_that("function works", {
  expect_equal(absorption(freq1,temp1,d), test_output)
})

freq2 <-"60"
temp2 <-20
d2 <-c(20,30)

 test_that("error reported", {
   expect_error(absorption(freq2,temp2,d2), "non-numeric argument to binary operator")
 })