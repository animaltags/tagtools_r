first_five_rows <- beaked_whale$A$data[1:5, ]


# Create the lowpass matrix
lowpass <- matrix(c(-0.09049187810,-0.07072657779,0.9959465170,
                      0.18937340183,-0.02137228053,1.0204908047,
                      0.47347142451, 0.10272558074,0.9025616669,
                     0.35912600715, 0.07394708043,0.8713049182,
                     -0.07765997054,-0.08183871836,0.9550576677),
                  nrow = 5, ncol = 3, byrow = TRUE)

# Create the highpass matrix
highpass <- matrix(c(-5.919447227e-05,  4.350911750e-05, -1.689597370e-04,
                     1.094917461e-04, -1.130080528e-05,  5.706795706e-05,
                     -1.685258273e-04, -2.767650046e-05, -1.458495301e-05,
                     2.275599084e-04,  6.665380620e-05, -2.789805105e-05,
                     -2.717682895e-04, -9.246074494e-05,  6.516502957e-05),
                   nrow = 5, ncol = 3, byrow = TRUE)

# Create the list
test_output <- list(lowpass = lowpass, highpass = highpass)


test_that("function works", {
  expect_equal(comp_filt(
    X=first_five_rows,sampling_rate = 1 ,fc=0.5), test_output)
})

test_data <- matrix(c(2,3,1,4,5,6,1,4,7,2,3,5,2,"3",5),ncol = 3, byrow = TRUE)

test_that("function works", {
  expect_error(comp_filt(
    X=test_data,sampling_rate = 1 ,fc=0.5), "non-numeric argument to binary operator")
})

