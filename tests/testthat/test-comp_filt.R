first_five_rows <- beaked_whale$A$data[1:5, ]


# Create the lowpass matrix
lowpass <- matrix(c( -0.090491878104, -0.070726577794, 0.99594651698,
                     0.189373401831, -0.021372280531, 1.02049080467,
                     0.473471424511,  0.102725580744, 0.90256166691,
                     0.359126007146,  0.073947080429, 0.87130491818,
                     -0.077659970538, -0.081838718361, 0.95505766770),
                  nrow = 5, ncol = 3, byrow = TRUE)

# Create the highpass matrix
highpass <- matrix(c(-5.9194472274e-05,  4.3509117499e-05, -1.6895973695e-04,
                     1.0949174615e-04, -1.1300805279e-05,  5.7067957061e-05,
                     -1.6852582727e-04, -2.7676500459e-05, -1.4584953007e-05,
                     2.2755990840e-04,  6.6653806196e-05, -2.7898051047e-05,
                     -2.7176828948e-04, -9.2460744941e-05,  6.5165029568e-05),
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
    X=test_data,sampling_rate = 1 ,fc=0.5), "x must be a numeric or complex vector or matrix")
})

