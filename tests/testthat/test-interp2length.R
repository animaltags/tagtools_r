HS <- harbor_seal
P <- decdc(HS$P, 5)
sample <- c(23.5016995, 23.4982005, 23.4947014, 23.4912023, 23.4877033, 23.4842042, 23.4908630, 23.4975218, 23.5041806, 23.5108394)

test_that("function works", {
  expect_equal(interp2length(X = P, Z = HS$A)$data[1:10], sample)
})

S <- c(15,19,21,28)
test_that("function does not works", {
  expect_error(interp2length(X = S, Z = HS$A), "Input fs_in is required if X is not a sensor data list.")
})

test_that("function does not works", {
  expect_error(interp2length(Z = HS$A), "Inputs X and Z are required for interp2length().")
})