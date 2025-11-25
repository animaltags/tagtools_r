HS <- harbor_seal
P <- decdc(HS$P, 5)
sample <- c(23.492908885,
            23.489411137,
            23.485913388,
            23.482415640,
            23.478917892,
            23.475420144,
            23.482076458,
            23.488732772,
            23.495389086,
            23.502045400)

test_that("function works", {
  expect_equal(interp2length(X = P, Z = HS$A)$data[1:10], sample)
})

S <- c(15,19,21,28)
test_that("function does not work", {
  expect_error(interp2length(X = S, Z = HS$A), "Input fs_in is required if X is not a sensor data list.")
})

test_that("function does not work", {
  expect_error(interp2length(Z = HS$A), "Inputs X and Z are required for interp2length().")
})
