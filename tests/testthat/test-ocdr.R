HS <- harbor_seal
set <- c(0.023108285, 0.022274135, 0.019812952,
         0.015853774, 0.010609605, 0.004345634,
         -0.002626563, -0.009949951, -0.017259067,
         -0.024203017, -0.030455932, -0.035731832,
         -0.039794096, -0.042491366, -0.043784747,
         -0.043722605, -0.042408117, -0.039979023)

test_that("function works", {
  expect_equal(ocdr(p = HS$P$data, A = HS$A$data, sampling_rate = HS$P$sampling_rate)[1:18], set)
})


test_that("function does not works", {
  expect_error(ocdr(p = HS$P$data, A = HS$A$data, sampling_rate = "5"), "non-numeric argument to binary operator")
})