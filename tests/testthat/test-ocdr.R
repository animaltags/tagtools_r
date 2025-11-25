HS <- harbor_seal
set <- c(0.0231855626563,
         0.0223486225764,
         0.0198792096725,
         0.0159067910049,
         0.0106450846615,
         0.0043601661302,
         -0.0026353470284,
         -0.0099832250281,
         -0.0173167840341,
         -0.0242839554090,
         -0.0305577814774,
         -0.0358513247691,
         -0.0399271731834,
         -0.0426334627738,
         -0.0439311699277,
         -0.0438688196175,
         -0.0425499360104,
         -0.0401127184618)

test_that("function works", {
  expect_equal(ocdr(p = HS$P$data, A = HS$A$data, sampling_rate = HS$P$sampling_rate)[1:18], set)
})


test_that("function does not work", {
  expect_error(ocdr(p = HS$P$data, A = HS$A$data, sampling_rate = "5"), "non-numeric argument to binary operator")
})
