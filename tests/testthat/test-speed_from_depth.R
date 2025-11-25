harbor <- harbor_seal
BW <- beaked_whale
s <- c(-0.0231855626563,
       -0.0223486225764, -0.0198792096725, -0.0159067910049, -0.0106450846615,
       -0.0043601661302, 0.0026353470284,  0.0099832250281,  0.0173167840341,
       0.0242839554090,  0.0305577814774,  0.0358513247691,
       0.0399271731834,  0.0426334627738,  0.0439311699277,  0.0438688196175,
       0.0425499360104,  0.0401127184618)

test_that("function works", {
  expect_equal(speed_from_depth(harbor_seal$P, harbor_seal$A)[1:18], s)
})

test_that("function does not work", {
  expect_error(speed_from_depth(12, 15), "input fs_p is required for speed_from_depth()")
})
