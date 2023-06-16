harbor <- harbor_seal
BW <- beaked_whale
s <- c(-0.023108285, -0.022274135, -0.019812952, -0.015853774, -0.010609605,
       -0.004345634, 0.002626563, 0.009949951, 0.017259067, 0.024203017,
       0.030455932, 0.035731832, 0.039794096, 0.042491366, 0.043784747,
       0.043722605, 0.042408117, 0.039979023)

test_that("function works", {
  expect_equal(speed_from_depth(harbor_seal$P, harbor_seal$A)[1:18], s)
})

test_that("function does not works", {
  expect_error(speed_from_depth(12, 15), "input fs_p is required for speed_from_depth()")
})