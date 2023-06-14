BW <- beaked_whale
sample <- data.frame(northing = c(-1.406682199, -2.844875721, -4.355947837, -5.953254013,
                                  -7.634714636, -9.372674268, -11.165807179, -13.013175415),
                     easting = c(1.421705029, 2.811524944, 4.121739071, 5.325322462,
                                 6.408230664, 7.397925728, 8.283744199, 9.050053927))

test_that("function works", {
  expect_equal(htrack(A = beaked_whale$A, M = beaked_whale$M, s = 2) %>% "["(.,1:8,), sample)
})


test_that("function does not works", {
  expect_error(htrack(A = beaked_whale$A, M = beaked_whale$M), "inputs for A, M, and s are all required.")
})