timez <- seq(from = 0, by = 1/512, to = 1)
x <- sin(2*pi*40*timez)

sample <- complex(real = c(0.0000000000, 0.4713967368, 0.8314696123, 0.9951847267,
                           0.9238795325, 0.6343932842, 0.1950903220, -0.2902846773,
                           -0.7071067812, -0.9569403357),
                  imaginary = c(-0.7332306264, -0.8601722812, -0.4838726524, -0.0933391897,
                                0.4192789560, 0.7728545813, 1.0039471140, 0.9550682189,
                                0.7235302742, 0.2877666791))

test_that("function works", {
  expect_equal(hilbert_transform(x)[1:10], sample)
})


test_that("function does not works", {
  expect_error(hilbert_transform("2"), "non-numeric argument")
})