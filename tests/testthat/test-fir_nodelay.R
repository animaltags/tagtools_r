test_x <- sin(t(2 * pi * 0.05 * (1:10)) +
           t(cos(2 * pi * 0.25 * (1:10))))


test_output <- c(0.21759870900,
                 0.29154081030,
                 0.46953534860,
                 0.64525212003,
                 0.71824422514,
                 0.65663166257,
                 0.52231875077,
                 0.40430695045,
                 0.34348064015,
                 0.32711544584)

test_that("function works", {
  expect_equal(fir_nodelay(x = test_x, n = 10, fc = 0.2, qual = "low"), test_output)
})

test_matrix <- matrix(seq(0, 20, 
                          length.out = 5), nrow = 10, ncol = 1)

test_output2 <- c(6.2824045317,
                  7.2299565071,
                  9.2354307237,
                  10.6004283568,
                  10.4568098221,
                  9.5431901779,
                  9.3995716432,
                  10.7645692763,
                  12.7700434929,
                  13.7175954683)

test_that("function works", {
  expect_equal(fir_nodelay(x = test_matrix, n = 10, fc = 0.2, qual = "low"), test_output2)
})

test_data <- matrix(2, 4)

test_that("function does not work", {
  expect_error(fir_nodelay(x = test_data, n = 10, fc = 0.2, qual = "low"), 
               "subscript out of bounds")
})
