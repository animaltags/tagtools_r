BW <- beaked_whale
sample <- data.frame(
  northing = c(-2.120204789, -4.237066670, -6.248253759, -8.481072583, -10.988549602,
               -13.427957913, -16.052810633, -18.794814608, -21.568980996, -24.373243168),
  easting = c(2.10506209, 4.15910906, 5.90078747, 7.54824624, 9.17719370,
              10.65608016, 11.91137029, 13.04227137, 14.09101129, 15.11195755),
  dunno = c(-0.271007831, 0.276520077, 1.662767829, 2.803118513, 2.560035827,
            1.631498305, 0.900475130, 0.450500748, -0.001323815, -0.307562929)
)

test_that("function works", {
  expect_equal(ptrack(A = BW$A$data, M = BW$M$data, s = 3, 
                      sampling_rate = BW$A$sampling_rate, fc = NULL, 
                      return_pe = TRUE)$T %>% "["(.,1:10,), sample)
})


test_that("function does not works", {
  expect_error(ptrack(A = BW$A$data, M = BW$M$data, s = 12, 
                      sampling_rate = BW$A$sampling_rate, fc = 2, 
                      return_pe = TRUE), "frequency must be nondecreasing starting from 0 and ending at 1")
})