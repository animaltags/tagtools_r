BW <- beaked_whale
sample <- data.frame(
  northing = c(-2.0953960268,  -4.2082928031,  -6.3496047671,  -8.6432735759,
               -11.1641458942, -13.7114748259, -16.2961650853,
               -18.9931458704, -21.7684502686, -24.5604780297),
  easting = c(2.1177740584,  4.1596035981,  6.0162834723,  7.7445818565,  
              9.3680949337, 10.8186918424, 12.0955444166,
              13.2142832650, 14.2482692714, 15.3227149168),
  dunno = c(0.352630867004, 0.958105345105, 1.941732773214, 2.808951367677,
            2.905946436717, 2.268071901974, 1.437997346985,
            0.748999499727, 0.270921643646, 0.046985187928)
)

test_that("function works", {
  expect_equal(ptrack(A = BW$A$data, M = BW$M$data, s = 3, 
                      sampling_rate = BW$A$sampling_rate, fc = 0.2, 
                      return_pe = TRUE)$track %>% "["(.,1:10,), sample)
})


test_that("function does not work", {
  expect_error(ptrack(A = BW$A$data, M = BW$M$data, s = 12, 
                      sampling_rate = BW$A$sampling_rate, fc = 2, 
                      return_pe = TRUE), "frequency vector f must be nondecreasing between 0 and 1")
})
