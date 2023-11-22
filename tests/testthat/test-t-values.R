# Various dsa2-objects
# a = additive vs. m = multiplicative
# o = with outliers vs. c = with calendar regressors
r_ao <- readRDS(testthat::test_path("fixtures","r_ao.rds"))
r_mo <- readRDS(testthat::test_path("fixtures","r_mo.rds"))
r_aco <- readRDS(testthat::test_path("fixtures","r_aco.rds"))
r_mco <- readRDS(testthat::test_path("fixtures","r_mo.rds"))

test_that("t-values", {
  
  outliers1 <- as.numeric(dsa2:::.outOutlier(r_ao)$t.Value)
  outliers2 <- as.numeric(dsa2:::.outOutlier(r_mo)$t.Value)
  outliers3 <- as.numeric(dsa2:::.outOutlier(r_aco)$t.Value)
  outliers4 <- as.numeric(dsa2:::.outOutlier(r_mco)$t.Value)
  
  expect_equal(outliers1, 
               c(-21.262, 31.620, 31.891, 22.658, -7.224, -28.693, -6.469), 
               tolerance = 0.001)
  
  expect_equal(outliers2, 
               c(-34.230, 32.012, 23.268, 17.385, -31.083),
               tolerance = 0.001)
  
  expect_equal(outliers3, 
               c(-21.234, 31.624, 31.861, 22.639, -7.221, -28.655, -6.458), 
               tolerance = 0.001)
  
  expect_equal(outliers4, 
               c(-34.230, 32.012, 23.268, 17.385, -31.083), 
               tolerance = 0.001)

})
