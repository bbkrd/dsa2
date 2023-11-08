# Various dsa2-objects
# a = additive vs. m = multiplicative
# o = with outliers vs. c = with calendar regressors
r_ao <- readRDS(testthat::test_path("fixtures","r_ao.rds"))
r_mo <- readRDS(testthat::test_path("fixtures","r_ao.rds"))
r_aco <- readRDS(testthat::test_path("fixtures","r_ao.rds"))
r_mco <- readRDS(testthat::test_path("fixtures","r_ao.rds"))

test_that("Order of outliers", {
  
  outliers1 <- dsa2:::.outOutlier(r_ao)
  outliers2 <- dsa2:::.outOutlier(r_mo)
  outliers3 <- dsa2:::.outOutlier(r_aco)
  outliers4 <- dsa2:::.outOutlier(r_mco)
  
  expect_equal(
    order(outliers1$Date),
    1:nrow(outliers1)
  )
  
  expect_equal(
    order(outliers2$Date),
    1:nrow(outliers2)
  )
  
  expect_equal(
    order(outliers3$Date),
    1:nrow(outliers3)
  )
  
  expect_equal(
    order(outliers4$Date),
    1:nrow(outliers4)
  )
  
})
