test_that("STL ninnerloop default", {
  result<-dsa2:::.estimate_component.stl_method(method = stl_method(period = 7, log = F),series =daily_data$elec_consumption) 
  expect_equal(result$adjustment$parameters$ninnerloop, 1)
})


test_that("STL nouterloop default", {
  result<-dsa2:::.estimate_component.stl_method(method = stl_method(period = 7, log = F),series =daily_data$elec_consumption) 
  expect_equal(result$adjustment$parameters$nouterloop, 15)
})

test_that("STL SA", {
  result<-dsa2:::.estimate_component.stl_method(method = stl_method(period = 7, log = F),series =daily_data$elec_consumption) 
  expected_decomopsition<-sa_2<-readRDS(testthat::test_path("fixtures","data_test-example.rds"))
  expect_equal(result$seasComp, expected_decomopsition)
})

test_that("STL SA with tolerance", {
  result<-dsa2:::.estimate_component.stl_method(method = stl_method(period = 7, log = F),series =daily_data$elec_consumption) 
  expected_seasComp<-sa_2<-readRDS(testthat::test_path("fixtures","data_test-example.rds"))
  expect_equal(result$seasComp,  expected_seasComp*0.9991, tolerance=0.00001)
})
