
result<-dsa2:::.estimate_component.stl_method(method = stl_method(period = 7, log = F),
                                              series =daily_data$elec_consumption) 
test_that("STL ninnerloop default", {
  
  expect_equal(result$adjustment$parameters$ninnerloop, 1)
})

test_that("STL nouterloop default", {
  expect_equal(result$adjustment$parameters$nouterloop, 15)
})

test_that("STL SA", {
  result<-dsa2:::.estimate_component.stl_method(method = stl_method(period = 7, log = F),
                                                series =daily_data$elec_consumption) 
  expected_seasComp<-readRDS(testthat::test_path("fixtures","data_test-example.rds"))
  expect_equal(result$seasComp, expected_seasComp)
})

test_that("STL SA with tolerance", {
  result<-dsa2:::.estimate_component.stl_method(method = stl_method(period = 7, log = F),
                                                series =daily_data$elec_consumption) 
  expected_seasComp<-readRDS(testthat::test_path("fixtures","data_test-example.rds"))
  expect_equal(result$seasComp,  expected_seasComp*0.9991, tolerance=0.01)
})



test_that("x11_method Warning log", {
  expect_warning(x11_method(period = 12,log = 5),"log needs to be a boolean")
})

# Beispiel für excape characters
test_that("x11_method Warning period", {
  expect_warning(x11_method(period = 1,log = T),"period in x11_method\\() should be at least 2")
})

test_that("dsa", {
  skip(message="jars werde beim Unit-test nicht ordentlich eingelesen, muss gefixed werden")
  result_adjust<-dsa(na.omit(daily_data$elec_consumption))
  expect_equal(result_adjust$adjustment$parameters$multiplicative, T)
})