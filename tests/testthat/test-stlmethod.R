  # Unit tests for stl_method

testthat::test_that("stl_method Warning log", {
  expect_warning(stl_method(period = NA, 
                              swindow = 13, 
                              log = 0,
                              twindow = 0, 
                              ninnerloop = 1, 
                              nouterloop = 15, 
                              nojump = FALSE, 
                              weight.threshold = 0.001, 
                              weight.function = c('BIWEIGHT')),
                 "log needs to be a boolean")
})

testthat::test_that("stl_method Warning nojump", {
  expect_warning(stl_method(period = NA, 
                              swindow = 13, 
                              log = NULL,
                              twindow = 0, 
                              ninnerloop = 1, 
                              nouterloop = 15, 
                              nojump = 0, 
                              weight.threshold = 0.001, 
                              weight.function = c('BIWEIGHT')),
                 "nojump needs to be a boolean")
})

testthat::test_that("stl_method Warning class period", {
  expect_error(stl_method(period = "Fifteen", 
                          swindow = 13, 
                          log = NULL,
                          twindow = 0, 
                          ninnerloop = 1, 
                          nouterloop = 15, 
                          nojump = FALSE, 
                          weight.threshold = 0.001, 
                          weight.function = c('BIWEIGHT')),
                 "period in stl_method\\() needs to be of class numeric or integer")
})

testthat::test_that("stl_method Warning period length", {
  expect_warning(stl_method(period = 1, 
                              swindow = 13, 
                              log = NULL,
                              twindow = 0, 
                              ninnerloop = 1, 
                              nouterloop = 15, 
                              nojump = FALSE, 
                              weight.threshold = 0.001, 
                              weight.function = c('BIWEIGHT')),
                 "period in stl_method\\() should be at least 2")
})

testthat::test_that("stl_method Warning class swindow", {
  expect_warning(stl_method(period = NA, 
                              swindow = "thirteen", 
                              log = NULL,
                              twindow = 0, 
                              ninnerloop = 1, 
                              nouterloop = 15, 
                              nojump = FALSE, 
                              weight.threshold = 0.001, 
                              weight.function = c('BIWEIGHT')),
                 "swindow in stl_method\\() need to be of class numeric or integer")
})

testthat::test_that("stl_method Warning class twindow", {
  expect_warning(stl_method(period = NA, 
                              swindow = 13, 
                              log = NULL,
                              twindow = "zero", 
                              ninnerloop = 1, 
                              nouterloop = 15, 
                              nojump = FALSE, 
                              weight.threshold = 0.001, 
                              weight.function = c('BIWEIGHT')),
                 "twindow, ninnerloop, nouterloop and weight.threshold in 
             stl_method\\() need to be of class numeric or integer")
})

testthat::test_that("stl_method Warning class ninnerloop", {
  expect_warning(stl_method(period = NA, 
                              swindow = 13, 
                              log = NULL,
                              twindow = 0, 
                              ninnerloop = "one", 
                              nouterloop = 15, 
                              nojump = FALSE, 
                              weight.threshold = 0.001, 
                              weight.function = c('BIWEIGHT')),
                 "twindow, ninnerloop, nouterloop and weight.threshold in 
             stl_method\\() need to be of class numeric or integer")
})

testthat::test_that("stl_method Warning class nouterloop", {
  expect_warning(stl_method(period = NA, 
                              swindow = 13, 
                              log = NULL,
                              twindow = 0, 
                              ninnerloop = 1, 
                              nouterloop = "fifteen", 
                              nojump = FALSE, 
                              weight.threshold = 0.001, 
                              weight.function = c('BIWEIGHT')),
                 "twindow, ninnerloop, nouterloop and weight.threshold in 
             stl_method\\() need to be of class numeric or integer")
})

testthat::test_that("stl_method Warning class weight.threshold", {
  expect_warning(stl_method(period = NA, 
                              swindow = 13, 
                              log = NULL,
                              twindow = 0, 
                              ninnerloop = 1, 
                              nouterloop = 15, 
                              nojump = FALSE, 
                              weight.threshold = "verysmall", 
                              weight.function = c('BIWEIGHT')),
                 "twindow, ninnerloop, nouterloop and weight.threshold in 
             stl_method\\() need to be of class numeric or integer")
})

testthat::test_that("stl_method Warning class weigth.function", {
  expect_warning(stl_method(period = NA, 
                              swindow = 13, 
                              log = NULL,
                              twindow = 0, 
                              ninnerloop = 1, 
                              nouterloop = 15, 
                              nojump = FALSE, 
                              weight.threshold = 0.001, 
                              weight.function = c(5)),
                 "weight.function in stl_method\\() need to be of class character")
})

