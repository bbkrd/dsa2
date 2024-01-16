  # Unit tests for stl_method

testthat::test_that("stl_method Warning log", {
  expect_warning(stl_method(period = NA, 
                              swindow = 13, 
                              log = 0,
                              twindow = 0, 
                              ninnerloop = 1, 
                              nouterloop = 15, 
                              sjump = 0,
                              tjump = 0,
                              ljump = 0,
                              weight.threshold = 0.001, 
                              weight.function = c('BIWEIGHT'),
                              legacy = FALSE),
                 "log needs to be a boolean")
})

testthat::test_that("stl_method Warning sjump", {
  expect_warning(stl_method(period = NA, 
                              swindow = 13, 
                              log = NULL,
                              twindow = 0, 
                              ninnerloop = 1, 
                              nouterloop = 15, 
                              sjump = "zero",
                              tjump = 0,
                              ljump = 0,
                              weight.threshold = 0.001, 
                              weight.function = c('BIWEIGHT'),
                              legacy = FALSE),
                 "sjump, tjump and ljump need to be numericals")
})

testthat::test_that("stl_method Warning tjump", {
  expect_warning(stl_method(period = NA, 
                            swindow = 13, 
                            log = NULL,
                            twindow = 0, 
                            ninnerloop = 1, 
                            nouterloop = 15, 
                            sjump = 0,
                            tjump = "zero",
                            ljump = 0,
                            weight.threshold = 0.001, 
                            weight.function = c('BIWEIGHT'),
                            legacy = FALSE),
                 "sjump, tjump and ljump need to be numericals")
})

testthat::test_that("stl_method Warning ljump", {
  expect_warning(stl_method(period = NA, 
                            swindow = 13, 
                            log = NULL,
                            twindow = 0, 
                            ninnerloop = 1, 
                            nouterloop = 15, 
                            sjump = 0,
                            tjump = 0,
                            ljump = "zero",
                            weight.threshold = 0.001, 
                            weight.function = c('BIWEIGHT'),
                            legacy = FALSE),
                 "sjump, tjump and ljump need to be numericals")
})

testthat::test_that("stl_method Warning class period", {
  expect_error(stl_method(period = "Fifteen", 
                          swindow = 13, 
                          log = NULL,
                          twindow = 0, 
                          ninnerloop = 1, 
                          nouterloop = 15, 
                          sjump = 0,
                          tjump = 0,
                          ljump = 0,
                          weight.threshold = 0.001, 
                          weight.function = c('BIWEIGHT'),
                          legacy = FALSE),
                 "period in stl_method\\() needs to be of class numeric or integer")
})

testthat::test_that("stl_method Warning period length", {
  expect_warning(stl_method(period = 1, 
                              swindow = 13, 
                              log = NULL,
                              twindow = 0, 
                              ninnerloop = 1, 
                              nouterloop = 15, 
                              sjump = 0,
                              tjump = 0,
                              ljump = 0,
                              weight.threshold = 0.001, 
                              weight.function = c('BIWEIGHT'),
                              legacy = FALSE),
                 "period in stl_method\\() should be at least 2")
})

testthat::test_that("stl_method Warning class swindow", {
  expect_warning(stl_method(period = NA, 
                              swindow = "thirteen", 
                              log = NULL,
                              twindow = 0, 
                              ninnerloop = 1, 
                              nouterloop = 15, 
                              sjump = 0,
                              tjump = 0,
                              ljump = 0,
                              weight.threshold = 0.001, 
                              weight.function = c('BIWEIGHT'),
                              legacy = FALSE),
                 "swindow in stl_method\\() need to be of class numeric or integer")
})

testthat::test_that("stl_method Warning class twindow", {
  expect_warning(stl_method(period = NA, 
                              swindow = 13, 
                              log = NULL,
                              twindow = "zero", 
                              ninnerloop = 1, 
                              nouterloop = 15, 
                              sjump = 0,
                              tjump = 0,
                              ljump = 0,
                              weight.threshold = 0.001, 
                              weight.function = c('BIWEIGHT'),
                              legacy = FALSE),
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
                              sjump = 0,
                              tjump = 0,
                              ljump = 0,
                              weight.threshold = 0.001, 
                              weight.function = c('BIWEIGHT'),
                              legacy = FALSE),
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
                              sjump = 0,
                              tjump = 0,
                              ljump = 0,
                              weight.threshold = 0.001, 
                              weight.function = c('BIWEIGHT'),
                              legacy = FALSE),
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
                              sjump = 0,
                              tjump = 0,
                              ljump = 0, 
                              weight.threshold = "verysmall", 
                              weight.function = c('BIWEIGHT'),
                              legacy = FALSE),
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
                              sjump = 0,
                              tjump = 0,
                              ljump = 0, 
                              weight.threshold = 0.001, 
                              weight.function = c(5),
                              legacy = FALSE),
                 "weight.function in stl_method\\() need to be of class character")
})

