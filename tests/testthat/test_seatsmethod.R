# Unit test for seats_method 

testthat::test_that("seats_method Warning period length", {
  expect_warning(seats_method(period = 1),
                 "period in seats_method\\() should be at least 2")
})


testthat::test_that("seats_method Warning log", {
  expect_warning(seats_method(period = 12,  
                              log = 0, 
                              sn = FALSE,
                              stde = FALSE,
                              nbcasts = 0,
                              nfcasts = 0),
                 "log needs to be a boolean")
})

testthat::test_that("seats_method error class", {
  expect_error(seats_method(period = "twelve",  
                              log = NULL, 
                              sn = FALSE,
                              stde = FALSE,
                              nbcasts = 0,
                              nfcasts = 0),
                 "period in seats_method\\() needs to be of class numeric or integer")
})


testthat::test_that("seats_method Warning sn", {
  expect_warning(seats_method(period = 12,  
                              log = NULL, 
                              sn = 2,
                              stde = FALSE,
                              nbcasts = 0,
                              nfcasts = 0),
                 "sn and stde in seats_method\\() should be either TRUE or FALSE")
})

testthat::test_that("seats_method Warning stde", {
  expect_warning(seats_method(period = 12,  
                              log = NULL, 
                              sn = FALSE,
                              stde = 2,
                              nbcasts = 0,
                              nfcasts = 0),
                 "sn and stde in seats_method\\() should be either TRUE or FALSE")
})


testthat::test_that("seats_method Warning nbcasts", {
  expect_warning(seats_method(period = 12,  
                              log = NULL, 
                              sn = FALSE,
                              stde = FALSE,
                              nbcasts = "Zero",
                              nfcasts = 0), 
                 "nbcasts and nfcasts in seats_method\\() need to be of class numeric or integer")
})

testthat::test_that("seats_method Warning nfcasts", {
  expect_warning(seats_method(period = 12,  
                              log = NULL, 
                              sn = FALSE,
                              stde = FALSE,
                              nbcasts = 0,
                              nfcasts = "Zero"),
                 "nbcasts and nfcasts in seats_method\\() need to be of class numeric or integer")
})

