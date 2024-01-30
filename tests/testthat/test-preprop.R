test_that("Warning use old Prepreocessing Result", {
  #skip(message = "Es wird im Moment kein Warning mehr geschrieben")
  result <- dsa2::dsa(series = daily_data$currency_circulation)
  expect_warning(
    dsa2::dsa(
      series = dsa2::daily_data$no2,
      pre_processing = result
    ),
    "The pre-processing result included is apparently not based on the time series to be adjusted. \nBe sure that you are not using a different time series"
  )
})

test_that("Using old preprocessing result", {
  result1 <- dsa2::dsa(series = dsa2::daily_data$currency_circulation)
  result2 <-
    dsa2::dsa(series = dsa2::daily_data$currency_circulation,
              pre_processing = result1)
  expect_equal(print(result1), print(result2))
})




