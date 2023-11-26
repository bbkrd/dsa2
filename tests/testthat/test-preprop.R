test_that("Warning use old Prepreocessing Result", {
  skip(message="Es wird im Moment kein Warning mehr geschrieben")
  result<-dsa2::dsa(series =daily_data$currency_circulation)
  expect_warning(dsa2::dsa(series =dsa2::daily_data$currency_circulation, pre_processing = result),"Warning use old Prepreocessing Result")
})

test_that("Print use old Preprocessing Result", {
  result1<-dsa2::dsa(series =dsa2::daily_data$currency_circulation)
  result2<-dsa2::dsa(series =dsa2::daily_data$currency_circulation, pre_processing = result1)
  expect_equal(print(result1),print(result2))
    })




