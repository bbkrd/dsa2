# Testing the regressors

test_that("holidays", {
  
  ep <- xts::endpoints(holidays$DstAutumn, on = "years")
  yearsum_autumn <-
    xts::period.apply(holidays$DstAutumn, 
                      INDEX = ep[ep >= 0 &
                      ep <= length(holidays$DstAutumn)], 
                      FUN = sum)
  
  expect_equal(
    as.numeric(yearsum_autumn["1950/1979"]),
    rep(0, length(yearsum_autumn["1950/1979"]))
  )
  
  expect_equal(
    as.numeric(yearsum_autumn["1980/2025"]),
    rep(1, length(yearsum_autumn["1980/2025"]))
  )
  
  
  ep <- xts::endpoints(holidays$DstSpring, on = "years")
  yearsum_spring <-
    xts::period.apply(holidays$DstSpring, 
                      INDEX = ep[ep >= 0 &
                                   ep <= length(holidays$DstSpring)], 
                      FUN = sum)
  
  expect_equal(
    as.numeric(yearsum_spring["1950/1979"]),
    rep(0, length(yearsum_spring["1950/1979"]))
  )
  
  expect_equal(
    as.numeric(yearsum_spring["1980/2025"]),
    rep(1, length(yearsum_spring["1980/2025"]))
  )
  
  
})



