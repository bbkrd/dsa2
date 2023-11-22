# seriesx <- ts(rnorm(14), freq=7)
# 
# test_that("estimate_component with stl_method()", {
#   expect_no_error(dsa2:::.estimate_component.stl_method(method=stl_method(), 
#                                                         series = seriesx, 
#                                                         log = TRUE))
# })
# 
# test_that("estimate_component with seats_method()", {
#   expect_no_error(dsa2:::.estimate_component.seats_method(method=seats_method(), 
#                                                           series = seriesx, 
#                                                           log = TRUE))
# })
# 
# test_that("estimate_component with 'stl'", {
#   expect_no_error(dsa2:::.estimate_component.stl_method(method="stl", 
#                                                         series = seriesx, 
#                                                         log = TRUE))
# })
# 
# test_that("estimate_component with 'seats'", {
#   expect_no_error(dsa2:::.estimate_component.seats_method(method="seats", 
#                                                           series = seriesx, 
#                                                           log = TRUE))
# })
