test_that("wcdata", {
  expect_error(WorldClim_data(period = "a"))
  expect_error(WorldClim_data(period = "current", variable = "a"))
  expect_error(WorldClim_data(period = "current", variable = "bioc", year = "a"))
  expect_error(WorldClim_data(period = "current", variable = "bioc", year = "2090", gcm = "a"))
  expect_error(WorldClim_data(period = "current", variable = "bioc", year = "2090", gcm = "mi", ssp = "a"))
  expect_error(WorldClim_data(period = "current", variable = "bioc", year = "2090", gcm = "mi", ssp = "585", resolution = 10000))
})
