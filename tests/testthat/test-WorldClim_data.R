test_that("wcdata", {
  skip_on_cran()
  expect_error(WorldClim_data(period = "a"))
  expect_error(WorldClim_data(period = "current", variable = "a"))
  expect_error(WorldClim_data(period = "current", variable = "bioc", year = "a"))
  expect_error(WorldClim_data(period = "current", variable = "bioc", year = "2090", gcm = "a"))
  expect_error(WorldClim_data(period = "current", variable = "bioc", year = "2090", gcm = "mi", ssp = "a"))
  expect_error(WorldClim_data(period = "current", variable = "bioc", year = "2090", gcm = "mi", ssp = "585", resolution = 10000))

  local_mocked_bindings(
    .download_file_httr2 = function(...) TRUE,
    .package = "caretSDM"
  )
  expect_no_error(WorldClim_data(period = "future", gcm = "all"))


  local_mocked_bindings(
    .download_file_httr2 = function(...) FALSE,
    .package = "caretSDM"
  )
  expect_message(WorldClim_data(period = "current"))



  })
