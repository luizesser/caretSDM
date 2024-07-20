test_that("find 3 columns works", {
  df <- data.frame(spp_names = rep("Aa", 100), longitude = runif(100), decimalLatitude = runif(100))
  expect_length(find_columns(df), 3)
})

test_that("find 2 columns works", {
  df <- data.frame(longitude = runif(100), decimalLatitude = runif(100))
  expect_length(find_columns(df), 2)
})

test_that("find 3 columns given names works", {
  df <- data.frame(spp_names = rep("Aa", 100), longitude = runif(100), decimalLatitude = runif(100))
  expect_length(find_columns(df, col_names = c("spp_names", "longitude", "decimalLatitude")), 3)
})

test_that("fail given wrong names", {
  df <- data.frame(spp_names = rep("Aa", 100), longitude = runif(100), decimalLatitude = runif(100))
  expect_error(find_columns(df, col_names = c("a", "b", "c")))
})

test_that("find 2 columns given names works", {
  df <- data.frame(spp_names = rep("Aa", 100), longitude = runif(100), decimalLatitude = runif(100))
  expect_length(find_columns(df, col_names = c("longitude", "decimalLatitude")), 2)
})

test_that("find 2 columns with no species", {
  df <- data.frame(spp_names = rep("Aa", 100), longitude = runif(100), decimalLatitude = runif(100))
  expect_length(find_columns(df, spp = F), 2)
})

test_that("missing coordinate column", {
  df <- data.frame(spp_names = rep("Aa", 100), decimalLatitude = runif(100))
  expect_error(find_columns(df))
})

test_that("columns with dubious names", {
  df <- data.frame(a = rep("Aa", 100), b = runif(100), c = runif(100))
  expect_error(find_columns(df))
})
