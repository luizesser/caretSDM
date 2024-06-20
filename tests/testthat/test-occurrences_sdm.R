test_that("occurrences - data.frame - single species", {
  df <- data.frame(spp_names = rep("Aa", 100), longitude = runif(100), decimalLatitude = runif(100))
  expect_s3_class(occurrences_sdm(df), "occurrences")
})

test_that("occurrences - data.frame - multiple species", {
  df <- data.frame(spp_names = c(rep("Aa", 50), rep("Bb", 50)), longitude = runif(100), decimalLatitude = runif(100))
  expect_s3_class(occurrences_sdm(df), "occurrences")
})

test_that("occurrences - tibble - single species", {
  tb <- tibble(spp_names = rep("Aa", 100), longitude = runif(100), decimalLatitude = runif(100))
  expect_s3_class(occurrences_sdm(tb), "occurrences")
})

test_that("occurrences - tibble - multiple species", {
  tb <- tibble(spp_names = c(rep("Aa", 50), rep("Bb", 50)), longitude = runif(100), decimalLatitude = runif(100))
  expect_s3_class(occurrences_sdm(tb), "occurrences")
})
