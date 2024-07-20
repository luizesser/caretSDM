test_that("sf_to_df_sdm - working fine", {
  df <- data.frame(spp_names = rep("Aa", 100), longitude = runif(100), decimalLatitude = runif(100))
  oc <- occurrences_sdm(df)
  expect_true(all(sf_to_df_sdm(oc$occurrences) == df))
})

test_that("sf_to_df_sdm - entering the wrong class", {
  df <- data.frame(spp_names = rep("Aa", 100), longitude = runif(100), decimalLatitude = runif(100))
  oc <- occurrences_sdm(df)
  expect_error(all(sf_to_df_sdm(oc) == df))
})
