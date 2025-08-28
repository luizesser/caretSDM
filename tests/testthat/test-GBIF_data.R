test_that("gbif", {
  skip_on_cran()
  expect_error(GBIF_data(file = NULL, as_df = "a"))
  expect_error(GBIF_data(vector(), file = NULL, as_df = FALSE))
})
