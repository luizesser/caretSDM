test_that("gbif", {
  skip_on_cran()
  expect_error(GBIF_data(file = NULL, as_df = "a"))
  expect_error(GBIF_data(vector(), file = NULL, as_df = FALSE))

  local_mocked_bindings(
    read.csv = function(...) occ,
    .package = "utils"
  )
  local_mocked_bindings(
    occ_data = function(...) occ,
    .package = "rgbif"
  )

  expect_no_error(GBIF_data("Araucaria angustifolia", file = NULL, as_df = TRUE))
  #expect_no_error(GBIF_data("Araucaria angustifolia", file = NULL, as_df = FALSE))
  expect_no_error(GBIF_data("Araucaria angustifolia", file = "teste", as_df = TRUE))
  expect_no_error(GBIF_data("Araucaria angustifolia", file = "teste", as_df = FALSE))

})
