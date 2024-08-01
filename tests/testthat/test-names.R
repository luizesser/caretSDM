if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
  pr_tif <- here::here("tests", "testthat", "testdata", "parana.tiff") |>
    stars::read_stars(quiet = TRUE)
  pr_gpkg <- here::here("tests", "testthat", "testdata", "parana.gpkg") |>
    sf::st_read(quiet = TRUE)
} else {
  pr_tif <- test_path("testdata", "ES.csv", "parana.tiff") |>
    stars::read_stars(quiet = TRUE)
  pr_gpkg <- test_path("testdata","parana.gpkg") |>
    sf::st_read(quiet = TRUE)
}

# Predictors

test_that("predictors - sdm_area", {
  sa <- sdm_area(pr_gpkg, cell_size = 50000, crs = 6399)
  expect_equal(
    predictors(sa),
    c("GID0", "CODIGOIB1", "NOMEUF2", "SIGLAUF3")
  )
  expect_equal(
    get_predictor_names(sa),
    c("GID0", "CODIGOIB1", "NOMEUF2", "SIGLAUF3")
  )
})


test_that("predictors - set/get rename predictors", {
  sa <- sdm_area(pr_gpkg, cell_size = 50000, crs = 6399)
  expect_equal(
    set_predictor_names(sa, get_predictor_names(sa) |> tolower()) |> predictors(),
    c("gid0", "codigoib1", "nomeuf2", "siglauf3")
  )
})

test_that("predictors - set/get invalid rename predictors", {
  sa <- sdm_area(pr_gpkg, cell_size = 50000, crs = 6399)
  expect_snapshot(
    expect_error(
      set_predictor_names(sa, c("gid0", "codigoib1")),
      "Assertion on new_names failed."
    )
  )
})

