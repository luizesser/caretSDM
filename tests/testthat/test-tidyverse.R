if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
  pr_gpkg <- here::here("tests", "testthat", "testdata", "parana.gpkg") |>
    sf::st_read(quiet = TRUE)
} else {
  pr_gpkg <- test_path("testdata","parana.gpkg") |>
    sf::st_read(quiet = TRUE)
}

test_that("select - tidyverse", {
  sa <- sdm_area(pr_gpkg, cell_size = 10000, crs = 6399)

  sa |> select("NOMEUF2", "SIGLAUF3")
  sa_col_names <- sa$grid |>
    colnames()
  expect_true(
    "cell_id" %in% sa_col_names
  )
})
