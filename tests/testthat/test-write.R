if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
  pr_gpkg <- here::here("tests", "testthat", "testdata", "parana.gpkg") |>
    sf::st_read(quiet = TRUE)
} else {
  pr_gpkg <- test_path("testdata","parana.gpkg") |>
    sf::st_read(quiet = TRUE)
}


## Test write_gpkg
test_that("write_gpkg.sdm_area - salvar arquivo com sf", {
  tmp_dir <- "/tmp"
  withr::with_tempdir(
    pattern = "tmp_sdm_area",
    {
      sa <- sdm_area(pr_gpkg, cell_size = 50000, crs = 6933)
      sa |> write_gpkg(file_path = ".", file_name = "parana")
      expect_file_exists(fs::path("parana.gpkg"))
    },
    tmpdir = tmp_dir
  )
})


test_that("write_gpkg.sdm_area - salvar arquivo com sf path invalido", {
  sa <- sdm_area(pr_gpkg, cell_size = 50000, crs = 6933)
  expect_error(
    sa |> write_gpkg(file_path = "parana.gpkg", file_name = "parana"),
    "x Assertion on file_path failed."
  )
})


test_that("write_gpkg.sdm_area - salvar arquivo com sf path invalido", {
  sa <- sdm_area(pr_gpkg, cell_size = 50000, crs = 6933)
  expect_error(
    sa |> write_gpkg(file_path = "/parana", file_name = "parana"),
    "x Assertion on file_path failed."
  )
})


test_that("write_gpkg.sdm_area - salvar arquivo com sf file path invalido", {
  sa <- sdm_area(pr_gpkg, cell_size = 50000, crs = 6933)
  expect_error(
    sa |> write_gpkg(file_path = "/tmp", file_name = 1),
    "x Assertion on file_name failed."
  )
})
