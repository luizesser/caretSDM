if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
  pr_gpkg <- here::here("tests", "testthat", "testdata", "parana.gpkg") |>
    sf::st_read(quiet = TRUE)
} else {
  pr_gpkg <- test_path("testdata","parana.gpkg") |>
    sf::st_read(quiet = TRUE)
}

test_that("select - tidyverse sa", {
  sa <- sdm_area(pr_gpkg, cell_size = 10000, crs = 6933)
  sa <- select(sa, c("CODIGOIB1", "NOMEUF2"))
  sa_col_names <- sa$grid |> colnames()
  expect_true("cell_id" %in% sa_col_names)
  expect_equal(c("cell_id", "CODIGOIB1", "NOMEUF2", "geometry"), sa_col_names)
})

test_that("select - tidyverse i", {
  sa <- sdm_area(pr_gpkg, cell_size = 10000, crs = 6933)
  i <- input_sdm(sa)
  i <- select(i, c("CODIGOIB1", "NOMEUF2"))
  i_col_names <- i$predictors$grid |> colnames()
  expect_true("cell_id" %in% i_col_names)
  expect_equal(c("cell_id", "CODIGOIB1", "NOMEUF2", "geometry"), i_col_names)
})

test_that("mutate - tidyverse sa", {
  sa <- sdm_area(pr_gpkg, cell_size = 10000, crs = 6933)
  sa <- sa |> mutate(teste=GID0/CODIGOIB1)
  sa_col_names <- sa$grid |> colnames()
  expect_true("cell_id" %in% sa_col_names)
  expect_true("teste" %in% sa_col_names)
  expect_true(round(as.numeric(sa$grid[1,"teste"])[1], 6)==0.463415)
})

test_that("mutate - tidyverse i", {
  sa <- sdm_area(pr_gpkg, cell_size = 10000, crs = 6933)
  i <- input_sdm(sa)
  i <- i |> mutate(teste=GID0/CODIGOIB1)
  sa_col_names <- i$predictors$grid |> colnames()
  expect_true("cell_id" %in% sa_col_names)
  expect_true("teste" %in% sa_col_names)
  expect_true(round(as.numeric(i$predictors$grid[1,"teste"])[1], 6) == 0.463415)
})
