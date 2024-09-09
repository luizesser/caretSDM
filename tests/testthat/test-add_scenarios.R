if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
  pr_stars <-
    here::here("tests", "testthat", "testdata", "parana.tiff") |>
    stars::read_stars(quiet = TRUE)
  pr_raster <-
    here::here("tests", "testthat", "testdata", "parana.tiff") |>
    raster::stack()
  pr_gpkg <-
    here::here("tests", "testthat", "testdata", "parana.gpkg") |>
    sf::st_read(quiet = TRUE)
  pr_shp <-
    here::here("tests", "testthat", "testdata", "parana.shp") |>
    sf::st_read(quiet = TRUE)
} else {
  pr_stars <- test_path("testdata", "parana.tiff") |>
    stars::read_stars(quiet = TRUE)
  pr_raster <- test_path("testdata", "parana.tiff") |>
    raster::stack()
  pr_gpkg <- test_path("testdata", "parana.gpkg") |>
    sf::st_read(quiet = TRUE)
  pr_shp <- test_path("testdata", "parana.shp") |>
    sf::st_read(quiet = TRUE)
}
sa <- sdm_area(pr_gpkg, cell_size = 20000, crs = 6933)
sa <- add_predictors(sa, pr_raster)
sa <- select(sa, c("wc2.1_10m_bio_1","wc2.1_10m_bio_12"))
sa <- set_predictor_names(sa, c("bio01", "bio12"))

test_that("add_scenarios - stars", {
  sa_pred <- add_scenarios(sa, scen)
  expect_equal(
    get_predictor_names(sa_pred),
    c("bio01", "bio12")
  )
  expect_equal(
    get_predictor_names(sa_pred),
    get_predictor_names(sa_pred$scenarios)
  )
  expect_equal(
    names(sa_pred$scenarios$data$ssp585_2090),
    c("cell_id", "bio01", "bio12", "geometry")
  )
})

test_that("add_scenarios - stars selecionando uma variável", {
  sa_pred <- add_scenarios(sa, scen, variables_selected=c("bio01"))
  expect_equal(
    get_predictor_names(sa_pred),
    c("bio01")
  )
  expect_equal(
    get_predictor_names(sa_pred),
    get_predictor_names(sa_pred$scenarios)
  )
  expect_equal(
    names(sa_pred$scenarios$data$ssp585_2090),
    c("cell_id", "bio01", "geometry")
  )
})

test_that("add_scenarios - NULL", {
  sa_pred <- add_scenarios(sa, variables_selected=c("bio01"))
  expect_equal(
    sa_pred$scenarios$data$current,
    sa_pred$grid
  )
})

test_that("add_scenarios - SpatRaster", {
  suppressWarnings(sa_pred <- add_scenarios(sa, as(scen, "SpatRaster")))
  expect_equal(
    sa_pred$scenarios$data$current,
    sa_pred$grid
  )
  expect_equal(
    get_predictor_names(sa_pred),
    c("bio01", "bio12")
  )
})

test_that("add_scenarios - RasterStack", {
  suppressWarnings(test <- as(scen, "Raster"))
  test <- stack(test)
  sa_pred <- add_scenarios(sa, test)
  expect_equal(
    sa_pred$scenarios$data$current,
    sa_pred$grid
  )
  expect_equal(
    get_predictor_names(sa_pred),
    c("bio01", "bio12")
  )
  expect_equal(
    names(sa_pred$scenarios$data[[1]]),
    c("cell_id", "bio01", "bio12", "geometry")
  )
})

test_that("add_scenarios - stars mas a variável é inválida", {
  expect_error(add_scenarios(sa, scen, variables_selected=c("bio01", "bio12", "bio13")))
})

test_that("add_scenarios - stars mas nem todas variaveis estão presente", {
  expect_error(add_scenarios(sa, scen, variables_selected=c("bio1")))
  expect_error(add_scenarios(sa, scen, variables_selected=c("NA")))
  expect_error(add_scenarios(sa, scen, variables_selected=c(1,2,3)))
})

test_that("add_scenarios - input_sdm", {
  i <- input_sdm(sa)
  i <- add_scenarios(i)
  expect_true("scenarios" %in% names(i))
  expect_true(all(i$scenarios$grid == i$predictors$grid))
  expect_equal(i$scenarios$cell_size, i$predictors$cell_size)
  expect_equal("current", names(i$scenarios$data))
  expect_true(all(i$predictors$grid == i$scenarios$data$current))
})

test_that("add_scenarios - stars e vars tem nomes diferentes", {
  sa <- sdm_area(pr_gpkg, cell_size = 20000, crs = 6933)
  sa <- add_predictors(sa, pr_raster)
  sa <- select(sa, c("wc2.1_10m_bio_1","wc2.1_10m_bio_12"))
  expect_no_error(sa_scen <- add_scenarios(sa, scen))
  expect_equal(get_predictor_names(sa),get_predictor_names(sa_scen))
  expect_true(all(get_predictor_names(sa) %in% names(sa_scen$scenarios$data$ssp126_2030)))
})

