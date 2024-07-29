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
sa <- sdm_area(pr_gpkg, cell_size = 10000, crs = 6933)

test_that("add_predictors - rasterStack", {
  sa_pred <- add_predictors(sa, pr_raster)
  expect_equal(
    predictors(sa_pred),
    c(
      "GID0", "CODIGOIB1", "NOMEUF2", "SIGLAUF3", "wc2.1_10m_bio_1",
      "wc2.1_10m_bio_12"
    )
  )
  expect_true(
    sa_pred$grid |> nrow() <=  sa$grid |> nrow()
  )
  expect_true("cell_id" %in% colnames(sa_pred$grid))
  expect_true("geometry" %in% colnames(sa_pred$grid))
  checkmate::expect_integer(
    sa_pred$grid$cell_id,
    any.missing = FALSE,
    all.missing = FALSE,
    unique = TRUE,
    sorted = TRUE,
    null.ok = FALSE
  )
})

test_that("add_predictors - rasterStack selecionando uma variável", {
  sa_pred <- add_predictors(sa, pr_raster, c("wc2.1_10m_bio_1"))
  expect_equal(
    predictors(sa_pred),
    c("GID0", "CODIGOIB1", "NOMEUF2", "SIGLAUF3", "wc2.1_10m_bio_1")
  )
})

test_that("add_predictors - stars", {
  sa_pred <- add_predictors(sa, pr_stars)
  expect_equal(
    predictors(sa_pred),
    c(
      "GID0", "CODIGOIB1", "NOMEUF2", "SIGLAUF3", "wc2.1_10m_bio_1",
      "wc2.1_10m_bio_12"
    )
  )
})

test_that("add_predictors - sf", {
  sa_pred <- add_predictors(sa, pr_gpkg)
  expect_equal(
    predictors(sa_pred),
    c(
      "GID0.x", "CODIGOIB1.x", "NOMEUF2.x", "SIGLAUF3.x", "GID0.y",
      "CODIGOIB1.y", "NOMEUF2.y", "SIGLAUF3.y"
    )
  )
})


test_that("add_predictors - rasterStack não usando sdm_area", {
  expect_snapshot(
    sa_pred <- add_predictors(pr_raster, pr_raster),
    error = TRUE
  )
})


test_that("add_predictors - lista de variáveis invalida", {
  expect_snapshot(
    sa_pred <- add_predictors(sa, pr_raster, list("foo"))
  )
})

test_that("add_predictors - stack/terra", {
  if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
    pr <-
      terra::rast(here::here("tests", "testthat", "testdata", "parana.tiff"))
  } else {
    pr <- terra::rast(test_path("testdata/parana.tiff"))
  }
  sa_pred <- add_predictors(sa, pr)
  expect_equal(
    predictors(sa_pred),
    c(
      "GID0", "CODIGOIB1", "NOMEUF2", "SIGLAUF3", "wc2.1_10m_bio_1",
      "wc2.1_10m_bio_12"
    )
  )
})


