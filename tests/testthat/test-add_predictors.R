## Test as expected to work
test_that("add_predictors", {
  sa <- sdm_area(test_path("parana.gpkg"))
  pred <- read_stars(test_path("parana.tiff"))
  expect_no_error(suppressWarnings(sa_pred <- add_predictors(sa, pred)))
})

## Test for different inputs
test_that("add_predictors - stars", {
  sa <- sdm_area(test_path("parana.gpkg"))
  pred <- read_stars(test_path("parana.tiff"))
  suppressWarnings(sa_pred <- add_predictors(sa, pred))
  expect_equal(sa_pred$predictors, c("wc2.1_10m_bio_1", "wc2.1_10m_bio_12", "GID0", "CODIGOIB1", "NOMEUF2", "SIGLAUF3" ))
})

test_that("add_predictors - rasterStack", {
  sa <- sdm_area(test_path("parana.gpkg"))
  pred <- raster::stack(test_path("parana.tiff"))
  suppressWarnings(sa_pred <- add_predictors(sa, pred))
  expect_equal(sa_pred$predictors, c("wc2.1_10m_bio_1", "wc2.1_10m_bio_12", "GID0", "CODIGOIB1", "NOMEUF2", "SIGLAUF3" ))
})

test_that("add_predictors - terra", {
  sa <- sdm_area(test_path("parana.gpkg"))
  pred <- terra::rast(test_path("parana.tiff"))
  suppressWarnings(sa_pred <- add_predictors(sa, pred))
  expect_equal(sa_pred$predictors, c("wc2.1_10m_bio_1", "wc2.1_10m_bio_12", "GID0", "CODIGOIB1", "NOMEUF2", "SIGLAUF3" ))
})

## Test for errors
test_that("add_predictors - stars/multiple-attributes", {
  sa <- sdm_area(test_path("parana.gpkg"))
  pred <- read_stars(test_path("parana.tiff"))
  pred <- c(pred,pred)
  expect_error(add_predictors(sa,pred))
})

test_that("add_predictors - sdm_area is a stars", {
  pred <- read_stars(test_path("parana.tiff"))
  expect_error(add_predictors(pred,pred))
})

## Test the grid
test_that("add_predictors - bbox", {
  sa <- sdm_area(test_path("parana.gpkg"))
  pred <- read_stars(test_path("parana.tiff"))
  suppressWarnings(sa_pred <- add_predictors(sa, pred))
  expect_equal(st_bbox(sa_pred$grid), st_bbox(sa$grid))
})

test_that("add_predictors - epsg", {
  sa <- sdm_area(test_path("parana.gpkg"))
  pred <- read_stars(test_path("parana.tiff"))
  suppressWarnings(sa_pred <- add_predictors(sa, pred))
  expect_equal(st_crs(sa_pred$grid), st_crs(sa$grid))
})

test_that("add_predictors - cell_size", {
  sa <- sdm_area(test_path("parana.gpkg"))
  pred <- read_stars(test_path("parana.tiff"))
  suppressWarnings(sa_pred <- add_predictors(sa, pred))
  expect_equal(sa_pred$cell_size, sa$cell_size)
})

test_that("sdm_area - GEOMTYPE - sf", {
  sa <- sdm_area(test_path("parana.gpkg"))
  pred <- read_stars(test_path("parana.tiff"))
  suppressWarnings(sa_pred <- add_predictors(sa, pred))
  expect_equal(as.character(unique(st_geometry_type(sa_pred$grid))), "POLYGON")
})
