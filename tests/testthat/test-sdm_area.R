## Test read
test_that("sdm_area - leitura stars", {
  bioc <- read_stars(test_path("parana.tiff"))
  expect_equal(round(bioc$parana.tiff[1, 1, 1], 4), 22.9386)
})

test_that("sdm_area - leitura sf", {
  pr <- st_read(test_path("parana.gpkg"))
  expect_equal(as.numeric(pr$GID0), 19)
})

## Test sf
test_that("sdm_area - sf/predictors", {
  pr <- st_read(test_path("parana.gpkg"))
  sa <- sdm_area(pr, cell_size = 1)
  expect_equal(sa$predictors, c("GID0", "CODIGOIB1", "NOMEUF2", "SIGLAUF3"))
})

test_that("sdm_area - sf/grid-bbox", {
  pr <- st_read(test_path("parana.gpkg"))
  sa <- sdm_area(pr, cell_size = 1)
  expect_equal(round(as.numeric(sa$bbox), 4), c(-54.6183, -26.7168, -47.6183, -21.7168))
})

test_that("sdm_area - sf/epsg", {
  pr <- st_read(test_path("parana.gpkg"))
  sa <- sdm_area(pr, cell_size = 1)
  expect_equal(sa$epsg, "WGS 84")
})

test_that("sdm_area - sf/cellsize", {
  pr <- st_read(test_path("parana.gpkg"))
  sa <- sdm_area(pr, cell_size = 1)
  expect_equal(class(sa$cell_size), "numeric")
})

test_that("sdm_area - sf/bbox", {
  pr <- st_read(test_path("parana.gpkg"))
  sa <- sdm_area(pr, cell_size = 1)
  expect_equal(class(sa$bbox), "bbox")
})

test_that("sdm_area - sf/grid", {
  pr <- st_read(test_path("parana.gpkg"))
  sa <- sdm_area(pr, cell_size = 1)
  expect_equal(class(sa$grid)[1], "sf")
})

test_that("sdm_area - sf/no-epsg", {
  pr <- st_read(test_path("parana.gpkg"))
  st_crs(pr) <- NA
  expect_error(sdm_area(pr, cell_size = 1))
})

test_that("sdm_area - stars/epsg", {
  pr <- st_read(test_path("parana.gpkg"))
  sa <- sdm_area(pr, cell_size = 100000, epsg = 6933)
  expect_equal(sa$epsg, "EPSG:6933")
})

## Test stars
test_that("sdm_area - stars/predictors", {
  pr <- read_stars(test_path("parana.tiff"))
  sa <- sdm_area(pr, cell_size = 1)
  expect_equal(sa$predictors, c("wc2.1_10m_bio_1", "wc2.1_10m_bio_12"))
})

test_that("sdm_area - stars/grid-bbox", {
  pr <- read_stars(test_path("parana.tiff"))
  sa <- sdm_area(pr, cell_size = 1)
  expect_equal(round(as.numeric(sa$bbox), 4), c(-54.6667, -26.6667, -47.6667, -21.6667))
})

test_that("sdm_area - stars/epsg", {
  pr <- read_stars(test_path("parana.tiff"))
  sa <- sdm_area(pr, cell_size = 1)
  expect_equal(sa$epsg, "WGS 84")
})

test_that("sdm_area - stars/cellsize", {
  pr <- read_stars(test_path("parana.tiff"))
  sa <- sdm_area(pr, cell_size = 1)
  expect_equal(class(sa$cell_size), "numeric")
})

test_that("sdm_area - stars/bbox", {
  pr <- read_stars(test_path("parana.tiff"))
  sa <- sdm_area(pr, cell_size = 1)
  expect_equal(class(sa$bbox), "bbox")
})

test_that("sdm_area - stars/grid", {
  pr <- read_stars(test_path("parana.tiff"))
  sa <- sdm_area(pr, cell_size = 1)
  expect_equal(class(sa$grid)[1], "sf")
})

test_that("sdm_area - stars/multiple-attributes", {
  pr <- read_stars(test_path("parana.tiff"))
  pr <- c(pr, pr)
  expect_error(sdm_area(pr, cell_size = 1))
})

test_that("sdm_area - stars/epsg", {
  pr <- read_stars(test_path("parana.tiff"))
  sa <- sdm_area(pr, cell_size = 100000, epsg = 6933)
  expect_equal(sa$epsg, "EPSG:6933")
})

## Test alternative inputs
test_that("sdm_area - character/sf", {
  sa <- sdm_area(test_path("parana.gpkg"))
  expect_equal(class(sa$grid)[1], "sf")
})

test_that("sdm_area - character/stars", {
  sa <- sdm_area(test_path("parana.tiff"))
  expect_equal(class(sa$grid)[1], "sf")
})

test_that("sdm_area - character/error", {
  expect_error(sdm_area(test_path("test.gpkg")))
})

test_that("sdm_area - stack/raster", {
  pr <- raster::stack(test_path("parana.tiff"))
  sa <- sdm_area(pr)
  expect_equal(class(sa$grid)[1], "sf")
})

test_that("sdm_area - stack/terra", {
  pr <- terra::rast(test_path("parana.tiff"))
  sa <- sdm_area(pr)
  expect_equal(class(sa$grid)[1], "sf")
})

test_that("sdm_area - print", {
  sa <- sdm_area(test_path("parana.gpkg"))
  expect_no_error(print(sa))
})

## Test outputs
test_that("sdm_area - GEOMTYPE - sf", {
  sa <- sdm_area(test_path("parana.gpkg"))
  expect_equal(as.character(unique(st_geometry_type(sa$grid))), "POLYGON")
})

test_that("sdm_area - GEOMTYPE - stars", {
  sa <- sdm_area(test_path("parana.tiff"))
  expect_equal(as.character(unique(st_geometry_type(sa$grid))), "POLYGON")
})
