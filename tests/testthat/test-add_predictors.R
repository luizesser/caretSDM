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
  pr_file <- here::here("tests", "testthat", "testdata", "parana.tiff")
} else {
  pr_stars <- test_path("testdata", "parana.tiff") |>
    stars::read_stars(quiet = TRUE)
  pr_raster <- test_path("testdata", "parana.tiff") |>
    raster::stack()
  pr_gpkg <- test_path("testdata", "parana.gpkg") |>
    sf::st_read(quiet = TRUE)
  pr_shp <- test_path("testdata", "parana.shp") |>
    sf::st_read(quiet = TRUE)
  pr_file <- test_path("testdata", "parana.tiff")
}

sa <- sdm_area(pr_gpkg, cell_size = 10000, crs = 6933)

# Parana
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

test_that("add_predictors - lista de variáveis invalida", {
  expect_error(
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

test_that("add_predictors, but there is no overlap", {
  expect_error(add_predictors(sa, amazon_shp))
})

test_that("get_predictors - sdm_area", {
  sa_pred <- add_predictors(sa, pr_raster)
  expect_equal(get_predictors(sa_pred), sa_pred$grid)
})

test_that("get_predictors - input_sdm", {
  sa_pred <- add_predictors(sa, pr_raster)
  expect_equal(get_predictors(input_sdm(sa_pred)), sa_pred$grid)
})

test_that("add_predictors - character input", {
  sa_pred <- add_predictors(sa, pr_file)
  sa_pred2 <- add_predictors(sa, pr_raster)
  expect_equal(sa_pred$grid, sa_pred2$grid)
})

# Rivs
sa_rivs <- sdm_area(rivs, cell_size = 25000, crs = 6933, lines_as_sdm_area = TRUE)
test_that("add_predictors - rasterStack", {
  sa_pred <- add_predictors(sa_rivs, pr_raster)
  expect_equal(
    get_predictor_names(sa_pred),
    c("LENGTH_KM", "DIST_DN_KM", "wc2.1_10m_bio_1", "wc2.1_10m_bio_12")
  )
  expect_true(
    sa_pred$grid |> nrow() >=  sa_rivs$grid |> nrow()
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
  sa_pred <- add_predictors(sa_rivs, pr_raster, c("wc2.1_10m_bio_1"))
  expect_equal(
    get_predictor_names(sa_pred),
    c("LENGTH_KM", "DIST_DN_KM", "wc2.1_10m_bio_1")
  )
})

test_that("add_predictors - stars", {
  sa_pred <- add_predictors(sa_rivs, pr_stars)
  expect_equal(
    get_predictor_names(sa_pred),
    c("LENGTH_KM", "DIST_DN_KM", "wc2.1_10m_bio_1", "wc2.1_10m_bio_12")
  )
})

test_that("add_predictors - sf", {
  sa_pred <- add_predictors(sa_rivs, pr_gpkg)
  expect_equal(
    get_predictor_names(sa_pred),
    c("LENGTH_KM", "DIST_DN_KM", "GID0", "CODIGOIB1", "NOMEUF2", "SIGLAUF3")
  )
})

test_that("add_predictors - lista de variáveis invalida", {
  expect_error(
    sa_pred <- add_predictors(sa_rivs, pr_raster, list("foo"))
  )
})

test_that("add_predictors - stack/terra", {
  if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
    pr <-
      terra::rast(here::here("tests", "testthat", "testdata", "parana.tiff"))
  } else {
    pr <- terra::rast(test_path("testdata/parana.tiff"))
  }
  sa_pred <- add_predictors(sa_rivs, pr)
  expect_equal(
    get_predictor_names(sa_pred),
    c("LENGTH_KM", "DIST_DN_KM", "wc2.1_10m_bio_1", "wc2.1_10m_bio_12")
  )
})

test_that("add_predictors, but there is no overlap", {
  expect_error(add_predictors(sa_rivs, amazon_shp))
})

test_that("get_predictors - sdm_area", {
  sa_pred <- add_predictors(sa_rivs, pr_raster)
  expect_equal(get_predictors(sa_pred), sa_pred$grid)
})

test_that("get_predictors - input_sdm", {
  sa_pred <- add_predictors(sa_rivs, pr_raster)
  expect_equal(get_predictors(input_sdm(sa_pred)), sa_pred$grid)
})

test_that("add_predictors - character input", {
  sa_pred <- add_predictors(sa_rivs, pr_file)
  sa_pred2 <- add_predictors(sa_rivs, pr_raster)
  expect_equal(sa_pred$grid, sa_pred2$grid)
})

# Others

test_that("add_predictors - rasterStack não usando sdm_area", {
  expect_snapshot(
    sa_pred <- add_predictors(pr_raster, pr_raster),
    error = TRUE
  )
})

test_that("add_predictors - correção do tidyr::drop_na: drop_na modifica o bbox buff+gdal", {
 buf_sa <- occ |>
   sf::st_as_sf(coords = c(2,3)) |>
   sf::st_buffer(dist = 10000) |>
   sf::st_union() |>
   sf::st_as_sf(crs=sf::st_crs(6933))
 sa_buf <- sdm_area(buf_sa, cell_size = 5000, crs = 6933)
 sa_pred <- add_predictors(sa_buf, bioc)
 suppressWarnings(bbox_intersect <- sf::st_bbox(sf::st_intersection(sa_buf$grid, sa_pred$grid))) #find intersection between sdm_area and add_pred.
 expect_equal(bbox_intersect, sf::st_bbox(sa_pred$grid))
})

test_that("add_predictors - correção do tidyr::drop_na: drop_na modifica o bbox Gpkg+gdal", {
 sa_buf <- sdm_area(pr_gpkg, cell_size = 5000, crs = 6933)
 sa_pred <- add_predictors(sa_buf, bioc)
 suppressWarnings(bbox_intersect <- sf::st_bbox(sf::st_intersection(sa_buf$grid, sa_pred$grid))) #find intersection between sdm_area and add_pred.
 expect_equal(bbox_intersect, sf::st_bbox(sa_pred$grid))
})

test_that("add_predictors - correção do tidyr::drop_na: drop_na modifica o bbox buff-nogdal", {
 buf_sa <- occ |>
   sf::st_as_sf(coords = c(2,3)) |>
   sf::st_buffer(dist = 10000) |>
   sf::st_union() |>
   sf::st_as_sf(crs=sf::st_crs(6933))
 sa_buf <- sdm_area(buf_sa, cell_size = 5000, crs = 6933, gdal = FALSE)
 sa_pred <- add_predictors(sa_buf, bioc, gdal = FALSE)
 suppressWarnings(bbox_intersect <- sf::st_bbox(sf::st_intersection(sa_buf$grid, sa_pred$grid))) #find intersection between sdm_area and add_pred.
 expect_equal(bbox_intersect, sf::st_bbox(sa_pred$grid))
})

test_that("add_predictors - correção do tidyr::drop_na: drop_na modifica o bbox Gpkg-nogdal", {
 sa_buf <- sdm_area(pr_gpkg, cell_size = 5000, crs = 6933, gdal = FALSE)
 sa_pred <- add_predictors(sa_buf, bioc, gdal = FALSE)
 suppressWarnings(bbox_intersect <- sf::st_bbox(sf::st_intersection(sa_buf$grid, sa_pred$grid))) #find intersection between sdm_area and add_pred.
 expect_equal(bbox_intersect, sf::st_bbox(sa_pred$grid))
})

