if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
  pr_tif <- here::here("tests", "testthat", "testdata", "parana.tiff") |>
    stars::read_stars(quiet = TRUE)
  pr_gpkg <- here::here("tests", "testthat", "testdata", "parana.gpkg") |>
    sf::st_read(quiet = TRUE)
  pr_shp <- here::here("tests", "testthat", "testdata", "parana.shp") |>
    sf::st_read(quiet = TRUE)
  amazon_shp <-  here::here("tests", "testthat", "testdata", "AmazonHydroRivers4.shp") |>
    sf::st_read(quiet = TRUE)
} else {
  pr_tif <- test_path("testdata", "parana.tiff") |>
    stars::read_stars(quiet = TRUE)
  pr_gpkg <- test_path("testdata","parana.gpkg") |>
    sf::st_read(quiet = TRUE)
  pr_shp <- test_path("testdata","parana.shp")|>
    sf::st_read(quiet = TRUE)
  amazon_shp <-  test_path("testdata", "AmazonHydroRivers4.shp") |>
    sf::st_read(quiet = TRUE)
}

## Test read
test_that("sdm_area - leitura stars", {
  expect_equal(round(pr_tif$parana.tiff[1, 1, 1], 4), 22.9386)
})

test_that("sdm_area - leitura sf", {
  expect_equal(as.numeric(pr_gpkg$GID0), 19)
})

## Test sf
test_that("sdm_area - sf/predictors", {
  pr_gpkg_tmp <- pr_gpkg |>
    dplyr::rename(cell_id = GID0)
  sa <- sdm_area(pr_gpkg_tmp, cell_size = 1, variables_selected = list("cell_id", "CODIGOIB1", "NOMEUF2"))
  expect_equal(predictors(sa), c("cell_id.1", "CODIGOIB1", "NOMEUF2"))
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
})


test_that("sdm_area - sf/predictors no variables selected", {
  sa <- sdm_area(pr_gpkg, cell_size = 1, variables_selected = list())
  expect_equal(predictors(sa), character(0))
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
})

test_that("sdm_area - sf/predictors no variables selected", {
  expect_snapshot(
    sa <- sdm_area(pr_gpkg, cell_size = 1, variables_selected = c("CODIGOIB1", "NOMEUF2", "foo"))
  )
  expect_equal(predictors(sa), c("CODIGOIB1", "NOMEUF2"))
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
})

## Test sf
test_that("sdm_area - sf/predictors lines instead of polygons", {
  sa <- sdm_area(amazon_shp, cell_size = 10000, crs = 6933)
  checkmate::expect_names(
    predictors(sa),
    permutation.of = c(
      "HYRIV_ID", "NEXT_DOWN", "MAIN_RIV", "LENGTH_KM", "DIST_DN_KM",
      "DIST_UP_KM", "CATCH_SKM", "UPLAND_SKM", "ENDORHEIC", "DIS_AV_CMS",
      "ORD_STRA", "ORD_CLAS", "ORD_FLOW", "HYBAS_L12"
  ))
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
})

test_that("sdm_area - sf/grid-bbox", {
  sa <- sdm_area(pr_gpkg, cell_size = 1)
  expect_equal(
    as.numeric(sf::st_bbox(sa$grid)),
    c(-54.82071, -27.14062, -47.82071, -22.14062),
    tolerance = 0.01
  )
})

test_that("sdm_area - sf/crs", {
  sa <- sdm_area(pr_gpkg, cell_size = 1)
  expect_equal(sf::st_crs(sa$grid), sf::st_crs(pr_gpkg))
})

test_that("sdm_area - stars_proxy", {
  sa <- sdm_area(pr_shp, cell_size = 10000)
  expect_equal(sf::st_crs(sa$grid), sf::st_crs(pr_shp))
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
})

test_that("sdm_area - sf/cellsize", {
  sa <- sdm_area(pr_gpkg, cell_size = 1)
  expect_equal(class(sa$cell_size), "numeric")
})

test_that("sdm_area - sf/grid", {
  sa <- sdm_area(pr_gpkg, cell_size = 1)
  expect_equal(class(sa$grid)[1], "sf")
})

test_that("sdm_area - sf/grid erro", {
  sa <- sdm_area(pr_gpkg, cell_size = 1)
  sa$grid <- sa$grid |> select(-cell_id)
  expect_error(
    .check_sdm_area(sa),
    "sdm_area object is corrupted!"
  )
})

test_that("sdm_area - sf/grid erro tamanho celula", {
  sa <- sdm_area(pr_gpkg, cell_size = 50000, crs = 6933)
  sa2 <- sdm_area(pr_gpkg, cell_size = 49000, crs = 6933)
  sa$grid <- sa2$grid
  expect_error(
    .check_sdm_area(sa),
    "sdm_area object is corrupted!"
  )
})


test_that("sdm_area - sf/no-epsg", {
  pr_gpkg_tmp <- pr_gpkg
  sf::st_crs(pr_gpkg_tmp) <- NA
  expect_error(sdm_area(pr_gpkg_tmp, cell_size = 1))
})

test_that("sdm_area - stars/epsg", {
  sa <- sdm_area(pr_gpkg, cell_size = 100000, crs = 6933)
  expect_true(sf::st_crs(sa$grid) == sf::st_crs(6933))
})

## Test stars
test_that("sdm_area - stars/predictors", {
  sa <- sdm_area(pr_tif, cell_size = 1)
  sa <- set_predictor_names(sa, c("wc2.1_10m_bio_1", "cell_id"))
  expect_equal(predictors(sa), c("wc2.1_10m_bio_1", "cell_id.2"))
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
})

test_that("sdm_area - stars/predictors chossing some vars", {
  pr_tif_tmp <- set_band_names(pr_tif, c("wc2.1_10m_bio_1", "wc2.1_10m_bio_2"))
  sa <- sdm_area(pr_tif_tmp, cell_size = 1, variables_selected = c("wc2.1_10m_bio_1"))
  expect_equal(predictors(sa), c("wc2.1_10m_bio_1"))
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
})

test_that("sdm_area - stars/predictors chossing some vars using list", {
  pr_tif_tmp <- set_band_names(pr_tif, c("wc2.1_10m_bio_1", "wc2.1_10m_bio_2"))
  sa <- sdm_area(pr_tif_tmp, cell_size = 1, variables_selected = list("wc2.1_10m_bio_1"))
  expect_equal(predictors(sa), c("wc2.1_10m_bio_1"))
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
})


test_that("sdm_area - stars/grid-bbox", {
  sa <- sdm_area(pr_tif, cell_size = 1)
  expect_equal(
    round(as.numeric(sf::st_bbox(sa$grid)), 4),
    c(-54.6667, -26.6667, -47.6667, -21.6667)
  )
})

test_that("sdm_area - stars/epsg", {
  sa <- sdm_area(pr_tif, cell_size = 1)
  expect_true(sf::st_crs(sa$grid) == sf::st_crs("wgs84"))
})

test_that("sdm_area - stars/cellsize", {
  sa <- sdm_area(pr_tif, cell_size = 1)
  expect_equal(class(sa$cell_size), "numeric")
})

test_that("sdm_area - stars/grid", {
  sa <- sdm_area(pr_tif, cell_size = 1)
  expect_equal(class(sa$grid)[1], "sf")
})

test_that("sdm_area - stars/epsg", {
  sa <- sdm_area(pr_tif, cell_size = 100000, crs = 6933)
  expect_true(sf::st_crs(sa$grid) == sf::st_crs(6933))
})

## Test alternative inputs
test_that("sdm_area - character/sf", {
  if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
    sa <- sdm_area(
      here::here("tests", "testthat", "testdata", "parana.gpkg"),
      cell_size = 0.3334
    )
  } else {
    sa <- sdm_area(
      test_path("testdata", "parana.gpkg"),
      cell_size = 0.3334
    )
  }

  expect_equal(class(sa$grid)[1], "sf")
})

test_that("sdm_area - character/stars", {
  if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
    sa <- sdm_area(
      here::here("tests", "testthat", "testdata", "parana.tiff"),
      cell_size = 0.3334
    )
  } else {
    sa <- sdm_area(test_path("testdata", "parana.tiff"), cell_size = 0.3334)
  }

  expect_equal(class(sa$grid)[1], "sf")
})

test_that("sdm_area - character/error", {
  expect_error(sdm_area(test_path("test.gpkg")))
})

test_that("sdm_area - stack/raster", {
  if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
    pr <- raster::stack(here::here("tests", "testthat", "testdata", "parana.tiff"))
    sa <- sdm_area(pr, cell_size = 0.3334)
  } else {
    pr <- raster::stack(test_path("testdata/parana.tiff"))
    sa <- sdm_area(pr, cell_size = 0.3334)
  }

  expect_equal(class(sa$grid)[1], "sf")
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
})

test_that("sdm_area - stack/terra", {
  if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
    pr <- terra::rast(here::here("tests", "testthat", "testdata", "parana.tiff"))
    sa <- sdm_area(pr, cell_size = 0.3334)
  } else {
    pr <- terra::rast(test_path("testdata/parana.tiff"))
    sa <- sdm_area(pr, cell_size = 0.3334)
  }

  expect_equal(class(sa$grid)[1], "sf")
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
})

test_that("sdm_area - print", {
  sa <- sdm_area(test_path("testdata/parana.gpkg"), cell_size = 0.3334)
  expect_snapshot(print(sa), error = FALSE)
})

## Test outputs
test_that("sdm_area - GEOMTYPE - sf", {
  if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
    sa <- sdm_area(
      here::here("tests", "testthat", "testdata", "parana.gpkg"),
      cell_size = 0.3334
    )
  } else {
    sa <- sdm_area(test_path("testdata", "parana.gpkg"), cell_size = 0.3334)
  }

  expect_equal(as.character(unique(sf::st_geometry_type(sa$grid))), "POLYGON")
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
})

test_that("sdm_area - GEOMTYPE - stars", {
  if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
    sa <- sdm_area(
      here::here("tests", "testthat", "testdata", "parana.tiff"),
      cell_size = 0.3334)
  } else {
    sa <- sdm_area(test_path("testdata", "parana.tiff"), cell_size = 0.3334)
  }
  expect_equal(as.character(unique(sf::st_geometry_type(sa$grid))), "POLYGON")
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
})


## Test .detect_sdm_area
test_that("sdm_area - sdm_area para ser detectado", {
  sa <- sdm_area(pr_gpkg, cell_size = 50000, crs = 6933)
  expect_snapshot(
    expect_equal(
      .detect_sdm_area(sa$grid, 50000, 6933),
      sa
    )
  )
})

test_that("sdm_area - gpkg para retornar NULL", {
  expect_equal(
      .detect_sdm_area(pr_gpkg, 50000, 6933),
      c(
        "Variable 'grid': Names must include the elements {'cell_id','geometry'}, but is missing elements {'cell_id','geometry'}",
        "Variable 'geometry': Must inherit from class 'sfc', but has class 'NULL'",
        "Variable 'cell_id': Must be of type 'numeric', not 'NULL'"
      )
  )
})

test_that("sdm_area - sdm_area para ser detectado com parametros diferentes", {
  sa <- sdm_area(pr_gpkg, cell_size = 50000, crs = 6933)
  expect_snapshot(
    expect_equal(
      .detect_sdm_area(sa$grid, 30000, 5388),
      sa
    )
  )
})

test_that("sdm_area - sf/predictors try detect lines instead of polygons", {
  expect_equal(
    .detect_sdm_area(amazon_shp |> rename(cell_id=HYRIV_ID), cell_size = 10000, crs = 6933),
    "x has other features than of polygons."
  )
})

## Test .detect_sdm_area
test_that("sdm_area - sdm_area para ser detectado", {
  sa <- sdm_area(pr_gpkg, cell_size = 50000, crs = 6933)
  sa2 <- sdm_area(sa$grid, cell_size = 50000, crs = 6933)
  expect_equal(sa2, sa)
})

## Test .detect_sdm_area
test_that("sdm_area - sdm_area para ser detectado com avisos", {
  sa <- sdm_area(pr_gpkg, cell_size = 50000, crs = 6933)
  expect_snapshot(
    expect_equal(
      sa2 <- sdm_area(sa$grid, cell_size = 40000, crs = 5388),
      sa
    )
  )
})
