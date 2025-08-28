
if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
  pr_tif <- here::here("tests", "testthat", "testdata", "parana.tiff") |>
    stars::read_stars(quiet = TRUE)
  pr_gpkg <- here::here("tests", "testthat", "testdata", "parana.gpkg") |>
    sf::st_read(quiet = TRUE)
} else {
  pr_tif <- test_path("testdata", "parana.tiff") |>
    stars::read_stars(quiet = TRUE)
  pr_gpkg <- test_path("testdata","parana.gpkg") |>
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
  sa <- sdm_area(pr_gpkg_tmp, cell_size = 2, variables_selected = list("cell_id", "CODIGOIB1", "NOMEUF2"))
  expect_equal(get_predictor_names(sa), c("cell_id.1", "CODIGOIB1", "NOMEUF2"))
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
  expect_equal(as.character(unique(sf::st_geometry_type(sa$grid))), "POLYGON")
  expect_no_error(grd <- get_sdm_area(sa))
  expect_true(all(c("cell_id", "geometry") %in% colnames(grd)))
  expect_no_error(grd <- get_sdm_area(input_sdm(sa)))
  expect_true(all(c("cell_id", "geometry") %in% colnames(grd)))
})

test_that("sdm_area - sf/predictors no variables selected", {
  sa <- sdm_area(pr_gpkg, cell_size = 2, variables_selected = list())
  expect_equal(get_predictor_names(sa), character(0))
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
  expect_equal(as.character(unique(sf::st_geometry_type(sa$grid))), "POLYGON")
  expect_no_error(grd <- get_sdm_area(sa))
  expect_true(all(c("cell_id", "geometry") %in% colnames(grd)))
})

test_that("sdm_area - sf/predictors no variables selected", {
  skip_on_cran()
  expect_snapshot(
    sa <- sdm_area(pr_gpkg, cell_size = 2, variables_selected = c("CODIGOIB1", "NOMEUF2", "foo"))
  )
})

test_that("sdm_area - sf/predictors no variables selected", {
  expect_warning(sa <- sdm_area(pr_gpkg, cell_size = 2, variables_selected = c("CODIGOIB1", "NOMEUF2", "foo")))
  expect_equal(get_predictor_names(sa), c("CODIGOIB1", "NOMEUF2"))
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
  expect_equal(as.character(unique(sf::st_geometry_type(sa$grid))), "POLYGON")
})

test_that("sdm_area - sf/predictors lines instead of polygons", {
  sa <- sdm_area(rivs, cell_size = 100000, crs = 6933)
  checkmate::expect_names(
    get_predictor_names(sa),
    permutation.of = c("LENGTH_KM", "DIST_DN_KM"))
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
  expect_equal(as.character(unique(sf::st_geometry_type(sa$grid))), "POLYGON")
})

#test_that("sdm_area - stars_proxy", {
#  sa <- sdm_area(pr_shp, cell_size = 100000)
#  expect_equal(sf::st_crs(sa$grid), sf::st_crs(pr_shp))
#  expect_true("cell_id" %in% colnames(sa$grid))
#  expect_true("geometry" %in% colnames(sa$grid))
#  expect_equal(as.character(unique(st_geometry_type(sa$grid))), "POLYGON")
#})

test_that("sdm_area - sf/grid-bbox", {
  sa <- sdm_area(pr_gpkg, cell_size = 2)
  expect_equal(
    as.numeric(sf::st_bbox(sa$grid)),
    c(-55.32, -27.64, -47.32, -21.64),
    tolerance = 0.01
  )
  expect_equal(sf::st_crs(sa$grid), sf::st_crs(pr_gpkg))
  expect_equal(sf::st_crs(sa$grid), sf::st_crs(pr_gpkg))
  expect_equal(class(sa$cell_size), "numeric")
  expect_equal(class(sa$grid)[1], "sf")
  expect_equal(as.character(unique(sf::st_geometry_type(sa$grid))), "POLYGON")
  sa$grid <- sa$grid |> dplyr::select(-cell_id)
  expect_error(
    caretSDM:::.check_sdm_area(sa),
    "sdm_area object is corrupted!"
  )
})

test_that("sdm_area - sf/grid erro tamanho celula", {
  sa <- sdm_area(pr_gpkg, cell_size = 100000, crs = 6933)
  sa2 <- sdm_area(pr_gpkg, cell_size = 99000, crs = 6933)
  sa$grid <- sa2$grid
  expect_error(
    caretSDM:::.check_sdm_area(sa),
    "sdm_area object is corrupted!"
  )
})

test_that("sdm_area - sf/no-epsg", {
  pr_gpkg_tmp <- pr_gpkg
  sf::st_crs(pr_gpkg_tmp) <- NA
  expect_error(sdm_area(pr_gpkg_tmp, cell_size = 2))
})

test_that("sdm_area - stars/epsg", {
  sa <- sdm_area(pr_gpkg, cell_size = 100000, crs = 6933)
  expect_true(sf::st_crs(sa$grid) == sf::st_crs(6933))
  expect_equal(as.character(unique(sf::st_geometry_type(sa$grid))), "POLYGON")
})

## Test stars
test_that("sdm_area - stars/predictors - wrong names", {
  sa <- sdm_area(pr_tif, cell_size = 2)
  expect_error(set_predictor_names(sa, c("wc2.1_10m_bio_1", "cell_id")))
  expect_error(set_predictor_names(sa, c("wc2.1_10m_bio_1", "geometry")))
})

test_that("sdm_area - stars/predictors choosing some vars", {
  sa <- sdm_area(pr_tif, cell_size = 2, variables_selected = c("wc2.1_10m_bio_1", "wc2.1_10m_bio_12"))
  sa <- set_predictor_names(sa, c("bio1", "bio12"))
  expect_equal(get_predictor_names(sa), c("bio1", "bio12"))
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
  expect_equal(as.character(unique(sf::st_geometry_type(sa$grid))), "POLYGON")
})

test_that("sdm_area - stars/predictors chossing some vars using list", {
  sa <- sdm_area(pr_tif, cell_size = 2, variables_selected = list("wc2.1_10m_bio_1"))
  expect_equal(get_predictor_names(sa), c("wc2.1_10m_bio_1"))
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
  expect_equal(as.character(unique(sf::st_geometry_type(sa$grid))), "POLYGON")
})

test_that("sdm_area - stars/grid-bbox", {
  sa <- sdm_area(parana, cell_size = 2)
  expect_equal(
    round(as.numeric(sf::st_bbox(sa$grid)), 3),
    c(-55.321, -27.641, -47.321, -21.641),
    tolerance = 0.01
  )
  expect_true(sf::st_crs(sa$grid) == sf::st_crs("wgs84"))
  expect_equal(class(sa$cell_size), "numeric")
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
      cell_size = 2
    )
  } else {
    sa <- sdm_area(
      test_path("testdata", "parana.gpkg"),
      cell_size = 2
    )
  }

  expect_equal(class(sa$grid)[1], "sf")
})

test_that("sdm_area - character/stars", {
  if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
    sa <- sdm_area(
      here::here("tests", "testthat", "testdata", "parana.tiff"),
      cell_size = 2
    )
  } else {
    sa <- sdm_area(test_path("testdata", "parana.tiff"), cell_size = 2)
  }

  expect_equal(class(sa$grid)[1], "sf")
  expect_equal(as.character(unique(sf::st_geometry_type(sa$grid))), "POLYGON")
})

test_that("sdm_area - character/error", {
  expect_error(sdm_area(test_path("test.gpkg")))
})

test_that("sdm_area - stack/raster", {
  if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
    pr <- raster::stack(here::here("tests", "testthat", "testdata", "parana.tiff"))
    sa <- sdm_area(pr, cell_size = 2)
  } else {
    pr <- raster::stack(test_path("testdata/parana.tiff"))
    sa <- sdm_area(pr, cell_size = 2)
  }

  expect_equal(class(sa$grid)[1], "sf")
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
  expect_equal(as.character(unique(sf::st_geometry_type(sa$grid))), "POLYGON")
})

test_that("sdm_area - stack/terra", {
  if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
    pr <- terra::rast(here::here("tests", "testthat", "testdata", "parana.tiff"))
    sa <- sdm_area(pr, cell_size = 2)
  } else {
    pr <- terra::rast(test_path("testdata/parana.tiff"))
    sa <- sdm_area(pr, cell_size = 2)
  }
  expect_equal(class(sa$grid)[1], "sf")
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
  expect_equal(as.character(unique(sf::st_geometry_type(sa$grid))), "POLYGON")
})

test_that("sdm_area - print", {
  skip_on_cran()
  sa <- sdm_area(test_path("testdata/parana.gpkg"), cell_size = 2)
  expect_snapshot(print(sa), error = FALSE)
})

test_that("sdm_area - stars' crs = NA", {
  skip_on_cran()
  pr_tif2 <- pr_tif
  sf::st_crs(pr_tif2) <- NA
  expect_error(sdm_area(pr_tif2, cell_size = 50000, crs=6933, gdal=F))
})

test_that("sdm_area - crs=NA", {
  skip_on_cran()
  expect_error(sdm_area(pr_tif, cell_size = 50000, crs=NA, gdal=F))
})

## Test outputs
test_that("sdm_area - GEOMTYPE - sf", {
  if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
    sa <- sdm_area(
      here::here("tests", "testthat", "testdata", "parana.gpkg"),
      cell_size = 2
    )
  } else {
    sa <- sdm_area(test_path("testdata", "parana.gpkg"), cell_size = 2)
  }

  expect_equal(as.character(unique(sf::st_geometry_type(sa$grid))), "POLYGON")
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
  expect_equal(as.character(unique(sf::st_geometry_type(sa$grid))), "POLYGON")
})

test_that("sdm_area - GEOMTYPE - stars", {
  if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
    sa <- sdm_area(
      here::here("tests", "testthat", "testdata", "parana.tiff"),
      cell_size = 2)
  } else {
    sa <- sdm_area(test_path("testdata", "parana.tiff"), cell_size = 2)
  }
  expect_equal(as.character(unique(sf::st_geometry_type(sa$grid))), "POLYGON")
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
  expect_equal(as.character(unique(sf::st_geometry_type(sa$grid))), "POLYGON")
})


## Test .detect_sdm_area
test_that("sdm_area - sdm_area para ser detectado", {
  skip_on_cran()
  sa <- sdm_area(pr_gpkg, cell_size = 100000, crs = 6933, gdal = TRUE, lines_as_sdm_area = FALSE)
  expect_snapshot(
    expect_equal(
      caretSDM:::.detect_sdm_area(sa$grid, 100000, 6933, gdal = TRUE, lines_as_sdm_area = FALSE),
      sa
    )
  )
})

test_that("sdm_area - gpkg para retornar NULL", {
  expect_equal(
    caretSDM:::.detect_sdm_area(pr_gpkg, 100000, 6933, gdal = TRUE, lines_as_sdm_area = FALSE),
    c(
      "Variable 'grid': Names must include the elements {'cell_id','geometry'}, but is missing elements {'cell_id','geometry'}",
      "Variable 'geometry': Must inherit from class 'sfc', but has class 'NULL'",
      "Variable 'cell_id': Must be of type 'numeric', not 'NULL'"
    )
  )
})

test_that("sdm_area - sdm_area para ser detectado com parametros diferentes", {
  skip_on_cran()
  sa <- sdm_area(pr_gpkg, cell_size = 100000, crs = 6933, gdal = TRUE, lines_as_sdm_area = FALSE)
  expect_snapshot(
    expect_equal(
      caretSDM:::.detect_sdm_area(sa$grid, 90000, 5839, gdal = TRUE, lines_as_sdm_area = FALSE),
      sa
    )
  )
})

test_that("sdm_area - sf/predictors try detect lines instead of polygons", {
  expect_equal(
    caretSDM:::.detect_sdm_area(rivs |> dplyr::mutate(cell_id=rep(1:nrow(rivs))),
                     cell_size = 100000,
                     crs = 6933,
                     gdal = TRUE,
                     lines_as_sdm_area = FALSE),
    "x has other features than of polygons."
  )
})

## Test .detect_sdm_area
test_that("sdm_area - sdm_area para ser detectado", {
  sa <- sdm_area(pr_gpkg, cell_size = 100000, crs = 6933)
  sa2 <- sdm_area(sa$grid, cell_size = 100000, crs = 6933)
  expect_equal(sa2, sa)
})

## Test .detect_sdm_area
test_that("sdm_area - sdm_area para ser detectado com avisos", {
  skip_on_cran()
  sa <- sdm_area(pr_gpkg, cell_size = 100000, crs = 6933)
  expect_snapshot(
    expect_equal(
      sa2 <- sdm_area(sa$grid, cell_size = 90000, crs = 5839),
      sa
    )
  )
})

# test crop!=NULL
test_that("sdm_area - crop_by tem crs diferente", {
  expect_error(sdm_area(bioc, cell_size = 100000, crs = 6933, crop_by = pr_gpkg))
})

test_that("sdm_area - crop_by tem crs igual", {
  pr <- sf::st_transform(pr_gpkg, crs=6933)
  sa <- sdm_area(bioc, cell_size = 100000, crs = 6933, crop_by = pr)
  expect_equal(sf::st_crs(pr)[2], sf::st_crs(sa$grid)[2])
})

test_that("sdm_area - crop_by tem crs diferente de bioc e crs=NULL", {
  pr <- sf::st_transform(pr_gpkg, crs=6933)
  expect_error(sdm_area(bioc, cell_size = 100000, crs = NULL, crop_by = pr))
})

# print
test_that("sdm_area - print", {
  skip_on_cran()
  sa <- sdm_area(bioc, cell_size = 100000, crs = 6933)
  expect_snapshot(sa)
})

# test gdal=F
test_that("sdm_area - sf+gdal=F", {
  skip_on_cran()
  sa <- sdm_area(pr_gpkg, cell_size = 100000, crs=6933, gdal=F)
  expect_snapshot(sa)
})

test_that("sdm_area - sf+gdal=F areas do not intersect", {
  skip_on_cran()
  box <- sf::st_bbox(c(xmin = 16.1, xmax = 16.6, ymax = 48.6, ymin = 47.9), crs = sf::st_crs(4326))
  box <- sf::st_transform(box, crs=6933)
  expect_warning(sdm_area(parana, cell_size = 100000, crs=6933, crop_by = box, gdal=F))
})

test_that("sdm_area - sf+gdal=F numeric col", {
  skip_on_cran()
  pr_gpkg2 <- pr_gpkg
  class(pr_gpkg2$CODIGOIB1) <- "numeric"
  sa <- sdm_area(pr_gpkg2, cell_size = 100000, crs=6933, gdal=F)
  expect_true(is.numeric(sa$grid$CODIGOIB1))
})

test_that("sdm_area - stars+gdal=F", {
  skip_on_cran()
  sa <- sdm_area(pr_tif, cell_size = 100000, crs=6933, gdal=F)
  expect_snapshot(sa)
  expect_no_error(grd <- get_sdm_area(sa))
  expect_true(all(c("cell_id", "geometry") %in% colnames(grd)))
  expect_no_error(grd <- get_sdm_area(input_sdm(sa)))
  expect_true(all(c("cell_id", "geometry") %in% colnames(grd)))
})

test_that("sdm_area - stars+gdal=F areas do not intersect", {
  skip_on_cran()
  box <- sf::st_bbox(c(xmin = 16.1, xmax = 16.6, ymax = 48.6, ymin = 47.9), crs = sf::st_crs(4326))
  box <- sf::st_transform(box, crs=6933)
  expect_warning(sdm_area(pr_tif, cell_size = 100000, crs=6933, crop_by = box, gdal=F))
})

# test lines
test_that("sdm_area - lines", {
  sa <- sdm_area(rivs, cell_size = 5, lines_as_sdm_area = TRUE)
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
  expect_equal(class(sa$cell_size), "numeric")
  expect_equal(sf::st_crs(sa$grid), sf::st_crs(rivs))
  expect_equal(class(sa$grid)[1], "sf")
  expect_equal(as.character(unique(sf::st_geometry_type(sa$grid))), "LINESTRING")
})

# test cell_size=NULL
test_that("sdm_area - cell_size=NULL", {
  sa <- sdm_area(bioc, cell_size = NULL)
  expect_true("cell_id" %in% colnames(sa$grid))
  expect_true("geometry" %in% colnames(sa$grid))
  expect_equal(class(sa$cell_size), "numeric")
  expect_equal(round(sa$cell_size, 3), round(as.numeric(stars::st_res(bioc)[1]), 3))
  #expect_equal(sf::st_crs(sa$grid), sf::st_crs(bioc))
  expect_equal(class(sa$grid)[1], "sf")
  expect_equal(as.character(unique(sf::st_geometry_type(sa$grid))), "POLYGON")
})

test_that("add_sdm_area", {
  sa1 <- sdm_area(bioc, cell_size = 1)
  sa2 <- sdm_area(scen_rs, cell_size = 1)
  expect_error(add_sdm_area(sa1, sa2))
  sa2 <- sdm_area(bioc, cell_size = 100000, crs = 6933)
  expect_error(add_sdm_area(sa1, sa2))
  sa2 <- sdm_area(scen_rs, cell_size = 1) |>
    select_predictors(c("current.bio1", "current.bio4", "current.bio12")) |>
    set_predictor_names(get_predictor_names(sa1))
  expect_no_error(sa <- add_sdm_area(sa1, sa2))
  expect_no_error(add_sdm_area(sa1, sa2 = NULL))
  expect_no_error(add_sdm_area(sa1 = NULL, sa2))
  expect_true(all(c(sf::st_bbox(sa2$grid)[1:2], sf::st_bbox(sa1$grid)[3:4]) == sf::st_bbox(sa$grid)))
})
