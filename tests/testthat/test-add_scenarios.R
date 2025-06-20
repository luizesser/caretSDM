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
sa <- sdm_area(pr_gpkg, cell_size = 100000, crs = 6933)
sa <- add_predictors(sa, pr_raster)
sa <- dplyr::select(sa, c("wc2.1_10m_bio_1","wc2.1_10m_bio_12"))
sa <- set_predictor_names(sa, c("bio1", "bio12"))
scen <- scen[,,,c("bio1", "bio12")]

test_that("add_scenarios - stars", {
  sa_pred <- add_scenarios(sa, scen)
  expect_equal(
    get_predictor_names(sa_pred),
    c("bio1", "bio12")
  )
  expect_equal(
    get_predictor_names(sa_pred),
    get_predictor_names(sa_pred$scenarios)
  )
  expect_equal(
    names(sa_pred$scenarios$data$ca_ssp245_2090),
    c("cell_id", "bio1", "bio12", "geometry")
  )
})

test_that("add_scenarios - stars selecionando uma variável", {
  sa_pred <- add_scenarios(sa, scen, variables_selected=c("bio1"))
  expect_equal(
    get_predictor_names(sa_pred),
    c("bio1")
  )
  expect_equal(
    get_predictor_names(sa_pred),
    get_predictor_names(sa_pred$scenarios)
  )
  expect_equal(
    names(sa_pred$scenarios$data$ca_ssp245_2090),
    c("cell_id", "bio1", "geometry")
  )
})

test_that("add_scenarios - NULL", {
  sa_pred <- add_scenarios(sa, variables_selected=c("bio1"))
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
    c("bio1", "bio12")
  )
})

test_that("add_scenarios - RasterStack", {
  #suppressWarnings(test <- as(na.omit(scen), "Raster"))
  suppressWarnings(test <- stars:::st_as_raster(scen, "SpatRaster") |> raster::stack())
  test <- raster::stack(test)
  sa_pred <- add_scenarios(sa, test)
  expect_equal(
    sa_pred$scenarios$data$current,
    sa_pred$grid
  )
  expect_equal(
    get_predictor_names(sa_pred),
    c("bio1", "bio12")
  )
  expect_equal(
    names(sa_pred$scenarios$data[[1]]),
    c("cell_id", "bio1", "bio12", "geometry")
  )
})

test_that("add_scenarios - stars mas a variável é inválida", {
  expect_error(add_scenarios(sa, scen, variables_selected=c("bio1", "bio12", "bio13")))
})

test_that("add_scenarios - stars mas nem todas variaveis estão presente", {
  expect_error(add_scenarios(sa, scen, variables_selected=c("bio7")))
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
  expect_true(all(get_predictor_names(sa) %in% names(sa_scen$scenarios$data$ca_ssp245_2090)))
})

test_that("add_scenarios - stationary data", {
  sa <- sdm_area(pr_gpkg, cell_size = 20000, crs = 6933)
  names(pr_raster) <- c("bio1","bio12")
  sa <- add_predictors(sa, pr_raster)
  sa_pred <- add_scenarios(sa, pr_raster, stationary = c("GID0", "CODIGOIB1",
                                                              "NOMEUF2", "SIGLAUF3"))
  expect_equal(
    get_predictor_names(sa_pred),
    c("GID0", "CODIGOIB1", "NOMEUF2", "SIGLAUF3", "bio1","bio12")
  )
  expect_equal(
    get_predictor_names(sa_pred),
    get_predictor_names(sa_pred$scenarios)
  )
  expect_equal(
    names(sa_pred$scenarios$data$bio1),
    c("cell_id", "GID0", "CODIGOIB1", "NOMEUF2", "SIGLAUF3",
      "bio1","bio12", "geometry")
  )
  expect_equal(length(sa_pred$scenarios$data), 2)

  sa_pred <- add_scenarios(sa_pred, scen, stationary = c("GID0", "CODIGOIB1",
                                                              "NOMEUF2", "SIGLAUF3"))
  expect_equal(length(sa_pred$scenarios$data), 6)

})

test_that("add_scenarios - stationary data/input_sdm", {
  sa <- sdm_area(pr_gpkg, cell_size = 20000, crs = 6933)
  names(pr_raster) <- c("bio1","bio12")
  sa <- add_predictors(sa, pr_raster)
  sa <- add_scenarios(sa)
  i <- input_sdm(sa)
  i_pred <- add_scenarios(i, pr_raster, stationary = c("GID0", "CODIGOIB1",
                                                         "NOMEUF2", "SIGLAUF3"))
  expect_equal(
    get_predictor_names(i_pred),
    c("GID0", "CODIGOIB1", "NOMEUF2", "SIGLAUF3", "bio1","bio12")
  )
  expect_equal(
    get_predictor_names(i_pred),
    get_predictor_names(i_pred$scenarios)
  )
  expect_equal(
    names(i_pred$scenarios$data$bio1),
    c("cell_id", "GID0", "CODIGOIB1", "NOMEUF2", "SIGLAUF3",
      "bio1","bio12", "geometry")
  )
  expect_equal(length(i_pred$scenarios$data), 2)
  expect_error(add_scenarios(i_pred, scen, stationary = c("GID0", "CODIGOIB1",
                                                          "NOMEUF2")))
  i_pred <- add_scenarios(i_pred, scen, stationary = c("GID0", "CODIGOIB1",
                                                         "NOMEUF2", "SIGLAUF3"))
  expect_equal(length(i_pred$scenarios$data), 6)

})

test_that("select_scenarios", {
  sa_pred <- add_scenarios(sa, scen)
  expect_equal(
    scenarios_names(sa_pred),
    c("ca_ssp245_2090", "ca_ssp585_2090", "mi_ssp245_2090", "mi_ssp585_2090", "current")
  )
  expect_error(select_scenarios(sa_pred))
  expect_equal(
    scenarios_names(select_scenarios(sa_pred, c("ca_ssp245_2090", "ca_ssp585_2090"))),
    c("ca_ssp245_2090", "ca_ssp585_2090")
  )
  expect_error(select_scenarios(sa_pred, c("a")))

  i_pred <- input_sdm(sa_pred)
  expect_equal(
    scenarios_names(i_pred),
    c("ca_ssp245_2090", "ca_ssp585_2090", "mi_ssp245_2090", "mi_ssp585_2090", "current")
  )
  expect_error(select_scenarios(i_pred))
  expect_equal(
    scenarios_names(select_scenarios(i_pred, c("ca_ssp245_2090", "ca_ssp585_2090"))),
    c("ca_ssp245_2090", "ca_ssp585_2090")
  )
  expect_error(select_scenarios(i_pred, c("a")))
  expect_error(select_scenarios(i_pred, c("a")))

})

test_that("set_scenarios_names", {
  sa_pred <- add_scenarios(sa, scen)
  expect_error(set_scenarios_names(sa_pred, c(1,2,3,4,5)))
  expect_no_error(set_scenarios_names(sa_pred, as.character(1:5)))
  expect_error(set_scenarios_names(sa_pred, as.character(1:4)))
  expect_equal(
    scenarios_names(set_scenarios_names(sa_pred, c("a", "b", "c", "d", "e"))),
    c("a", "b", "c", "d", "e")
  )

  i_pred <- input_sdm(sa_pred)
  expect_error(set_scenarios_names(i_pred, c(1,2,3,4,5)))
  expect_no_error(set_scenarios_names(i_pred, as.character(1:5)))
  expect_error(set_scenarios_names(i_pred, as.character(1:4)))
  expect_equal(
    scenarios_names(set_scenarios_names(i_pred, c("a", "b", "c", "d", "e"))),
    c("a", "b", "c", "d", "e")
  )
})

