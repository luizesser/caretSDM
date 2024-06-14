test_that("predictors misc - predictors_names", {
  sa <- sdm_area(test_path("parana.gpkg"))
  pred <- read_stars(test_path("parana.tiff"))
  suppressWarnings(sa_pred <- add_predictors(sa, pred))
  expect_equal(
    predictors_names(sa_pred),
    c("wc2.1_10m_bio_1", "wc2.1_10m_bio_12", "GID0", "CODIGOIB1", "NOMEUF2", "SIGLAUF3")
  )
})

test_that("predictors misc - set_predictors_names", {
  sa <- sdm_area(test_path("parana.gpkg"))
  pred <- read_stars(test_path("parana.tiff"))
  suppressWarnings(sa_pred <- add_predictors(sa, pred))
  expect_equal(get_predictors(sa_pred), sa_pred$grid)
})
